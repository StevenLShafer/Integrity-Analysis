# parseBaselineTable.R - the hybrid entry point: heuristics first, AI second.
#
############################################################################
# Provenance                                                               #
# Written 2026-08-15 by Claude Code (model: Claude Opus 5, Anthropic) at   #
# Steve Shafer's request. New code.                                        #
#                                                                          #
# Policy encoded here: the deterministic engine ALWAYS runs first, and its #
# rows always win. The AI engine is consulted only for what the            #
# deterministic pass could not read, and never overwrites a value that was #
# located on the page by coordinate.                                       #
# Status: run and verified by tests/testthat/test-hybrid.R (deterministic  #
# paths and the merge rule; the live API call is not exercised there).     #
############################################################################

#' What in a parsed table needs a human look
#'
#' Returns the reasons a parsed table should not be trusted as-is. An empty
#' character vector means the deterministic engine read the whole table
#' cleanly. [parseBaselineTable()] uses this to decide whether to consult the
#' AI fallback.
#'
#' @param x A `ParsePDFTable` object.
#' @return A character vector of human-readable reasons, possibly empty.
#' @export
reviewFlags <- function(x) {
  stopifnot(inherits(x, "ParsePDFTable"))
  flags <- character(0)
  if (nrow(x$data) == 0)
    flags <- c(flags, "no rows were parsed at all")
  if (nrow(x$skipped) > 0)
    flags <- c(flags, paste0(nrow(x$skipped),
                             " table line(s) could not be used: ",
                             paste(unique(x$skipped$label), collapse = ", ")))
  if (nrow(x$arms) < 2)
    flags <- c(flags, paste0("only ", nrow(x$arms),
                             " treatment arm(s) were found"))
  if (any(is.na(x$arms$N)))
    flags <- c(flags, "arm N is missing for at least one arm")
  # Recovered arm sizes are usable but not the same thing as an N printed in
  # the table header: say where each one came from, so a human can verify it
  # - a text-recovered N against the CONSORT flow diagram in particular
  # (2026-08-21, armNRecovery.R).
  if (!is.null(x$armNSource) && any(!is.na(x$armNSource))) {
    src      <- x$armNSource[!is.na(x$armNSource)]
    fromText <- grepl("^document text", src)
    if (any(!fromText))
      flags <- c(flags, paste0(sum(!fromText), " arm size(s) derived from ",
                               "the table's own printed n (%) cells"))
    if (any(fromText))
      flags <- c(flags, paste0(sum(fromText), " arm size(s) recovered from ",
                               "the document text - verify against the ",
                               "CONSORT flow diagram: ",
                               paste(src[fromText], collapse = " | ")))
  }
  # Counts converted from printed percentages are usable but derived: the
  # percentage and the arm N pinned a unique integer, but no count is
  # printed on the page. Say which rows, so a human can check them
  # (2026-08-21, the percent-block conversion).
  if (!is.null(x$derivedCounts) && length(x$derivedCounts) > 0)
    flags <- c(flags, paste0(length(x$derivedCounts), " category row(s) ",
                             "converted from printed percentages via the ",
                             "arm N (unique-count bracket): ",
                             paste(x$derivedCounts, collapse = ", ")))
  # APPROXIMATE conversions (pctApprox = TRUE) are a step further from the
  # page than the unique bracket: round(N x pct / 100) can be off by up to
  # half a printed unit of N/100. Usable, but check before analyzing.
  if (!is.null(x$approxCounts) && length(x$approxCounts) > 0)
    flags <- c(flags, paste0(length(x$approxCounts), " category row(s) use ",
                             "APPROXIMATE counts - round(arm N x percent) ",
                             "where the printed rounding could not pin a ",
                             "unique integer: ",
                             paste(x$approxCounts, collapse = ", "),
                             ". Check these against the paper before ",
                             "analyzing."))
  disp <- if ("SE" %in% names(x$data))
    !is.na(x$data$SD) | !is.na(x$data$SE) else !is.na(x$data$SD)
  cont <- !is.na(x$data$MEAN) | disp
  if (any(cont & (is.na(x$data$MEAN) | !disp)))
    flags <- c(flags,
               "a continuous row is missing its mean or its SD/SE")
  # A standard error is not interchangeable with a standard deviation, and the
  # conversion needs N. Say so rather than letting it pass silently.
  if ("SE" %in% names(x$data) && any(!is.na(x$data$SE)))
    flags <- c(flags, paste(sum(!is.na(x$data$SE)),
                            "row(s) report a standard error, not an SD -",
                            "converting needs N and is the analysis's decision"))
  if (!is.null(x$dispersion) && grepl("assumed", x$dispersion))
    flags <- c(flags, paste0("the table does not say whether its dispersion is",
                             " an SD or an SE; recorded as SD"))
  flags
}

#' Parse the baseline demographic table of a trial PDF
#'
#' Reads the baseline characteristics table ("Table 1") out of a randomized
#' controlled trial PDF and returns it as one line per baseline variable per
#' treatment arm, in the input layout of the Integrity-Analysis app.
#'
#' The deterministic engine ([parseBaselineTableHeuristics()]) always runs
#' first and its rows always win. What happens next depends on `ai`:
#'
#' * `"fallback"` (the default) consults the Claude API only when the
#'   deterministic pass left something unread - see [reviewFlags()] - and adds
#'   only those variables the deterministic pass did not produce. It never
#'   overwrites a value that was located on the page by coordinate.
#' * `"never"` is purely deterministic: no network call is made under any
#'   circumstance. Use this when the provenance of every number has to be
#'   mechanical, or when there is no API key.
#' * `"always"` skips the deterministic pass and asks the model to read the
#'   whole table. Useful for comparing the two engines against each other.
#'
#' If no table can be read by either engine and `prose = TRUE`, a last attempt
#' asks the model to find baseline characteristics in the article's running
#' text. Some trials never tabulate them — age, weight and sex appear in a
#' sentence in the Methods — and in a 250-article sample about a third of the
#' articles nothing could be extracted from were of that kind.
#'
#' Rows are tagged in `$provenance` with the engine that produced them, and
#' [writeIntegrityTemplate()] carries that tagging into the spreadsheet.
#'
#' @inheritParams parseBaselineTableHeuristics
#' @param ai When to consult the Claude API: `"fallback"`, `"never"`, or
#'   `"always"`. See Details.
#' @param prose When no table can be read at all, ask the model for baseline
#'   data stated in the article's running text — some trials report age,
#'   weight and sex in a sentence rather than tabulating them. Only reached
#'   when `ai` is not `"never"` and the table routes have both failed. Rows
#'   found this way are tagged `"ai-prose"` in `$provenance`.
#' @param model,effort,maxTokens,apiKey Passed to [parseBaselineTableAI()].
#'
#' @return An object of class `ParsePDFTable`. `$engine` is `"heuristic"`,
#'   `"ai"`, or `"hybrid"`; `$provenance` records the engine per row; and
#'   `$flags` records why the fallback was consulted, if it was.
#'
#' @examples
#' \dontrun{
#' # Deterministic only - no network call, fully reproducible
#' res <- parseBaselineTable("trial.pdf", ai = "never")
#' res
#' res$data
#'
#' # Let the model fill in what the heuristics could not read
#' res <- parseBaselineTable("trial.pdf")
#' subset(res$provenance, ENGINE == "ai")
#'
#' writeIntegrityTemplate(res, "trial.xlsx")
#' }
#' @export
parseBaselineTable <- function(pdfFile,
                               trial         = tools::file_path_sans_ext(basename(pdfFile)),
                               pages         = NULL,
                               ai            = c("fallback", "never", "always"),
                               prose         = TRUE,
                               parenIsSD     = c("auto", "sd", "percent"),
                               roundObsDelta = 1,
                               pctApprox     = FALSE,
                               model         = .ppDefaultModel,
                               effort        = "medium",
                               maxTokens     = 16000L,
                               apiKey        = NULL,
                               quiet         = FALSE)
{
  ai        <- match.arg(ai)
  parenIsSD <- match.arg(parenIsSD)
  say <- function(...) if (!quiet) message(...)

  # The AI fallback renders PDF pages (parseBaselineTableAI), which a
  # .docx does not have. Fail clearly up front rather than obscurely
  # later; the deployed app always passes ai = "never", so this guard
  # only ever fires for console users (issue 19).
  if (grepl("[.]docx$", pdfFile, ignore.case = TRUE) && ai != "never")
    stop("The AI fallback is not available for .docx input - it reads ",
         "rendered PDF pages. Call with ai = \"never\".")

  # ---- AI-only path -------------------------------------------------------
  if (ai == "always")
    return(parseBaselineTableAI(pdfFile, trial = trial, pages = pages,
                                model = model, effort = effort,
                                maxTokens = maxTokens,
                                roundObsDelta = roundObsDelta,
                                apiKey = apiKey, quiet = quiet))

  # ---- Deterministic pass, always first -----------------------------------
  het <- tryCatch(
    parseBaselineTableHeuristics(pdfFile, trial = trial, pages = pages,
                                 parenIsSD = parenIsSD,
                                 roundObsDelta = roundObsDelta,
                                 pctApprox = pctApprox, quiet = quiet),
    error = function(e) e)

  if (inherits(het, "error")) {
    if (ai == "never") stop(het)
    say("Deterministic parse failed (", conditionMessage(het),
        "); falling back to ", model, ".")
    out <- tryCatch(
      parseBaselineTableAI(pdfFile, trial = trial, pages = pages,
                           source = "table", model = model, effort = effort,
                           maxTokens = maxTokens,
                           roundObsDelta = roundObsDelta,
                           apiKey = apiKey, quiet = quiet),
      error = function(e) e)

    # Some trials never tabulate their baseline data - age, weight and sex are
    # given in a sentence in the Methods instead. In a 250-article sample,
    # about a third of the articles nothing could be extracted from were of
    # that kind, so when no table can be read anywhere, ask for the prose.
    if (inherits(out, "error") && prose) {
      say("No table could be read (", conditionMessage(out),
          "); asking ", model, " for baseline data stated in the text.")
      out <- parseBaselineTableAI(pdfFile, trial = trial, source = "prose",
                                  model = model, effort = effort,
                                  maxTokens = maxTokens,
                                  roundObsDelta = roundObsDelta,
                                  apiKey = apiKey, quiet = quiet)
    } else if (inherits(out, "error")) {
      stop(out)
    }
    out$flags <- paste0("deterministic parse failed: ", conditionMessage(het))
    return(out)
  }

  flags <- reviewFlags(het)
  het$flags <- flags
  if (ai == "never" || length(flags) == 0) return(het)

  if (!claudeAvailable() && is.null(apiKey)) {
    say("The deterministic parse left ", length(flags), " issue(s) open, but ",
        "ANTHROPIC_API_KEY is not set - returning the deterministic result. ",
        "Review $skipped by hand.")
    return(het)
  }

  # ---- AI fallback for what the heuristics could not read -----------------
  say("Deterministic parse left ", length(flags),
      " issue(s) open; consulting ", model, " for the rest:")
  for (f in flags) say("  - ", f)

  hint <- paste0(
    "Arms already identified: ",
    paste(sprintf("%s (n = %s)", het$arms$arm,
                  ifelse(is.na(het$arms$N), "unknown", het$arms$N)),
          collapse = "; "),
    ". Variables already read: ",
    paste(unique(het$data$ROW), collapse = "; "), ".")

  aiRes <- tryCatch(
    parseBaselineTableAI(pdfFile, trial = trial, pages = het$pages,
                         model = model, effort = effort, maxTokens = maxTokens,
                         roundObsDelta = roundObsDelta, hint = hint,
                         apiKey = apiKey, quiet = quiet),
    error = function(e) e)

  if (inherits(aiRes, "error")) {
    say("The AI fallback failed (", conditionMessage(aiRes),
        "); returning the deterministic result.")
    het$flags <- c(flags, paste0("AI fallback failed: ",
                                 conditionMessage(aiRes)))
    return(het)
  }

  # Merge: keep every deterministic row, add only variables the deterministic
  # pass never produced. Comparison is on the squished, case-folded label so
  # that "Age, yr" and "age, yr" are recognized as the same variable.
  key      <- function(v) tolower(.ppSquish(v))
  haveRows <- unique(key(het$data$ROW))
  newRows  <- aiRes$data[!key(aiRes$data$ROW) %in% haveRows, , drop = FALSE]

  if (nrow(newRows) == 0) {
    say("The model found nothing the deterministic pass had missed.")
    het$flags <- flags
    return(het)
  }
  say("Adding ", nrow(newRows), " line(s) from ", model,
      " for: ", paste(unique(newRows$ROW), collapse = ", "),
      ". These are tagged \"ai\" in $provenance - check them against the ",
      "printed table.")

  merged <- .ppRbindFill(het$data, newRows)
  merged <- merged[, c(.ppBaseColumns(),
                       setdiff(names(merged), .ppBaseColumns())), drop = FALSE]

  structure(
    list(data       = merged,
         arms       = het$arms,
         skipped    = het$skipped,
         provenance = rbind(het$provenance,
                            data.frame(ROW = newRows$ROW,
                                       ENGINE = rep("ai", nrow(newRows)),
                                       stringsAsFactors = FALSE)),
         pages      = het$pages,
         caption    = het$caption,
         trial      = trial,
         notes      = aiRes$notes,
         flags      = flags,
         engine     = "hybrid"),
    class = "ParsePDFTable")
}

#' @export
print.ParsePDFTable <- function(x, ...) {
  cat("<ParsePDFTable>  trial: ", x$trial, "\n", sep = "")
  byModel <- !is.null(x$provenance) && any(grepl("^ai", x$provenance$ENGINE))
  cat("  engine : ", x$engine,
      if (byModel)
        paste0(" (", sum(grepl("^ai", x$provenance$ENGINE)), " of ",
               nrow(x$provenance), " lines read by the model)") else "",
      "\n", sep = "")
  if (identical(x$engine, "ai-prose"))
    cat("           values were read from running text, not a table\n")
  cat("  page(s): ",
      if (all(is.na(x$pages))) "whole article" else
        paste(x$pages, collapse = ", "), "\n", sep = "")
  if (!is.na(x$caption) && nzchar(x$caption))
    cat("  caption: ", substr(x$caption, 1, 60), "\n", sep = "")
  cat("  arms   : ", nrow(x$arms), " (",
      paste(sprintf("%s n=%s", x$arms$arm,
                    ifelse(is.na(x$arms$N), "?", x$arms$N)), collapse = ", "),
      ")\n", sep = "")
  cat("  lines  : ", nrow(x$data), " over ",
      length(unique(x$data$ROW)), " variable(s)\n", sep = "")
  if (!is.null(x$dispersion))
    cat("  spread : ", x$dispersion,
        if ("SE" %in% names(x$data) && any(!is.na(x$data$SE)))
          paste0(" (", sum(!is.na(x$data$SE)), " row(s) in the SE column)")
        else "", "\n", sep = "")
  if (nrow(x$skipped) > 0) {
    cat("  skipped: ", nrow(x$skipped), "\n", sep = "")
    for (i in seq_len(nrow(x$skipped)))
      cat("    - ", x$skipped$label[i], ": ", x$skipped$reason[i], "\n", sep = "")
  }
  if (!is.null(x$notes) && nzchar(x$notes))
    cat("  model notes: ", x$notes, "\n", sep = "")
  cat("Check the parsed values against the printed table before analyzing.\n")
  invisible(x)
}
