# parseBaselineTableFiles.R - parse a whole directory of PDFs safely.
#
############################################################################
# Provenance                                                               #
# Written 2026-08-15 by Claude Code (model: Claude Opus 5, Anthropic) at   #
# Steve Shafer's request, after running the parser over samples of the     #
# 1,865-article corpus in C:/temp/journals. A plain loop over that corpus  #
# never finishes: PMID_17697219.pdf hangs poppler forever - even           #
# pdftools::pdf_info() never returns - and R cannot interrupt a C library  #
# that has stopped returning. Each file therefore gets its own R process   #
# and an operating-system timeout.                                         #
# Status: run and verified by tests/testthat/test-batch.R, and used to     #
# produce the validation figures quoted in README.md.                      #
############################################################################

#' Parse every PDF and Word manuscript in a directory
#'
#' Runs [parseBaselineTable()] over many PDFs and `.docx` manuscripts and
#' returns one row per file describing what happened, with the parsed
#' table itself kept in a list column.
#'
#' Each file is parsed in a **separate R process** with a timeout. That is not
#' defensive programming for its own sake: a malformed PDF can hang poppler
#' indefinitely, and because the hang is inside C code, `setTimeLimit()` and
#' `tryCatch()` cannot end it. A file that hangs or crashes is recorded with
#' `ok = FALSE` and the run carries on. The cost is a process launch per file,
#' roughly half a second. A `.docx` (a zip of XML read through officer /
#' libxml2) cannot execute anything, but crafted XML can stall or exhaust
#' its parser - the same subprocess-and-timeout containment covers it, which
#' is why the app routes Word uploads through here too (issue 19).
#'
#' `ai` defaults to `"never"` here, unlike in [parseBaselineTable()]. A
#' directory can hold thousands of articles, and each fallback is a billable
#' API call; opting a whole corpus into that should be deliberate. When you do
#' pass `ai = "fallback"`, the number of files that will be sent is reported
#' before any call is made.
#'
#' @param files A directory (every `.pdf` and `.docx` under it is parsed,
#'   recursively) or a character vector of PDF / .docx paths.
#' @param outputDir If given, each successful parse is also written there as
#'   `<file>.xlsx` by [writeIntegrityTemplate()]. Created if missing.
#' @param timeout Seconds allowed per file before its process is killed.
#' @param ai Passed to [parseBaselineTable()]. Defaults to `"never"` - see
#'   Details.
#' @param quiet Suppress per-file progress messages.
#' @param ... Further arguments for [parseBaselineTable()], e.g.
#'   `roundObsDelta` or `layout`.
#'
#' @return A data frame with one row per file: `file`, `ok`, `error`,
#'   `seconds`, `page`, `layout`, `arms`, `armsWithN`, `lines`, `variables`,
#'   `continuous`, `skipped`, `engine`, and `result` - a list column holding
#'   each `ParsePDFTable`, or `NULL` where parsing failed.
#'
#' @examples
#' \dontrun{
#' res <- parseBaselineTableFiles("C:/temp/journals", outputDir = "out")
#' table(res$ok)
#' subset(res, !ok)[, c("file", "error")]
#'
#' # The parsed table for one article
#' res$result[[1]]$data
#' }
#' @seealso [parseBaselineTable()]
#' @export
parseBaselineTableFiles <- function(files,
                                    outputDir = NULL,
                                    timeout   = 60,
                                    ai        = c("never", "fallback", "always"),
                                    quiet     = FALSE,
                                    ...) {
  ai  <- match.arg(ai)
  say <- function(...) if (!quiet) message(...)

  if (length(files) == 1 && dir.exists(files))
    files <- list.files(files, pattern = "[.](pdf|docx)$", full.names = TRUE,
                        recursive = TRUE, ignore.case = TRUE)
  files <- files[nzchar(files)]
  if (length(files) == 0) stop("No PDF or Word files to parse.")
  if (!is.null(outputDir) && !dir.exists(outputDir))
    dir.create(outputDir, recursive = TRUE)

  # FIX (2026-08-17): was package = "ParsePDF" - a fold-in leftover. It
  # worked on the development machine only because the retired ParsePDF
  # package was still installed there and silently supplied the script;
  # on shinyapps.io it made every PDF upload fail instantly.
  script <- system.file("scripts", "parseOne.R",
                        package = "IntegrityAnalysis")
  if (!nzchar(script))
    stop("Could not find parseOne.R inside the installed package.")
  rscript <- file.path(R.home("bin"),
                       if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

  if (ai != "never")
    say("ai = \"", ai, "\": up to ", length(files),
        " article(s) may be sent to the Claude API, one call each.")

  optsRds <- tempfile(fileext = ".rds")
  # devPath (2026-08-20, found by the renv adoption): when this parent
  # is a pkgload dev tree (load_all during development and testing),
  # tell the child to load THE SAME WORKING TREE. The child used to
  # library(IntegrityAnalysis) unconditionally, which under load_all
  # silently resolved to whatever stale INSTALLED copy the library
  # happened to hold - local subprocess tests were exercising the
  # last-installed parser, not the code under test. renv's isolated
  # library had no installed copy at all, which is how this surfaced.
  # In installed contexts (R CMD check, shinyapps.io) devPath is NULL
  # and the child loads the installed package exactly as before.
  devPath <- tryCatch(
    if (requireNamespace("pkgload", quietly = TRUE) &&
        pkgload::is_dev_package("IntegrityAnalysis"))
      pkgload::pkg_path() else NULL,
    error = function(e) NULL)
  saveRDS(list(libPaths = .libPaths(),
               devPath  = devPath,
               args     = c(list(ai = ai), list(...))), optsRds)
  on.exit(unlink(optsRds), add = TRUE)

  rows <- vector("list", length(files))
  for (i in seq_along(files)) {
    f      <- files[i]
    outRds <- tempfile(fileext = ".rds")
    t0     <- Sys.time()
    status <- tryCatch(
      system2(rscript,
              c("--vanilla", shQuote(script), shQuote(optsRds),
                shQuote(outRds), shQuote(f)),
              stdout = FALSE, stderr = FALSE, timeout = timeout),
      warning = function(w) 124L)   # system2 warns when it kills on timeout
    secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)

    res <- if (file.exists(outRds)) readRDS(outRds) else NULL
    unlink(outRds)

    if (inherits(res, "ParsePDFTable")) {
      d <- res$data
      cont <- unique(d$ROW[!is.na(d$MEAN)])
      rows[[i]] <- data.frame(
        file = basename(f), ok = TRUE, error = NA_character_, seconds = secs,
        page = res$pages[1],
        layout = if (is.null(res$layout)) NA_character_ else res$layout,
        arms = nrow(res$arms), armsWithN = sum(!is.na(res$arms$N)),
        lines = nrow(d), variables = length(unique(d$ROW)),
        continuous = length(cont), skipped = nrow(res$skipped),
        engine = res$engine, stringsAsFactors = FALSE)
      if (!is.null(outputDir))
        writeIntegrityTemplate(
          res, file.path(outputDir,
                         paste0(tools::file_path_sans_ext(basename(f)), ".xlsx")))
      say(sprintf("[%d/%d] %s: %d line(s), %d arm(s)", i, length(files),
                  basename(f), nrow(d), nrow(res$arms)))
    } else {
      msg <- if (inherits(res, "ppFailure")) res$message
             else if (!identical(status, 0L))
               paste0("the parser process did not finish within ", timeout,
                      "s, or crashed")
             else "no result was produced"
      rows[[i]] <- data.frame(
        file = basename(f), ok = FALSE, error = msg, seconds = secs,
        page = NA_integer_, layout = NA_character_, arms = NA_integer_,
        armsWithN = NA_integer_, lines = 0L, variables = 0L,
        continuous = 0L, skipped = NA_integer_, engine = NA_character_,
        stringsAsFactors = FALSE)
      say(sprintf("[%d/%d] %s: %s", i, length(files), basename(f), msg))
    }
    rows[[i]]$result <- I(list(if (inherits(res, "ParsePDFTable")) res else NULL))
  }

  out <- do.call(rbind, rows)
  say("Parsed ", sum(out$ok), " of ", nrow(out), " file(s).")
  if (any(!out$ok))
    say("Failures are listed in the `error` column; nothing was silently ",
        "dropped.")
  out
}
