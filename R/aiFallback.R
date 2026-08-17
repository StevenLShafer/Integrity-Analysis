# aiFallback.R - the optional AI extraction engine (Claude Messages API).
#
############################################################################
# Provenance                                                               #
# Written 2026-08-15 by Claude Code (model: Claude Opus 5, Anthropic) at   #
# Steve Shafer's request. New code, not ported from Integrity-Analysis.    #
#                                                                          #
# This is the ONLY file in the package that contacts a network service.    #
# It calls the Anthropic Messages API over raw HTTPS with httr2, because   #
# Anthropic publishes no official R SDK. Requests use structured outputs   #
# (output_config.format with a JSON schema), so the model must return the  #
# table in the exact shape declared in .ppTableSchemaJson() - there is no  #
# free-text parsing of the reply.                                          #
#                                                                          #
# The model is claude-opus-5 (list price at the time of writing: $5 per    #
# million input tokens, $25 per million output tokens - one table page is  #
# on the order of a few US cents).                                        #
#                                                                          #
# Two sources are supported. source = "table" sends one page and asks for   #
# the table on it. source = "prose" sends the article text and asks for     #
# baseline characteristics stated in the narrative: some trials never       #
# tabulate them, and in a 250-article sample about a third of the articles  #
# nothing could be extracted from were of that kind.                        #
#                                                                          #
# Status: the request-construction and response-decoding helpers are run   #
# and verified by tests/testthat/test-ai-fallback.R against canned JSON.    #
#                                                                          #
# source = "prose" has been RUN LIVE against claude-opus-5 and scored:     #
# over the ten corpus articles that report baseline data only in running   #
# text, it recovered 100 of Carlisle's 110 known mean/SD pairs (91%), with #
# eight of the ten fully recovered, for about $0.11 an article. The ten    #
# misses are not misreadings - the model located those values and excluded #
# them, disagreeing with Carlisle over what counts as baseline (duration   #
# of surgery; time points after T0). Do not "fix" that without deciding    #
# which definition the analysis wants.                                     #
#                                                                          #
# The widened definition of baseline in the prose system prompt came from  #
# that run: the first version listed only demographics, and the model      #
# dutifully discarded the pre-intervention physiological measurements that #
# Carlisle counts, scoring 2 of 14 on one article where it now scores 14.  #
#                                                                          #
# source = "table" has also been RUN LIVE, against the 21 corpus trials    #
# that have a baseline table the deterministic engine misread - all of     #
# which score zero against Carlisle. It recovered 205 of their 254 known   #
# pairs (81%), 14 of 21 articles completely, at about $0.055 an article.   #
#                                                                          #
# That path was worth only 50% until page selection was fixed. It chose    #
# the page by baseline vocabulary (.ppScorePage), which landed on prose    #
# discussing the results in seven of the 21 - and from the model's side an #
# empty page is indistinguishable from an article with no table, so those  #
# came back as flat refusals. It now selects by table caption              #
# (.ppBestCaptionPage), the same way the deterministic engine does.        #
#                                                                          #
# Trust note: numbers produced by this engine were read by a language      #
# model, not located on the page by coordinate. Every row it contributes   #
# is tagged "ai" in the result's `provenance` table. For research-integrity #
# work, check those rows against the printed table before relying on them. #
############################################################################

# The endpoint and API version are fixed here rather than made arguments,
# so a caller cannot accidentally point the package at another host.
.ppApiUrl      <- "https://api.anthropic.com/v1/messages"
.ppApiVersion  <- "2023-06-01"
.ppDefaultModel <- "claude-opus-5"

#' Is the Claude API reachable from this session?
#'
#' Checks only that an API key is present in the environment; it makes no
#' network call.
#'
#' @return `TRUE` if `ANTHROPIC_API_KEY` is set to a non-empty value.
#' @export
claudeAvailable <- function() {
  nzchar(Sys.getenv("ANTHROPIC_API_KEY"))
}

# Fetch the key, with an error that says exactly what to do about it.
.ppApiKey <- function(apiKey = NULL) {
  if (!is.null(apiKey) && nzchar(apiKey)) return(apiKey)
  key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (!nzchar(key))
    stop("No Anthropic API key. Set ANTHROPIC_API_KEY in ~/.Renviron ",
         "(then restart R), or pass apiKey=. To parse without the AI ",
         "fallback, use ai = \"never\".", call. = FALSE)
  key
}

# --------------------------------------------------------------------------
# The response schema
# --------------------------------------------------------------------------
# Held as a JSON string rather than a nested R list for two reasons: it is
# readable as-is against the API documentation, and round-tripping it through
# fromJSON(simplifyVector = FALSE) guarantees that JSON arrays stay arrays
# when httr2 re-serializes with auto_unbox = TRUE (a bare R character vector
# of length one would otherwise collapse to a scalar and break the schema).
.ppTableSchemaJson <- function() '{
  "type": "object",
  "additionalProperties": false,
  "required": ["found", "notes", "arms", "continuous", "categorical"],
  "properties": {
    "found": {
      "type": "boolean",
      "description": "true if baseline characteristics of the randomized groups were found in the text supplied."
    },
    "notes": {
      "type": "string",
      "description": "Anything a human reviewer should check: ambiguous cells, values reported as median and range, footnote definitions used, or an empty string."
    },
    "arms": {
      "type": "array",
      "description": "Treatment arms, in the left-to-right order of the printed table. Exclude any p-value or test-statistic column.",
      "items": {
        "type": "object",
        "additionalProperties": false,
        "required": ["name", "n"],
        "properties": {
          "name": {"type": "string", "description": "Arm label as printed, without the (n = ...) part."},
          "n": {"type": ["integer", "null"], "description": "Number of subjects in the arm, or null if the table does not state it."}
        }
      }
    },
    "continuous": {
      "type": "array",
      "description": "Variables reported as a mean with a standard deviation. Omit any variable reported only as a median with a range or interquartile range.",
      "items": {
        "type": "object",
        "additionalProperties": false,
        "required": ["label", "decimalsMean", "dispersion", "decimalsDispersion", "values"],
        "properties": {
          "label": {"type": "string", "description": "Variable name as printed, without the unit."},
          "decimalsMean": {"type": "integer", "description": "Number of digits printed after the decimal point in the mean, counted from the glyphs. 63 is 0, 61.3 is 1, 0.71 is 2."},
          "dispersion": {"type": "string", "enum": ["sd", "se", "unstated"], "description": "What the second number in each cell actually is, as the paper labels it. Use sd for a standard deviation, se for a standard error or SEM, and unstated if the table and its footnotes never say. Report what is printed. DO NOT convert one into the other - the analysis does that, because the conversion needs N and a bias correction."},
          "decimalsDispersion": {"type": "integer", "description": "Digits printed after the decimal point in the SD or SE, counted from the glyphs. It may differ from decimalsMean, as in a cell reading 39 (4.06), where the mean has 0 and the dispersion has 2."},
          "values": {
            "type": "array",
            "items": {
              "type": "object",
              "additionalProperties": false,
              "required": ["arm", "n", "mean", "sd"],
              "properties": {
                "arm": {"type": "string", "description": "Must match one of the arm names above."},
                "n": {"type": ["integer", "null"], "description": "Subjects contributing to this cell, or null to use the arm N."},
                "mean": {"type": ["number", "null"]},
                "sd": {"type": ["number", "null"], "description": "The dispersion value EXACTLY as printed, whether it is a standard deviation or a standard error. Which one it is goes in the dispersion field of the variable. Never convert between them."}
              }
            }
          }
        }
      }
    },
    "categorical": {
      "type": "array",
      "description": "Variables reported as counts. Report counts, never percentages. If only a percentage is printed and the arm N is known, convert to the nearest whole count and say so in notes.",
      "items": {
        "type": "object",
        "additionalProperties": false,
        "required": ["label", "categories", "values"],
        "properties": {
          "label": {"type": "string", "description": "Variable name as printed, e.g. Sex or Type of surgery."},
          "categories": {"type": "array", "description": "The mutually exclusive levels, e.g. Male and Female. Every subject in the arm must fall into exactly one level; add the complementary level yourself for a binary yes/no variable.", "items": {"type": "string"}},
          "values": {
            "type": "array",
            "items": {
              "type": "object",
              "additionalProperties": false,
              "required": ["arm", "counts"],
              "properties": {
                "arm": {"type": "string"},
                "counts": {"type": "array", "description": "One count per level, in the same order as categories.", "items": {"type": ["integer", "null"]}}
              }
            }
          }
        }
      }
    }
  }
}'

.ppSystemPrompt <- function(source = "table") paste(
  if (source == "prose")
    paste("You extract the baseline characteristics of a randomized",
          "controlled trial's treatment groups from the running text of the",
          "article - the Methods and Results narrative - for a Monte Carlo",
          "analysis of whether the reported baseline data are consistent with",
          "random allocation. The trial reports these in sentences rather",
          "than in a table.")
  else
    paste("You transcribe the baseline characteristics table (usually Table",
          "1) of a randomized controlled trial into structured data, for a",
          "Monte Carlo analysis of whether the reported baseline data are",
          "consistent with random allocation."),
  "",
  if (source == "prose")
    paste("Take every variable describing the groups *at baseline* - that is,",
          "measured at or before randomization, induction or the start of the",
          "intervention. This includes the demographics (age, weight, height,",
          "body mass index, sex, ASA status) AND baseline physiological and",
          "laboratory measurements: blood pressure, heart rate, central venous",
          "pressure, temperature, haemoglobin, and any other value recorded at",
          "time zero, at T0, pre-induction, pre-operatively or on admission.",
          "A pre-intervention measurement is baseline data even when it is",
          "printed in a table of results alongside later time points - take",
          "the time-zero column and leave the rest.",
          "",
          "Do NOT take outcomes, values measured during or after the",
          "intervention, doses given, or values pooled over the whole study",
          "population when the groups are reported separately. If a value is",
          "reported for the whole cohort and not per group, say so in notes",
          "and leave it out.")
  else "",
  "",
  "Transcribe only. Report what the table prints, cell by cell; do not",
  "reconcile inconsistencies, do not fill in values the table does not give,",
  "and do not carry a value across from another arm. Use null where the table",
  "is silent, and put anything doubtful in notes.",
  "",
  "The rounding of each printed mean is part of the data, not a formatting",
  "detail: count the digits after the decimal point exactly as printed, so a",
  "mean printed as 63 has 0 and one printed as 63.0 has 1.",
  "",
  "Exclude p-value, test-statistic, and effect-size columns; they are not",
  "treatment arms. Exclude variables reported only as a median with a range or",
  "an interquartile range, since the analysis needs a mean and a standard",
  "deviation, and note that you did.",
  sep = "\n")

.ppUserPrompt <- function(pageText, hint = NULL, source = "table") {
  paste0(
    if (source == "prose")
      paste0("Below is the text of a trial report, extracted from the PDF.",
             " No usable baseline characteristics table was found in it, so",
             " the baseline data are most likely stated in the narrative -",
             " typically a sentence in the Methods or at the start of the",
             " Results giving age, weight and sex per group. Read them from",
             " there. If the article genuinely does not report baseline",
             " characteristics per group, set found to false.\n\n")
    else
      paste0("Below is the text layer of one page of a trial report,",
             " extracted from the PDF. Word spacing survives; column",
             " alignment mostly does not, so use the row labels and the",
             " reading order to work out which value belongs to which",
             " arm.\n\n"),
    if (!is.null(hint) && nzchar(hint))
      paste0("What a deterministic parser already established about this",
             " article:\n", hint, "\n\n") else "",
    "---- BEGIN TEXT ----\n", pageText, "\n---- END TEXT ----\n")
}

# --------------------------------------------------------------------------
# The API call
# --------------------------------------------------------------------------

# Post one Messages request and return the decoded response body.
# Kept separate from the table logic so the request shape can be inspected
# and tested without a network.
.ppClaudeRequestBody <- function(pageText, hint = NULL,
                                 model = .ppDefaultModel,
                                 effort = "medium",
                                 maxTokens = 16000L,
                                 source = "table") {
  list(
    model      = model,
    max_tokens = maxTokens,
    system     = .ppSystemPrompt(source),
    # Thinking is on by default on claude-opus-5; max_tokens covers thinking
    # plus the reply, hence the generous default. effort trades depth for
    # cost - "medium" is ample for transcription.
    output_config = list(
      effort = effort,
      format = list(
        type   = "json_schema",
        schema = jsonlite::fromJSON(.ppTableSchemaJson(), simplifyVector = FALSE)
      )
    ),
    messages = list(list(role = "user",
                         content = .ppUserPrompt(pageText, hint, source)))
  )
}

.ppClaudePost <- function(body, apiKey, timeout = 300) {
  req <- httr2::request(.ppApiUrl)
  req <- httr2::req_headers(req,
                            "x-api-key"         = apiKey,
                            "anthropic-version" = .ppApiVersion,
                            "content-type"      = "application/json")
  req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  req <- httr2::req_timeout(req, timeout)
  req <- httr2::req_retry(req, max_tries = 3)
  # Surface the API's own error message rather than a bare status code.
  req <- httr2::req_error(req, body = function(resp) {
    err <- try(httr2::resp_body_json(resp), silent = TRUE)
    if (inherits(err, "try-error") || is.null(err$error$message)) NULL
    else paste0(err$error$type, ": ", err$error$message)
  })
  httr2::resp_body_json(httr2::req_perform(req))
}

# Pull the structured JSON out of a Messages response.
.ppClaudeStructuredOutput <- function(resp) {
  # A safety classifier can decline the request; that arrives as a normal
  # 200 with stop_reason "refusal" and no usable content.
  if (identical(resp$stop_reason, "refusal"))
    stop("The Claude API declined this request (stop_reason \"refusal\"",
         if (!is.null(resp$stop_details$category))
           paste0(", category \"", resp$stop_details$category, "\"") else "",
         "). Parse this PDF with ai = \"never\" and enter the table by hand.",
         call. = FALSE)
  if (identical(resp$stop_reason, "max_tokens"))
    stop("The reply hit max_tokens and is truncated. Retry with a larger ",
         "maxTokens, or with fewer pages.", call. = FALSE)
  txt <- vapply(resp$content,
                function(b) if (identical(b$type, "text")) b$text else "",
                character(1))
  txt <- paste(txt[nzchar(txt)], collapse = "")
  if (!nzchar(txt))
    stop("The Claude API returned no text content.", call. = FALSE)
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

# --------------------------------------------------------------------------
# Structured reply -> template rows
# --------------------------------------------------------------------------
.ppAiToTemplate <- function(parsed, trial, roundObsDelta = 1) {
  armNames <- vapply(parsed$arms, function(a) as.character(a$name), character(1))
  armN     <- vapply(parsed$arms,
                     function(a) if (is.null(a$n)) NA_integer_ else as.integer(a$n),
                     integer(1))
  names(armN) <- armNames

  catColumns <- character(0)
  rowsCont <- list()
  rowsCat  <- list()
  usedRowNames <- character(0)

  # ---- continuous variables ----------------------------------------------
  seen <- character(0)
  for (v in parsed$continuous) {
    rowName <- .ppUniqueName(.ppSquish(v$label), usedRowNames)
    usedRowNames <- c(usedRowNames, rowName)
    decMean <- if (is.null(v$decimalsMean)) NA_integer_ else as.integer(v$decimalsMean)
    decDisp <- if (is.null(v$decimalsDispersion)) NA_integer_
               else as.integer(v$decimalsDispersion)
    # The model reports which the printed value is; it never converts.
    kind <- if (is.null(v$dispersion)) "unstated" else as.character(v$dispersion)
    seen <- c(seen, kind)
    isSE <- identical(kind, "se")
    for (val in v$values) {
      if (is.null(val$mean) || is.null(val$sd)) next
      n <- if (!is.null(val$n)) as.integer(val$n) else
        unname(armN[as.character(val$arm)])
      rowsCont[[length(rowsCont) + 1]] <- data.frame(
        TRIAL = trial, ROW = rowName,
        N = n, MEAN = as.numeric(val$mean),
        SD = if (isSE) NA_real_ else as.numeric(val$sd),
        SE = if (isSE) as.numeric(val$sd) else NA_real_,
        ROUND_MEAN = decMean,
        ROUND_DISPERSION = decDisp,
        ROUND_OBSERVATION = decMean + roundObsDelta,
        stringsAsFactors = FALSE, check.names = FALSE)
    }
  }
  dispersionBasis <- if (!length(seen)) NA_character_
    else if (all(seen == "se")) "se (stated)"
    else if (all(seen == "sd")) "sd (stated)"
    else if (all(seen == "unstated")) "sd (assumed - table does not say)"
    else "mixed (per row)"

  # ---- categorical variables ---------------------------------------------
  for (v in parsed$categorical) {
    levels <- vapply(v$categories, as.character, character(1))
    if (length(levels) == 0) next
    # Category columns are shared across the whole spreadsheet, so a level
    # named "Other" in two different variables must not collide.
    levels <- vapply(levels, .ppUniqueName, character(1),
                     existing = catColumns)
    catColumns <- unique(c(catColumns, levels))
    rowName <- .ppUniqueName(.ppSquish(v$label), usedRowNames)
    usedRowNames <- c(usedRowNames, rowName)
    for (val in v$values) {
      counts <- vapply(val$counts,
                       function(x) if (is.null(x)) NA_integer_ else as.integer(x),
                       integer(1))
      if (length(counts) != length(levels)) next   # malformed row; drop it
      line <- data.frame(TRIAL = trial, ROW = rowName,
                         N = NA_integer_, MEAN = NA_real_,
                         SD = NA_real_, SE = NA_real_,
                         ROUND_MEAN = NA_integer_,
                         ROUND_DISPERSION = NA_integer_,
                         ROUND_OBSERVATION = NA_integer_,
                         stringsAsFactors = FALSE, check.names = FALSE)
      for (k in seq_along(levels)) line[[levels[k]]] <- counts[k]
      rowsCat[[length(rowsCat) + 1]] <- line
    }
  }

  DATA <- NULL
  for (r in c(rowsCont, rowsCat)) DATA <- .ppRbindFill(DATA, r)
  if (is.null(DATA)) {
    DATA <- data.frame(matrix(nrow = 0, ncol = length(.ppBaseColumns())))
    names(DATA) <- .ppBaseColumns()
  }
  # Keep the base columns leftmost, categories after them.
  DATA <- DATA[, c(.ppBaseColumns(),
                   setdiff(names(DATA), .ppBaseColumns())), drop = FALSE]

  list(data = DATA,
       arms = data.frame(arm = armNames, N = armN, row.names = NULL,
                         stringsAsFactors = FALSE),
       dispersion = dispersionBasis)
}

#' Parse a baseline table with the Claude API
#'
#' Sends the text layer of the table page to the Anthropic Messages API and
#' asks for the table back as structured JSON. Use this when the deterministic
#' engine cannot read a table's typography; [parseBaselineTable()] calls it
#' for you when needed.
#'
#' The values this function returns were read by a language model rather than
#' located on the page by coordinate, so they are tagged `"ai"` in the
#' result's `provenance` table. Check them against the printed table before
#' using them in an integrity analysis.
#'
#' Requires an API key in the `ANTHROPIC_API_KEY` environment variable (put it
#' in `~/.Renviron`, never in a script). One page costs a few US cents at
#' current `claude-opus-5` list prices.
#'
#' @inheritParams parseBaselineTableHeuristics
#' @param source `"table"` sends one page and asks for the table on it.
#'   `"prose"` sends the article text and asks for baseline characteristics
#'   stated in the narrative — some trials report age, weight and sex in a
#'   sentence in the Methods instead of tabulating them, and roughly a third
#'   of the articles this package cannot extract from are of that kind. Rows
#'   obtained this way are tagged `"ai-prose"` in `provenance`.
#' @param maxChars Ceiling on the number of characters of article text sent
#'   when `source = "prose"`, to bound the cost of one call.
#' @param model Anthropic model id. Defaults to `"claude-opus-5"`.
#' @param effort Reasoning effort passed to the API: `"low"`, `"medium"`,
#'   `"high"`, `"xhigh"` or `"max"`. Transcription does not need much;
#'   `"medium"` is the default.
#' @param maxTokens Output-token ceiling for the reply. Covers reasoning plus
#'   the JSON, so leave headroom.
#' @param hint Optional text passed to the model describing what a
#'   deterministic pass already established (arm names, arm N). Supplied
#'   automatically by [parseBaselineTable()].
#' @param apiKey API key. Defaults to the `ANTHROPIC_API_KEY` environment
#'   variable, which is the recommended way to supply it.
#' @param ocr,ocrDpi Read the article with OCR rather than its text layer, for
#'   scanned PDFs. Needs the `tesseract` package. This is the more promising
#'   route for a scan than the deterministic engine, which depends on word
#'   coordinates that OCR degrades — a model can read a noisy page the way a
#'   person does.
#'
#' @return An object of class `ParsePDFTable`, with `engine` `"ai"` and an
#'   extra `notes` element carrying the model's own caveats.
#' @export
parseBaselineTableAI <- function(pdfFile,
                                 trial         = tools::file_path_sans_ext(basename(pdfFile)),
                                 pages         = NULL,
                                 source        = c("table", "prose"),
                                 model         = .ppDefaultModel,
                                 effort        = "medium",
                                 maxTokens     = 16000L,
                                 maxChars      = 60000L,
                                 roundObsDelta = 1,
                                 hint          = NULL,
                                 apiKey        = NULL,
                                 ocr           = FALSE,
                                 ocrDpi        = 300,
                                 quiet         = FALSE)
{
  source <- match.arg(source)
  key <- .ppApiKey(apiKey)
  say <- function(...) if (!quiet) message(...)

  if (source == "prose") {
    # The whole article, since a baseline sentence can sit in the Methods or
    # at the head of the Results. Truncated to bound the cost of one call.
    pageText <- paste(if (isTRUE(ocr)) .ppOcrText(pdfFile, dpi = ocrDpi)
                      else .ppPdfText(pdfFile), collapse = "\n\n")
    if (nchar(pageText) > maxChars)
      pageText <- substr(pageText, 1, maxChars)
    pages <- NA_integer_
  } else {
    if (is.null(pages)) {
      allPages <- if (isTRUE(ocr)) .ppOcrData(pdfFile, dpi = ocrDpi)
                  else .ppPdfData(pdfFile)
      if (length(allPages) == 0)
        stop("No text layer found in ", pdfFile, ".")
      # Choose the page by table caption, the same way the deterministic
      # engine does. Picking it by baseline vocabulary instead - which is what
      # this did originally - sent the model to a page with no table on it in
      # seven of twenty-one corpus trials, and it can only answer that there
      # is no table there. Vocabulary scoring remains the fallback for
      # documents with no caption anywhere.
      pages <- .ppBestCaptionPage(allPages)
      if (is.null(pages))
        pages <- which.max(vapply(allPages, .ppScorePage, numeric(1)))
    }
    pageText <- paste(if (isTRUE(ocr)) .ppOcrText(pdfFile, dpi = ocrDpi, pages = pages)
                      else .ppPdfText(pdfFile)[pages], collapse = "\n\n")
  }
  if (!nzchar(.ppSquish(pageText)))
    stop("There is no text layer in ", pdfFile, " to send.")

  say("Asking ", model, " to read ",
      if (source == "prose") "the article text for baseline data stated in prose"
      else paste0("page ", paste(pages, collapse = ",")), " ...")
  body   <- .ppClaudeRequestBody(pageText, hint = hint, model = model,
                                 effort = effort, maxTokens = maxTokens,
                                 source = source)
  resp   <- .ppClaudePost(body, key)
  parsed <- .ppClaudeStructuredOutput(resp)

  if (!isTRUE(parsed$found))
    stop("The model found no baseline characteristics ",
         if (source == "prose") "in the text of this article"
         else paste0("table on page ", paste(pages, collapse = ",")),
         if (nzchar(parsed$notes %||% "")) paste0(" (", parsed$notes, ")") else "",
         ". Try the `pages` argument",
         if (source == "table") ", or source = \"prose\"" else "", ".",
         call. = FALSE)

  tbl <- .ppAiToTemplate(parsed, trial = trial, roundObsDelta = roundObsDelta)
  engine <- if (source == "prose") "ai-prose" else "ai"
  say("Model returned ", nrow(tbl$data), " template line(s) across ",
      nrow(tbl$arms), " arm(s).")
  if (nzchar(parsed$notes %||% "")) say("Model notes: ", parsed$notes)
  if (source == "prose")
    say("These values were read out of running text, not a table - there is ",
        "no printed row to check them against at a glance. Verify each one ",
        "against the article before analyzing it.")

  structure(
    list(data       = tbl$data,
         arms       = tbl$arms,
         dispersion = tbl$dispersion,
         skipped    = data.frame(label = character(0), reason = character(0),
                                 text = character(0)),
         provenance = data.frame(ROW = tbl$data$ROW,
                                 ENGINE = rep(engine, nrow(tbl$data)),
                                 stringsAsFactors = FALSE),
         pages      = pages,
         caption    = NA_character_,
         trial      = trial,
         notes      = parsed$notes %||% "",
         usage      = resp$usage,
         engine     = engine),
    class = "ParsePDFTable")
}

# Null-coalescing helper (R has no built-in one before 4.4).
`%||%` <- function(a, b) if (is.null(a)) b else a
