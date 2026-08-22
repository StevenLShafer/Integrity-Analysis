# tokenize.R - turning one line of a table into numeric "cells".
#
############################################################################
# Provenance                                                               #
# Ported 2026-08-15 by Claude Code (model: Claude Opus 5, Anthropic) from  #
# parseCovariateTable.R in the Integrity-Analysis repository (drafted      #
# 2026-08-14 by Claude Code, model Claude Fable 5). Logic unchanged; only  #
# the `.pcv` -> `.pp` rename and the file split.                          #
# Deterministic: no AI service is called here.                            #
# Status: run and verified by tests/testthat/test-tokenize.R.             #
############################################################################
#
# A line is stored as a data frame of words (text, x, width).  We join the
# words with single spaces, remember where each word starts in the joined
# string, and scan the joined string with one master regular expression.
# Matching on the joined string (rather than word by word) makes the
# tokenizer indifferent to how poppler happened to split the cell:
# "45.3 \u00b1 12.1", "45.3\u00b112.1", and "45.3 \u00b112.1" all match the same
# pattern.
#
# Token types, tried in priority order (first alternative wins in PCRE):
#   meanSD    45.3 \u00b1 12.1   |  45.3 +/- 12.1
#   medianRng 45 [30-60]    |  45 [30, 60] |  45 (30 to 60)
#             -> Q1/Q3 when the text says IQR, else skipped (issue 18)
#   nPct      15 (60%)      |  15 (60.0 %)
#   numParen  45.3 (12.1)   -> mean (SD) or n (%) - disambiguated later
#   fraction  15/10         |  12/8/5      (sex, ASA class, ...)
#   pctOnly   60%                                             -> unusable
#   plain     45.3          (category counts; header n's)
#
# The (?<![A-Za-z0-9_.]) guard keeps digits inside words like "SpO2" or
# "CO2" from starting a token.

# A printed number, allowing more than one separator group so that a
# thousands-separated value ("4,335" or "1,234.5") is captured whole rather
# than split into "4" and ",335".
.ppNUM <- "[<>]?-?\\d+(?:[.,\u00b7]\\d+)*"

.ppTokenRegex <- local({
  NUM <- .ppNUM
  paste0(
    "(?<![A-Za-z0-9_.])(?:",
    "(?<meanSD>",    NUM, "\\s*(?:\u00b1|\\+/-|\\+-)\\s*", NUM, ")",
    # interval separator: hyphen, en/em dash, Unicode minus (U+2212 - what
    # PDF fonts often use for "-"), the word "to" (ranges as journals
    # print them), plus comma/semicolon - the "median [Q1, Q3]" form
    # (issue 18); the comma cannot be mistaken for a thousands separator
    # because NUM only absorbs a comma when digits follow it immediately
    "|(?<medianRng>", NUM, "\\s*[\\[(]\\s*", NUM,
                     "\\s*(?:\u2013|\u2014|\u2212|-|to|[,;])\\s*", NUM, "\\s*[\\])])",
    # (.ppMedianParts below re-parses a matched medianRng token into its
    # three numbers; keep the two patterns in sync)
    "|(?<nPct>",     NUM, "\\s*\\(\\s*", NUM, "\\s*%\\s*\\))",
    "|(?<numParen>", NUM, "\\s*\\(\\s*", NUM, "\\s*\\))",
    "|(?<fraction>", "\\d+(?:\\s*/\\s*\\d+)+)",
    "|(?<pctOnly>",  NUM, "\\s*%)",
    "|(?<plain>",    NUM, ")",
    ")(?![A-Za-z0-9])"
  )
})

# The medianRng token's internal structure, with capture groups: median,
# lower bound, upper bound. Kept in sync with the medianRng alternative of
# .ppTokenRegex above; used by .ppTokenizeLine because a bare number grep
# over "127 [98-160]" would read the separator dash as a minus sign.
.ppMedianParts <- local({
  NUM <- .ppNUM
  paste0("(", NUM, ")\\s*[\\[(]\\s*(", NUM,
         ")\\s*(?:\u2013|\u2014|\u2212|-|to|[,;])\\s*(", NUM, ")\\s*[\\])]")
})

# Tokenize one line.  `line` is a data frame with columns text, x, width
# (one row per word, already sorted by x).  Returns a data frame with one
# row per token: type, text, the numbers it contains (num1, num2 =
# first/second number, e.g. mean and SD), decimals of num1, and the token's
# x extent (x0, x1, mid) recovered from the word coordinates.
.ppTokenizeLine <- function(line) {
  joined    <- paste(line$text, collapse = " ")
  wordStart <- cumsum(c(1, nchar(line$text) + 1))[seq_len(nrow(line))]
  wordEnd   <- wordStart + nchar(line$text) - 1

  m <- gregexpr(.ppTokenRegex, joined, perl = TRUE)[[1]]
  if (m[1] == -1) {
    return(data.frame(type = character(0), text = character(0),
                      num1 = numeric(0), num2 = numeric(0),
                      num3 = numeric(0),
                      dec1 = integer(0), dec2 = integer(0),
                      dec3 = integer(0),
                      x0 = numeric(0), x1 = numeric(0),
                      mid = numeric(0), start = integer(0),
                      stringsAsFactors = FALSE))
  }

  starts <- as.integer(m)
  lens   <- attr(m, "match.length")
  # Which named group matched, per token
  capStarts <- attr(m, "capture.start")   # matrix tokens x groups
  capNames  <- attr(m, "capture.names")

  tokens <- lapply(seq_along(starts), function(i) {
    tokText <- substr(joined, starts[i], starts[i] + lens[i] - 1)
    type    <- capNames[capStarts[i, ] > 0][1]
    nums    <- if (type == "medianRng") {
      # structured extraction: a bare .ppNUM grep over "127 [98-160]"
      # reads the separator dash as 160's minus sign; the capture groups
      # keep separator and sign apart
      mm <- regmatches(tokText,
                       regexec(.ppMedianParts, tokText, perl = TRUE))[[1]]
      mm[2:4]
    } else regmatches(tokText, gregexpr(.ppNUM, tokText, perl = TRUE))[[1]]
    # Map character positions back to words to recover x coordinates
    wFirst  <- which(wordEnd   >= starts[i])[1]
    wLast   <- rev(which(wordStart <= starts[i] + lens[i] - 1))[1]
    x0      <- line$x[wFirst]
    x1      <- line$x[wLast] + line$width[wLast]
    data.frame(type = type, text = tokText,
               num1 = if (length(nums) >= 1) .ppAsNumeric(nums[1]) else NA_real_,
               num2 = if (length(nums) >= 2) .ppAsNumeric(nums[2]) else NA_real_,
               # The THIRD number exists only in a medianRng token -
               # "127 [98, 160]" is median, Q1, Q3 - and is what lets the
               # engine emit quartiles instead of skipping the row (issue 18).
               num3 = if (length(nums) >= 3) .ppAsNumeric(nums[3]) else NA_real_,
               dec1 = if (length(nums) >= 1) .ppDecimals(nums[1]) else NA_integer_,
               # Decimals of the SECOND number too: when a table prints a
               # standard error rather than a standard deviation, the printed
               # granularity of that value is data in its own right, and it is
               # not recoverable from the mean's.
               dec2 = if (length(nums) >= 2) .ppDecimals(nums[2]) else NA_integer_,
               dec3 = if (length(nums) >= 3) .ppDecimals(nums[3]) else NA_integer_,
               x0 = x0, x1 = x1, mid = (x0 + x1) / 2,
               start = starts[i],
               stringsAsFactors = FALSE)
  })
  do.call(rbind, tokens)
}
