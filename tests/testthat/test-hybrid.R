# The hybrid entry point. The live API call is not exercised here; what is
# tested is that the deterministic engine runs first, that the review flags
# fire on the right conditions, and that no network call happens when it is
# not wanted.

withoutApiKey <- function(code) {
  old <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA)
  Sys.unsetenv("ANTHROPIC_API_KEY")
  on.exit(if (!is.na(old)) Sys.setenv(ANTHROPIC_API_KEY = old), add = TRUE)
  force(code)
}

test_that("a cleanly parsed table raises no review flags", {
  res <- parseBaselineTableHeuristics(syntheticPdfMeanParen(),
                                      roundObsDelta = 0, quiet = TRUE)
  expect_equal(reviewFlags(res), character(0))
})

test_that("review flags name the skipped lines", {
  res <- parseBaselineTableHeuristics(syntheticPdfMeanSD(), quiet = TRUE)
  flags <- reviewFlags(res)
  expect_true(any(grepl("could not be used", flags)))
  expect_true(any(grepl("Duration", flags)))
})

test_that("review flags fire when arm N is unknown", {
  res <- parseBaselineTableHeuristics(syntheticPdfMeanSD(), quiet = TRUE)
  res$arms$N <- c(NA_integer_, 17L)
  expect_true(any(grepl("arm N is missing", reviewFlags(res))))
})

test_that('ai = "never" makes no network call and returns the deterministic result', {
  res <- withoutApiKey(
    parseBaselineTable(syntheticPdfMeanSD(), trial = "T",
                       ai = "never", quiet = TRUE))
  expect_equal(res$engine, "heuristic")
  expect_true(all(res$provenance$ENGINE == "heuristic"))
  expect_equal(nrow(res$data), 10)
})

test_that('ai = "fallback" degrades to the deterministic result with no key', {
  # The mean +/- SD fixture has a skipped row, so the fallback would fire if
  # a key were present. Without one it must return the deterministic table
  # rather than erroring.
  res <- withoutApiKey(
    parseBaselineTable(syntheticPdfMeanSD(), trial = "T", quiet = TRUE))
  expect_equal(res$engine, "heuristic")
  expect_true(length(res$flags) > 0)
})

test_that('ai = "fallback" does not consult the model when nothing is open', {
  # No flags, so no key is needed and none is consulted.
  res <- withoutApiKey(
    parseBaselineTable(syntheticPdfMeanParen(), trial = "T",
                       roundObsDelta = 0, quiet = TRUE))
  expect_equal(res$engine, "heuristic")
  expect_equal(res$flags, character(0))
})

test_that('ai = "never" propagates a deterministic failure as an error', {
  f <- file.path(tempdir(), "prose2.pdf")
  makeTablePdf(f, list(list(x = 72, y = 80, text = "No table here.", adj = 0)))
  expect_error(withoutApiKey(parseBaselineTable(f, ai = "never", quiet = TRUE)))
})

test_that("prose rows are tagged apart from table rows and from heuristics", {
  # Built from a canned reply rather than a live call: what is under test is
  # that the tag survives into provenance and the print method, not the API.
  reply <- jsonlite::fromJSON('{
    "found": true, "notes": "Age and weight given in the Methods.",
    "arms": [{"name": "A", "n": 12}, {"name": "B", "n": 14}],
    "continuous": [
      {"label": "Age", "decimalsMean": 0,
       "values": [{"arm": "A", "n": null, "mean": 49, "sd": 15},
                  {"arm": "B", "n": null, "mean": 54, "sd": 18}]}],
    "categorical": []}', simplifyVector = FALSE)
  tbl <- .ppAiToTemplate(reply, trial = "T")
  expect_equal(nrow(tbl$data), 2)
  expect_equal(tbl$data$MEAN, c(49, 54))
  expect_equal(tbl$data$N, c(12L, 14L))

  fake <- structure(list(
    data = tbl$data, arms = tbl$arms,
    skipped = data.frame(label = character(0), reason = character(0),
                         text = character(0)),
    provenance = data.frame(ROW = tbl$data$ROW,
                            ENGINE = rep("ai-prose", nrow(tbl$data)),
                            stringsAsFactors = FALSE),
    pages = NA_integer_, caption = NA_character_, trial = "T",
    notes = "Age and weight given in the Methods.", engine = "ai-prose"),
    class = "ParsePDFTable")

  out <- paste(capture.output(print(fake)), collapse = "\n")
  expect_match(out, "ai-prose")
  expect_match(out, "read from running text")
  expect_match(out, "whole article")          # no single page to point at
  expect_true(all(grepl("^ai", fake$provenance$ENGINE)))
})

test_that('prose is not attempted when ai = "never"', {
  f <- file.path(tempdir(), "prose-never.pdf")
  makeTablePdf(f, list(list(x = 72, y = 80, text = "No table here.", adj = 0)))
  # Would need an API key if it tried; it must fail deterministically instead.
  expect_error(withoutApiKey(
    parseBaselineTable(f, ai = "never", prose = TRUE, quiet = TRUE)))
})

test_that("the print method reports the engine and the provenance split", {
  res <- parseBaselineTableHeuristics(syntheticPdfMeanSD(), trial = "T",
                                      quiet = TRUE)
  out <- paste(capture.output(print(res)), collapse = "\n")
  expect_match(out, "heuristic")
  expect_match(out, "arms   : 2")
  expect_match(out, "Duration")            # the skipped line is surfaced
  expect_match(out, "Check the parsed values")
})
