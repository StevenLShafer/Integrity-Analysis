# Issue 18: the parse engine emits median [Q1, Q3] rows when the text
# says the interval is an IQR.
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-21,
# with the medianRng changes in R/tokenize.R and
# R/parseBaselineTableHeuristics.R. The app has accepted median/Q1/Q3
# rows since issue 12; the engine's old unconditional skip predated
# that. The gate is deliberately conservative: an IQR and a min-max
# range are numerically indistinguishable (both straddle the median),
# so emission requires the row label - or, failing that, the caption or
# footnote - to say IQR; a stated range, or silence, still skips.
suppressWarnings(suppressPackageStartupMessages({
  library(shiny)
}))

# a synthetic PDF whose median rows carry the given label and footnote
medianPdf <- function(dir = tempdir(), label, footnote,
                      cells1 = "127 [98, 160]", cells2 = "133 [101, 155]",
                      name = "median.pdf") {
  f  <- file.path(dir, name)
  vx <- c(300, 420)
  makeTablePdf(f, c(
    list(list(x = 72, y = 80,
              text = "Table 1. Baseline patient characteristics", adj = 0)),
    rowCells(110, "", c("Control", "Treatment"), vx),
    rowCells(128, "", c("(n = 15)", "(n = 17)"), vx),
    rowCells(150, "Age (yr)", c("45.3 ± 12.1", "46.1 ± 11.8"), vx),
    rowCells(168, label, c(cells1, cells2), vx),
    if (!is.null(footnote))
      list(list(x = 72, y = 200, text = footnote, adj = 0))
  ))
}

test_that("the tokenizer reads comma-separated medians and keeps all three numbers", {
  line <- data.frame(text = c("127", "[98,", "160]"),
                     x = c(300, 330, 365), width = c(20, 25, 25),
                     stringsAsFactors = FALSE)
  tok <- .ppTokenizeLine(line)
  expect_identical(tok$type[1], "medianRng")
  expect_identical(tok$num1[1], 127)
  expect_identical(tok$num2[1], 98)
  expect_identical(tok$num3[1], 160)
  # the dash form still matches, and now carries its third number too
  tok2 <- .ppTokenizeLine(data.frame(text = "127 [98-160]", x = 0,
                                     width = 12, stringsAsFactors = FALSE))
  expect_identical(tok2$type[1], "medianRng")
  expect_identical(tok2$num3[1], 160)
})

test_that("a row whose label says IQR emits median, Q1, Q3", {
  f <- medianPdf(label = "Duration of surgery, median (IQR)",
                 footnote = NULL, name = "iqr-label.pdf")
  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  d <- res$data
  dur <- d[grepl("Duration", d$ROW), ]
  expect_identical(nrow(dur), 2L)
  expect_identical(dur$MEAN, c(127, 133))
  expect_identical(dur$Q1, c(98, 101))
  expect_identical(dur$Q3, c(160, 155))
  expect_true(all(is.na(dur$SD)))
  expect_identical(dur$N, c(15L, 17L))
  # the tag is stripped from the ROW label
  expect_false(any(grepl("(?i)median", dur$ROW, perl = TRUE)))
  expect_identical(nrow(res$skipped), 0L)
  # and the emitted rows validate as median rows end to end
  v <- shiny::isolate(validateData(d))
  expect_false(v$FAIL)
})

test_that("a footnote saying interquartile range unlocks unlabeled rows", {
  f <- medianPdf(label = "Length of stay (d)",
                 footnote = "Data are mean ± SD or median [interquartile range].",
                 cells1 = "5 [3, 9]", cells2 = "6 [4, 10]",
                 name = "iqr-footnote.pdf")
  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  stay <- res$data[grepl("Length of stay", res$data$ROW), ]
  expect_identical(stay$MEAN, c(5, 6))
  expect_identical(stay$Q1, c(3, 4))
  expect_identical(stay$Q3, c(9, 10))
})

test_that("a stated range still skips, with the quartile-focused reason", {
  f <- medianPdf(label = "Duration of surgery (min)",
                 footnote = "Values are mean ± SD or median [range].",
                 name = "range-footnote.pdf")
  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  expect_false(any(grepl("Duration", res$data$ROW)))
  expect_identical(nrow(res$skipped), 1L)
  expect_match(res$skipped$reason[1], "quartiles")
  # no median row, so no Q1/Q3 columns clutter the output
  expect_false(any(c("Q1", "Q3") %in% names(res$data)))
})

test_that("an unlabeled interval skips as ambiguous rather than guessing", {
  f <- medianPdf(label = "Duration of surgery (min)", footnote = NULL,
                 name = "no-evidence.pdf")
  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  expect_false(any(grepl("Duration", res$data$ROW)))
  expect_match(res$skipped$reason[1], "unlabeled interval")
})

test_that("a median outside its own interval refuses the row", {
  f <- medianPdf(label = "Duration, median (IQR)", footnote = NULL,
                 cells1 = "90 [98, 160]",   # median below Q1
                 name = "bad-median.pdf")
  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  expect_match(res$skipped$reason[1], "outside its own")
})

test_that("the row label outranks table-level text when they disagree", {
  # footnote says IQR, but THIS row says range - the row wins and skips
  f <- medianPdf(label = "Blood loss, median (range)",
                 footnote = "Continuous data are median [IQR] unless stated.",
                 name = "row-beats-doc.pdf")
  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  expect_false(any(grepl("Blood loss", res$data$ROW)))
  expect_match(res$skipped$reason[1], "quartiles")
})
