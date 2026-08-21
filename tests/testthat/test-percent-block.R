# Percent-block category tables (the .ppCountFromPct conversion).
# Motivated by the 654-submission AI comparison of 2026-08-21: ~800 rows
# were skipped because manuscripts tabulate categorical variables as bare
# percentages - "Male 55%", or "Race, %" over plain children, or a row
# whose own label announces the convention ("Gender (Male), % 47 44").

test_that("a percentage and the arm N pin the count, or refuse", {
  expect_equal(.ppCountFromPct(45, 0, 20), 9L)     # 9/20 = 45%
  expect_equal(.ppCountFromPct(68.4, 1, 19), 13L)  # 13/19 = 68.4%
  expect_equal(.ppCountFromPct(60, 0, 40), 24L)
  expect_equal(.ppCountFromPct(0, 0, 30), 0L)      # "0%" of 30 is 0
  # 47% of n = 702 spans 327..333: refuse rather than approximate
  expect_true(is.na(.ppCountFromPct(47, 0, 702)))
  expect_true(is.na(.ppCountFromPct(50, 0, NA)))
})

# One fixture exercises all three genres at once, with arm Ns printed in
# the header so the conversion has what it needs.
percentBlockPdf <- function(dir = tempdir()) {
  f  <- file.path(dir, "pctblock.pdf")
  vx <- c(300, 420)
  cells <- c(
    list(list(x = 72, y = 80,
              text = "Table 1. Baseline patient characteristics", adj = 0)),
    rowCells(110, "", c("Ketamine", "Saline"), vx),
    rowCells(136, "", c("(n = 20)", "(n = 25)"), vx),
    rowCells(164, "Age (yr)",           c("45.3 ± 12.1", "46.1 ± 11.8"), vx),
    rowCells(190, "Male sex",           c("55%",  "52%"),  vx),
    rowCells(216, "Diabetes, %",        c("25",   "20"),   vx),
    rowCells(242, "ASA status, %",      c("", ""),         vx),
    rowCells(268, "I",   c("45", "40"), vx, labelX = 86),
    rowCells(294, "II",  c("55", "60"), vx, labelX = 86))
  makeTablePdf(f, cells)
}

test_that("all three percent genres convert to counts", {
  res <- parseBaselineTableHeuristics(percentBlockPdf(), trial = "T",
                                      quiet = TRUE)
  d <- res$data
  expect_identical(res$arms$N, c(20L, 25L))

  # "Male sex 55% / 52%" -> 11 of 20, 13 of 25, with complements
  male <- d[grepl("^Male", d$ROW), ]
  expect_equal(male[["Male sex"]], c(11, 13))
  expect_equal(male[["Not Male sex"]], c(9, 12))

  # "Diabetes, % 25 / 20" -> 5 of 20, 5 of 25 (label announces percent)
  dia <- d[grepl("^Diabetes", d$ROW), ]
  expect_equal(dia[["Diabetes"]], c(5, 5))

  # "ASA status, %" children "I 45/40", "II 55/60" -> 9/10 and 11/15
  # (child columns keep their printed labels, as count children do)
  asa <- d[grepl("^ASA", d$ROW), ]
  expect_equal(asa[["I"]], c(9, 10))
  expect_equal(asa[["II"]], c(11, 15))

  # every conversion is reported for review
  expect_true(length(res$derivedCounts) >= 3)
  flags <- reviewFlags(res)
  expect_true(any(grepl("converted from printed percentages", flags)))
})

test_that("derived cells are recorded for the grid, with their notes", {
  res <- parseBaselineTableHeuristics(percentBlockPdf(), trial = "T",
                                      quiet = TRUE)
  dc <- res$derivedCells
  expect_false(is.null(dc))
  expect_true(all(c("ROW", "COL", "KIND", "NOTE") %in% names(dc)))
  expect_true(all(dc$KIND == "unique"))
  # the binary row records both the count column and its complement
  expect_true(any(dc$COL == "Male sex"))
  expect_true(any(dc$COL == "Not Male sex"))
  expect_true(any(grepl("uniquely pinned", dc$NOTE)))
})

test_that("pctApprox = TRUE converts what the bracket cannot pin", {
  f  <- file.path(tempdir(), "pctapprox.pdf")
  vx <- c(300, 420)
  cells <- c(
    list(list(x = 72, y = 80,
              text = "Table 1. Baseline patient characteristics", adj = 0)),
    rowCells(110, "", c("Drug", "Placebo"), vx),
    rowCells(136, "", c("(n = 702)", "(n = 695)"), vx),
    rowCells(164, "Age (yr)",     c("55 ± 15", "61 ± 15"), vx),
    rowCells(190, "Male sex, %",  c("47",      "44"),      vx))
  makeTablePdf(f, cells)

  # off by default: refused (pinned by the earlier test); on: approximated
  res <- parseBaselineTableHeuristics(f, trial = "T", quiet = TRUE,
                                      pctApprox = TRUE)
  male <- res$data[grepl("^Male", res$data$ROW), ]
  expect_equal(male[["Male sex"]], c(round(702 * .47), round(695 * .44)))
  expect_equal(male[["Not Male sex"]], c(702 - 330, 695 - 306))
  expect_true("Male sex" %in% res$approxCounts)
  expect_true(any(res$derivedCells$KIND == "approximate"))
  expect_true(any(grepl("APPROXIMATE", res$derivedCells$NOTE)))
  flags <- reviewFlags(res)
  expect_true(any(grepl("APPROXIMATE counts", flags)))

  # exact conversions must never silently become approximations
  res2 <- parseBaselineTableHeuristics(percentBlockPdf(), trial = "T",
                                       quiet = TRUE, pctApprox = TRUE)
  expect_length(res2$approxCounts, 0)
  expect_true(all(res2$derivedCells$KIND == "unique"))
})

test_that("a percent that does not pin a unique count is refused", {
  f  <- file.path(tempdir(), "pctbig.pdf")
  vx <- c(300, 420)
  cells <- c(
    list(list(x = 72, y = 80,
              text = "Table 1. Baseline patient characteristics", adj = 0)),
    rowCells(110, "", c("Drug", "Placebo"), vx),
    rowCells(136, "", c("(n = 702)", "(n = 695)"), vx),
    rowCells(164, "Age (yr)",     c("55 ± 15", "61 ± 15"), vx),
    rowCells(190, "Male sex, %",  c("47",      "44"),      vx))
  makeTablePdf(f, cells)
  res <- parseBaselineTableHeuristics(f, trial = "T", quiet = TRUE)
  # the row must be skipped with its reason, not silently approximated
  expect_true(any(grepl("unique count", res$skipped$reason)))
  expect_false(any(grepl("^Male", res$data$ROW)))
  expect_length(res$derivedCounts, 0)
})

test_that("plain counts under a non-percent header stay counts", {
  f  <- file.path(tempdir(), "plaincat.pdf")
  vx <- c(300, 420)
  cells <- c(
    list(list(x = 72, y = 80,
              text = "Table 1. Baseline patient characteristics", adj = 0)),
    rowCells(110, "", c("Ketamine", "Saline"), vx),
    rowCells(136, "", c("(n = 20)", "(n = 25)"), vx),
    rowCells(164, "Age (yr)", c("45.3 ± 12.1", "46.1 ± 11.8"), vx),
    rowCells(190, "Type of surgery", c("", ""), vx),
    rowCells(216, "Abdominal", c("12", "15"), vx, labelX = 86),
    rowCells(242, "Urologic",  c("8",  "10"), vx, labelX = 86))
  makeTablePdf(f, cells)
  res <- parseBaselineTableHeuristics(f, trial = "T", quiet = TRUE)
  surg <- res$data[grepl("^Type of surgery", res$data$ROW), ]
  expect_equal(surg[["Abdominal"]], c(12, 15))   # counts, untouched
  expect_length(res$derivedCounts, 0)
})
