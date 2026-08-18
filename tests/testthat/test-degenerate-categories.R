# Degenerate categorical tables must be refused with a message, not crash
# the whole trial (found by the corpus/TEST mass run, 2026-08-17: a
# category cell NA in one arm, or a zero-margin column, made the
# chi-square statistic NaN and if (P == 1) fatal).
suppressWarnings(suppressPackageStartupMessages({
  library(shiny); library(foreach); library(MBESS); library(Rfast)
  library(dqrng)
}))

catrow <- function(a, b) data.frame(
  TRIAL = "T", ROW = "Cat",
  N = NA_real_, MEAN = NA_real_, SD = NA_real_,
  ROUND_MEAN = NA_real_, ROUND_OBSERVATION = NA_real_,
  A = a, B = b, stringsAsFactors = FALSE)

test_that("NA category cell refuses the row instead of crashing", {
  d <- rbind(catrow(10, 5), catrow(NA, 7))
  x <- suppressWarnings(shiny::isolate(P_Calc("T", d, c("A", "B"), 500)))
  expect_true(any(grepl("Incomplete category", x$P)))
})

test_that("zero-margin category column is dropped; fully degenerate refused", {
  # B sums to zero -> dropped -> only one column left -> refused
  d <- rbind(catrow(10, 0), catrow(12, 0))
  x <- suppressWarnings(shiny::isolate(P_Calc("T", d, c("A", "B"), 500)))
  expect_true(any(grepl("Degenerate category", x$P)))
})

test_that("healthy categorical rows still analyze", {
  dqrng::dqset.seed(9); set.seed(9)
  d <- rbind(catrow(10, 15), catrow(12, 13))
  x <- suppressWarnings(shiny::isolate(P_Calc("T", d, c("A", "B"), 2000)))
  p <- suppressWarnings(as.numeric(x$P[x$ROW == "Summary" & !is.na(x$ROW)]))
  expect_true(!is.na(p) && p > 0 && p < 1)
})
