# Known-answer Monte Carlo cases under fixed seeds (ISSUES.md issue 4,
# second priority): a refactor must not silently change results.
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-19.
# The pinned values were computed on R 4.5.3 / dqrng 0.4.1 with the
# engine as of the PR 21-25 merges. Under fixed seeds the engine is
# fully deterministic (sequential %do% row loop, chunking depends only
# on N), so equality is exact. If an INTENDED statistical change moves
# these numbers, re-pin them in the same commit and say why; if an
# upgrade of R or dqrng moves them, re-pin and note the version. A
# surprise failure here means the engine changed when it was not meant
# to.
suppressWarnings(suppressPackageStartupMessages({
  library(shiny); library(foreach); library(MBESS); library(Rfast)
  library(dqrng)
}))

runP <- function(d, cats = NULL, m = 10000) {
  dqrng::dqset.seed(42); set.seed(42)
  suppressWarnings(shiny::isolate(P_Calc("T", d, cats, m)))
}
summaryP <- function(x)
  suppressWarnings(as.numeric(x$P[x$ROW == "Summary" & !is.na(x$ROW)]))[1]

test_that("the documentation's worked example: 77 vs 78, SD 30, n = 6", {
  x <- runP(data.frame(
    TRIAL = "T", ROW = "Weight", N = 6, MEAN = c(77, 78), SD = c(30, 30),
    ROUND_MEAN = 0, ROUND_OBSERVATION = 0, stringsAsFactors = FALSE))
  expect_equal(summaryP(x), 0.0495)         # the guide's "about 4%"
  expect_identical(x$M[1], "1000")          # unremarkable - stops at stage 1
})

test_that("three identical arms escalate and alarm", {
  d <- data.frame(TRIAL = "T", ROW = "X", N = 40, MEAN = 54.1, SD = 9.2,
                  ROUND_MEAN = 1, ROUND_OBSERVATION = 1,
                  stringsAsFactors = FALSE)[rep(1, 3), ]
  x <- runP(d)
  expect_equal(summaryP(x), 3e-04)
  expect_identical(x$M[1], "10000")         # escalated past stage 1
  expect_identical(x$CI95[1], "<=0.0013")   # explicit Monte Carlo bound
})

test_that("identical categorical arms give the pinned lower-tail mid-p", {
  x <- runP(data.frame(
    TRIAL = "T", ROW = "Sex", N = NA_real_, MEAN = NA_real_,
    SD = NA_real_, ROUND_MEAN = NA_real_, ROUND_OBSERVATION = NA_real_,
    MALE = c(25, 25), FEMALE = c(25, 25), stringsAsFactors = FALSE),
    cats = c("MALE", "FEMALE"))
  expect_equal(summaryP(x), 0.089)
})

test_that("a median/IQR pair gives the pinned metalog p", {
  x <- runP(data.frame(
    TRIAL = "T", ROW = "Dur", N = c(20, 20), MEAN = c(127, 128),
    SD = NA_real_, Q1 = c(98, 99), Q3 = c(160, 161),
    ROUND_MEAN = 0, ROUND_OBSERVATION = 0, stringsAsFactors = FALSE))
  expect_equal(summaryP(x), 0.0555)
})

test_that("categorical direction: homogeneous alarms, heterogeneous does not", {
  # ported from the issue-6 direction check (scratch catdir_test.R):
  # under the old chisq.test upper-tail convention these were REVERSED
  p_of <- function(male) {
    d <- data.frame(TRIAL = "T", ROW = "Sex", N = NA_real_,
                    MEAN = NA_real_, SD = NA_real_, ROUND_MEAN = NA_real_,
                    ROUND_OBSERVATION = NA_real_,
                    MALE = male, FEMALE = 50 - male,
                    stringsAsFactors = FALSE)
    summaryP(runP(d, cats = c("MALE", "FEMALE"), m = 15000))
  }
  expect_lt(p_of(c(25, 25)), 0.35)   # identical arms - suspicious
  expect_gt(p_of(c(40, 10)), 0.9)    # strongly imbalanced - not the signal
})
