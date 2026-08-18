# The input contract: column-name normalization and category detection.
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-19,
# for the test-suite consolidation (ISSUES.md issue 4, first priority).
# These rules are the app's public interface - a spreadsheet written for
# any earlier version must keep validating identically - and were
# previously exercised only implicitly.
suppressWarnings(suppressPackageStartupMessages({
  library(shiny)
}))

vd <- function(d) shiny::isolate(validateData(d))

test_that("loose column names normalize: TRIAL, ROW, ROUND MEAN, OBS", {
  # The contract, as implemented: TRIAL and ROW are matched by CONTAINS
  # ("My Trial ID", "Row Number" work); MEAN itself must be named MEAN
  # (case/whitespace-insensitive) - an ADDITIONAL column containing
  # "MEAN" becomes ROUND_MEAN, and an "OBS"-containing column becomes
  # ROUND_OBSERVATION.
  # (not "Row Number" - NUMBER is a Carlisle alias for N and is matched
  # first, so that name would be swallowed as N)
  v <- vd(data.frame(
    "My Trial ID" = "A", "Row Label" = c("Age", "Age"),
    N = c(15, 17), MEAN = c(45.3, 46.1), SD = c(12.1, 11.8),
    "Round Mean" = 1, "Obs decimals" = 0,
    check.names = FALSE, stringsAsFactors = FALSE))
  expect_false(v$FAIL)
  expect_true(all(c("TRIAL", "ROW", "N", "MEAN", "SD", "ROUND_MEAN",
                    "ROUND_OBSERVATION") %in% names(v$DATA)))
  expect_identical(v$DATA$MEAN, c(45.3, 46.1))
  expect_identical(v$DATA$ROUND_MEAN, c(1, 1))
  expect_identical(as.character(v$DATA$TRIAL), c("A", "A"))
})

test_that("a missing required column fails gracefully - never crashes", {
  # found by this suite: the per-line checks used to run even after a
  # header-level failure, and "undefined columns selected" killed the
  # whole session. The bare-FAIL return is the server's guarded shape.
  for (drop in c("MEAN", "SD", "N")) {
    d <- data.frame(TRIAL = "T", ROW = c("Age", "Age"), N = c(15, 17),
                    MEAN = c(45.3, 46.1), SD = c(12.1, 11.8),
                    stringsAsFactors = FALSE)
    d[[drop]] <- NULL
    expect_no_error(v <- vd(d))
    expect_true(v$FAIL)
  }
})

test_that("column names are case-insensitive and whitespace-trimmed", {
  v <- vd(data.frame(
    " row " = c("Age", "Age"), n = c(15, 17), mean = c(45.3, 46.1),
    sd = c(12.1, 11.8), check.names = FALSE, stringsAsFactors = FALSE))
  expect_false(v$FAIL)
  expect_identical(v$DATA$N, c(15, 17))
})

test_that("a table with no TRIAL column becomes trial 1", {
  v <- vd(data.frame(ROW = c("Age", "Age"), N = c(15, 17),
                     MEAN = c(45, 46), SD = c(12, 11),
                     stringsAsFactors = FALSE))
  expect_false(v$FAIL)
  expect_identical(unique(v$DATA$TRIAL), 1)
})

test_that("Carlisle-2017 column aliases are accepted", {
  # MEASURE -> ROW (GROUP and DECSD dropped), DECM -> ROUND_MEAN,
  # NUMBER -> N
  v <- vd(data.frame(
    MEASURE = c("Age", "Age"), GROUP = c(1, 2), DECSD = c(1, 1),
    NUMBER = c(15, 17), MEAN = c(45.3, 46.1), SD = c(12.1, 11.8),
    DECM = c(1, 1), stringsAsFactors = FALSE))
  expect_false(v$FAIL)
  expect_identical(as.character(v$DATA$ROW), c("Age", "Age"))
  expect_identical(v$DATA$N, c(15, 17))
  expect_identical(v$DATA$ROUND_MEAN, c(1, 1))
  expect_false("GROUP" %in% names(v$DATA))
})

test_that("is_category: numeric, integer-valued, with at least one NA", {
  expect_true(is_category(c(10, 12, NA, NA)))
  expect_false(is_category(c("a", "b", NA)))         # text is never a category
  expect_false(is_category(c(10, 12, 14, 16)))       # no NA -> not a category
  expect_false(is_category(c(1.5, 2, NA)))           # non-integer values
  expect_false(is_category(c(NA_real_, NA_real_)))   # nothing but NA
})

test_that("SE, Q1/Q3, and ROUND_DISPERSION are never swallowed as categories", {
  # integer-valued columns with NAs that would satisfy is_category(), but
  # are recognized fields and must reach the validated frame intact
  v <- vd(data.frame(
    TRIAL = "T", ROW = c("Age", "Age", "Sex", "Sex"),
    N = c(15, 17, NA, NA), MEAN = c(45, 46, NA, NA),
    SD = c(12, 11, NA, NA), SE = c(3, NA, NA, NA),
    ROUND_DISPERSION = c(1, 1, NA, NA),
    MALE = c(NA, NA, 10, 12), stringsAsFactors = FALSE))
  expect_false(v$FAIL)
  expect_identical(v$CategoryNames, "MALE")
  expect_true(all(c("SE", "ROUND_DISPERSION") %in% names(v$DATA)))
})

test_that("unrecognized columns ride along as Misc, untouched", {
  v <- vd(data.frame(
    TRIAL = "T", ROW = c("Age", "Age"), N = c(15, 17),
    MEAN = c(45, 46), SD = c(12, 11),
    COMMENTS = c("first arm", "second arm"), stringsAsFactors = FALSE))
  expect_false(v$FAIL)
  expect_true("COMMENTS" %in% v$MiscNames)
  expect_identical(v$DATA$COMMENTS, c("first arm", "second arm"))
})
