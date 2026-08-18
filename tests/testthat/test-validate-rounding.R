# Empty rounding columns must not crash validation.
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-19,
# with the fix in R/validateData.R. Found live during PR #22 testing:
# the blank-table starter ships ROUND_MEAN / ROUND_OBSERVATION as empty
# (all-NA) columns, and typing a DECIMAL mean then hit
# `if (DATA$ROUND_MEAN[i] < digits)` with NA - a fatal error that killed
# the whole Shiny session. An empty rounding cell means "infer it": NAs
# fill with 0 and the decimal bump raises them to the typed precision.
suppressWarnings(suppressPackageStartupMessages({
  library(shiny)
}))

vd <- function(d) shiny::isolate(validateData(d))

blankStyle <- function(MEAN, ROUND_MEAN = NA_real_) data.frame(
  TRIAL = "T", ROW = "Age", N = c(15, 17), MEAN = MEAN,
  SD = c(12, 11), SE = NA_real_,
  ROUND_MEAN = ROUND_MEAN, ROUND_DISPERSION = NA_real_,
  ROUND_OBSERVATION = NA_real_, stringsAsFactors = FALSE)

test_that("a decimal mean with an empty ROUND_MEAN validates (no crash)", {
  v <- vd(blankStyle(MEAN = c(45.3, 46.1)))
  expect_false(v$FAIL)
  # the inference: NA -> 0, then bumped to the typed precision
  expect_identical(v$DATA$ROUND_MEAN, c(1, 1))
  expect_identical(v$DATA$ROUND_OBSERVATION, c(0, 0))
})

test_that("integer means with empty rounding columns stay at 0", {
  v <- vd(blankStyle(MEAN = c(45, 46)))
  expect_false(v$FAIL)
  expect_identical(v$DATA$ROUND_MEAN, c(0, 0))
})

test_that("text in a rounding column is coerced, not fatal", {
  v <- vd(blankStyle(MEAN = c(45.3, 46.1),
                     ROUND_MEAN = c("one", "1")))
  expect_false(v$FAIL)
  # "one" -> NA -> 0 -> bumped to 1; "1" survives
  expect_identical(v$DATA$ROUND_MEAN, c(1, 1))
})

test_that("explicit rounding values are respected (no regression)", {
  v <- vd(blankStyle(MEAN = c(45.3, 46.1), ROUND_MEAN = c(2, 2)))
  expect_false(v$FAIL)
  expect_identical(v$DATA$ROUND_MEAN, c(2, 2))
})
