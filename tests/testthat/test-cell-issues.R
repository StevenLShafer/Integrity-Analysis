# Issue 13: color-coded grid cells for validation problems.
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-18,
# with the validateData()/app_server changes on branch cell-colors.
# validateData() now returns an `issues` data frame (row, col, code) with
# codes "missing" (yellow), "unreadable" (red - text where a number
# belongs), "incongruent" (blue - value contradicts the row's type); the
# same codes as docs/api-spec.md issues[]. The server publishes them to
# the grid renderer as instance.params.cellIssues keyed "row|col"
# (0-based).
suppressWarnings(suppressPackageStartupMessages({
  library(shiny)
}))

# minimal valid continuous frame builders
contFrame <- function(...) data.frame(
  TRIAL = "T", ..., ROUND_MEAN = 1, ROUND_OBSERVATION = 1,
  stringsAsFactors = FALSE)

vd <- function(d) shiny::isolate(validateData(d))

test_that("a missing required value is filed as 'missing' and DATA returns", {
  v <- vd(contFrame(ROW = c("Age", "Weight"), N = c(20, 20),
                    MEAN = c(50, 70), SD = c(9, NA)))
  expect_true(v$FAIL)
  # the normalized frame comes back so the grid can display what the
  # issue rows index into
  expect_false(is.null(v$DATA))
  expect_true(any(v$issues$row == 2 & v$issues$col == "SD" &
                  v$issues$code == "missing"))
  # nothing flagged on the complete row
  expect_false(any(v$issues$row == 1))
})

test_that("text where a number belongs is 'unreadable', not 'missing'", {
  v <- vd(contFrame(ROW = c("Age", "Weight"), N = c(20, 20),
                    MEAN = c(50, 70), SD = c("9", "n/a")))
  expect_true(v$FAIL)
  hit <- v$issues[v$issues$row == 2 & v$issues$col == "SD", ]
  expect_identical(hit$code, "unreadable")   # exactly one code, the red one
})

test_that("quartiles alongside SD/SE are 'incongruent' on the SD/SE cells", {
  v <- vd(contFrame(ROW = "Age", N = 20, MEAN = 50, SD = 9,
                    Q1 = 45, Q3 = 55))
  expect_true(v$FAIL)
  expect_true(any(v$issues$row == 1 & v$issues$col == "SD" &
                  v$issues$code == "incongruent"))
})

test_that("a median outside its quartiles paints MEAN, Q1, and Q3", {
  v <- vd(contFrame(ROW = "Age", N = 20, MEAN = 60, SD = NA_real_,
                    Q1 = 45, Q3 = 55))
  expect_true(v$FAIL)
  for (cn in c("MEAN", "Q1", "Q3"))
    expect_true(any(v$issues$row == 1 & v$issues$col == cn &
                    v$issues$code == "incongruent"))
})

test_that("continuous entries on a category row are 'incongruent'", {
  v <- vd(contFrame(ROW = c("Sex", "Age"), N = c(20, 20),
                    MEAN = c(NA, 50), SD = c(NA, 9), MALE = c(12, NA)))
  expect_true(v$FAIL)
  expect_true(any(v$issues$row == 1 & v$issues$col == "N" &
                  v$issues$code == "incongruent"))
})

test_that("non-integer values in a would-be category column are a SOFT warning", {
  # EXTRA is numeric with an NA - is_category() material except for the
  # 1.5 - so the column falls to Misc and the offending cell is painted,
  # but validation succeeds
  v <- vd(contFrame(ROW = c("Age", "Weight"), N = c(20, 20),
                    MEAN = c(50, 70), SD = c(9, 8), EXTRA = c(1.5, NA)))
  expect_false(v$FAIL)
  expect_true("EXTRA" %in% v$MiscNames)
  expect_true(any(v$issues$row == 1 & v$issues$col == "EXTRA" &
                  v$issues$code == "incongruent"))
})

test_that("a fully valid table returns NULL issues", {
  v <- vd(contFrame(ROW = c("Age", "Weight"), N = c(20, 20),
                    MEAN = c(50, 70), SD = c(9, 8)))
  expect_false(v$FAIL)
  expect_null(v$issues)
})

test_that("the widget carries cellIssues into instance.params", {
  w <- rhandsontable::rhandsontable(
    data.frame(a = 1, b = 2),
    cellIssues = list("0|1" = "missing"))
  expect_identical(w$x$cellIssues[["0|1"]], "missing")
})

test_that("the server paints on validation failure and clears on new input", {
  shiny::testServer(app_server, {
    bad <- data.frame(
      TRIAL = "T", ROW = c("Age", "Weight"), N = c(20, 20),
      MEAN = c(50, 70), SD = c(9, NA), ROUND_MEAN = 1,
      ROUND_OBSERVATION = 1, stringsAsFactors = FALSE)
    # the applyEdits test seam accepts a bare data.frame for
    # input$dataGrid (real clients send the widget payload)
    session$setInputs(dataGrid = bad, applyEdits = 1)
    expect_false(is.null(rIssues()))
    expect_true(any(rIssues()$code == "missing"))
    # the FAIL path pushes the normalized frame back into the grid so
    # the colors index the frame on screen
    expect_identical(nrow(reactiveData()), nrow(bad))
    # the widget JSON for the grid carries the payload, 0-based "row|col"
    json <- output$dataGrid
    expect_match(json, "cellIssues")
    # blank-table entry clears stale colors
    session$setInputs(blank = 1)
    expect_null(rIssues())
  })
})
