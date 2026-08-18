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

test_that("an SE beside a missing SD paints the SE incongruent too", {
  v <- vd(contFrame(ROW = "Age", N = 20, MEAN = 50, SD = NA_real_,
                    SE = 1.4))
  expect_true(v$FAIL)
  expect_true(any(v$issues$row == 1 & v$issues$col == "SD" &
                  v$issues$code == "missing"))
  expect_true(any(v$issues$row == 1 & v$issues$col == "SE" &
                  v$issues$code == "incongruent"))
})

test_that("label-only rows are soft-flagged and excluded from analysis", {
  # a ROW name with no data anywhere: what a parser-skipped table line
  # looks like once surfaced in the grid - painted, non-blocking,
  # excluded from the analyzed data
  v <- vd(contFrame(ROW = c("Age", "Pain score, median [range]"),
                    N = c(20, NA), MEAN = c(50, NA), SD = c(9, NA)))
  expect_false(v$FAIL)
  expect_true(any(v$issues$row == 2 & v$issues$col == "SD" &
                  v$issues$code == "missing"))
  expect_identical(v$DATA$ROW, "Age")   # excluded from the analyzed frame

  # ... but a table that is ONLY labels has nothing to analyze: failure
  v2 <- vd(contFrame(ROW = c("Age", "Weight"),
                     N = NA_real_, MEAN = NA_real_, SD = NA_real_))
  expect_true(v2$FAIL)
})

test_that("a single-line categorical variable is soft-flagged and excluded", {
  # Steve's Test4 report (2026-08-19): a misparsed footnote fragment
  # became a "variable" with one stray count in a junk category column -
  # a valid-looking, unflagged, unanalyzable row. One categorical line
  # is one arm; there is nothing to compare it against.
  v <- vd(contFrame(ROW = c("Age", "Age", "Use of PCA"),
                    N = c(15, 17, NA), MEAN = c(45.3, 46.1, NA),
                    SD = c(12.1, 11.8, NA), PCA = c(NA, NA, 1)))
  expect_false(v$FAIL)
  hit <- v$issues[v$issues$row == 3 & v$issues$col == "ROW", ]
  expect_identical(hit$code, "missing")
  expect_match(hit$note, "two arms")   # the cell-specific hover text
  expect_false("Use of PCA" %in% v$DATA$ROW)   # left out of the analysis

  # a category with counts in two arms is untouched
  v2 <- vd(contFrame(ROW = c("Age", "Age", "Sex", "Sex"),
                     N = c(15, 17, NA, NA), MEAN = c(45.3, 46.1, NA, NA),
                     SD = c(12.1, 11.8, NA, NA), MALE = c(NA, NA, 10, 12)))
  expect_false(v2$FAIL)
  expect_null(v2$issues)

  # a table that is ONLY unanalyzable lines fails
  v3 <- vd(contFrame(ROW = "Use of PCA", N = NA_real_, MEAN = NA_real_,
                     SD = NA_real_, PCA = c(1)))
  expect_true(v3$FAIL)
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

test_that("a parser-skipped table line becomes a painted grid row", {
  pdfPath <- syntheticPdfMeanSD()
  shiny::testServer(app_server, {
    session$setInputs(upload = data.frame(
      name = "meanSD.pdf", datapath = pdfPath, stringsAsFactors = FALSE))
    d <- reactiveData()
    # the skipped median [range] line is now a grid row: its label in
    # ROW, no data anywhere
    i <- which(grepl("Duration", d$ROW))
    expect_length(i, 1)
    expect_true(all(is.na(d[i, intersect(c("N", "MEAN", "SD"),
                                         names(d))])))
    # registered for painting, with the parser's reason preserved
    sk <- parseSkips()
    expect_false(is.null(sk))
    expect_true(any(grepl("Duration", sk$ROW)))
    expect_match(sk$reason[grepl("Duration", sk$ROW)][1], "median",
                 ignore.case = TRUE)
    # soft warning: required cells yellow, but validation passes and the
    # Analyze button path stays open
    expect_true(any(rIssues()$code == "missing"))
    # the widget carries the red ROW cell for the skipped line
    expect_match(output$dataGrid, "unreadable")
  })
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
