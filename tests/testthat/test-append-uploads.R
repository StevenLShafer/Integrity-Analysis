# Sequential uploads append to the existing table (Steve, 2026-08-19).
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-19,
# with the upload-observer change in R/app_server.R. Previously each
# upload REPLACED the table; combining only happened across files
# selected together. Now the current table (including unrevalidated grid
# edits, via currentGrid()) is seeded as the first frame of every upload,
# so the existing column-union and trial-disambiguation machinery appends
# instead. Start With an Empty Table remains the reset.
#
# Uploads are staged as COPIES in per-file subdirectories of tempdir(),
# mimicking real clients - never hand a real file's path to the server
# (the purge-on-exit handler deletes uploaded paths and their parent
# directories).
suppressWarnings(suppressPackageStartupMessages({
  library(shiny)
}))

stageCsv <- function(df) {
  d <- file.path(tempdir(), paste0("up", basename(tempfile(""))))
  dir.create(d)
  f <- file.path(d, "data.csv")
  write.csv(df, f, row.names = FALSE)
  f
}

contRows <- function(trial = NULL, row = "Age") {
  d <- data.frame(ROW = row, N = c(15, 17), MEAN = c(45, 46),
                  SD = c(12, 11), ROUND_MEAN = 0, ROUND_OBSERVATION = 0,
                  stringsAsFactors = FALSE)
  if (!is.null(trial)) d <- cbind(TRIAL = trial, d)
  d
}

test_that("a second upload appends instead of replacing", {
  shiny::testServer(app_server, {
    session$setInputs(upload = data.frame(
      name = "a.csv", datapath = stageCsv(contRows(trial = "1")),
      stringsAsFactors = FALSE))
    expect_identical(nrow(reactiveData()), 2L)

    # second upload, no TRIAL column: appends under its file stem
    session$setInputs(upload = data.frame(
      name = "b.csv", datapath = stageCsv(contRows(row = "Weight")),
      stringsAsFactors = FALSE))
    d <- reactiveData()
    expect_identical(nrow(d), 4L)
    expect_setequal(unique(as.character(d$TRIAL)), c("1", "b"))

    # third upload whose trial CLASHES with an existing one gets the
    # filename prefix - the existing table's trials are never renamed
    session$setInputs(upload = data.frame(
      name = "c.csv",
      datapath = stageCsv(contRows(trial = "1", row = "Height")),
      stringsAsFactors = FALSE))
    d <- reactiveData()
    expect_identical(nrow(d), 6L)
    expect_setequal(unique(as.character(d$TRIAL)), c("1", "b", "c: 1"))

    # Start With an Empty Table is the reset
    session$setInputs(blank = 1)
    expect_identical(nrow(reactiveData()), 8L)
  })
})

test_that("a failed upload leaves the existing table untouched", {
  shiny::testServer(app_server, {
    session$setInputs(upload = data.frame(
      name = "a.csv", datapath = stageCsv(contRows(trial = "1")),
      stringsAsFactors = FALSE))
    expect_identical(nrow(reactiveData()), 2L)

    bad <- file.path(tempdir(), paste0("up", basename(tempfile(""))))
    dir.create(bad)
    badf <- file.path(bad, "broken.csv")
    writeLines("", badf)
    session$setInputs(upload = data.frame(
      name = "broken.csv", datapath = badf, stringsAsFactors = FALSE))
    expect_identical(nrow(reactiveData()), 2L)
  })
})
