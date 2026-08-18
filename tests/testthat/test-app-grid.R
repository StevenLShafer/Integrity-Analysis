# Grid mechanics: structural editing, display formats, and the
# spreadsheet round trip (ISSUES.md issue 4).
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-19,
# consolidating the scratch harnesses gridtest / structtest / fmttest /
# multitest into the permanent suite.
suppressWarnings(suppressPackageStartupMessages({
  library(shiny); library(openxlsx); library(readxl); library(Rfast)
  library(foreach); library(MBESS); library(dqrng)
}))

stageCopy <- function(src) {
  d <- file.path(tempdir(), paste0("up", basename(tempfile(""))))
  dir.create(d)
  f <- file.path(d, basename(src))
  file.copy(src, f)
  f
}

test_that("Add 5 Rows and Add Column preserve data, classes, and refuse dupes", {
  shiny::testServer(app_server, {
    session$setInputs(blank = 1)
    d0 <- reactiveData()
    expect_identical(nrow(d0), 8L)

    # typed values must survive a structural change, silently
    d0$ROW[1] <- "Age"; d0$N[1] <- 25
    session$setInputs(dataGrid = d0, addRows = 1)
    d1 <- reactiveData()
    expect_identical(nrow(d1), 13L)
    expect_identical(d1$ROW[1], "Age")
    expect_true(is.numeric(d1$N) && is.character(d1$ROW))
    expect_null(reactiveDataValidated())   # no validation spam

    session$setInputs(dataGrid = d1, newColName = "PAIN SCORE", addCol = 1)
    d2 <- reactiveData()
    expect_true("PAIN SCORE" %in% names(d2))
    expect_true(is.numeric(d2[["PAIN SCORE"]]))
    # duplicate name (any case) refused
    session$setInputs(dataGrid = d2, newColName = "pain score", addCol = 2)
    expect_identical(sum(toupper(names(reactiveData())) == "PAIN SCORE"), 1L)

    # the grown structure analyzes end to end once filled in
    d3 <- reactiveData()
    d3$ROW[1:2] <- "Age"; d3$N[1:2] <- 25
    d3$MEAN[1:2] <- c(54.1, 53.8); d3$SD[1:2] <- c(9.2, 8.9)
    d3$ROUND_MEAN[1:2] <- 1; d3$ROUND_OBSERVATION[1:2] <- 1
    d3$ROW[3:4] <- "Sex"; d3$CAT1[3:4] <- c(12, 13); d3$CAT2[3:4] <- c(13, 12)
    session$setInputs(dataGrid = d3, applyEdits = 1)
    expect_false(is.null(reactiveDataValidated()))
    session$setInputs(go = 1)
    expect_true(any(session$env$OUTPUT$ROW == "Summary", na.rm = TRUE))
  })
})

test_that("grid formats: counts as integers, measurements as typed", {
  ex <- stageCopy(system.file("extdata", "Example.xlsx",
                              package = "IntegrityAnalysis"))
  shiny::testServer(app_server, {
    session$setInputs(upload = data.frame(
      name = "Example.xlsx", datapath = ex, stringsAsFactors = FALSE))
    g <- jsonlite::fromJSON(output$dataGrid, simplifyVector = FALSE)
    fmt <- lapply(g$x$columns, function(cl)
      if (is.null(cl$numericFormat$pattern)) NA_character_
      else cl$numericFormat$pattern)
    names(fmt) <- unlist(g$x$rColHeaders)
    expect_identical(fmt[["N"]], "0")
    expect_identical(fmt[["Male"]], "0")
    expect_identical(fmt[["MEAN"]], "0.[00000]")
    expect_identical(fmt[["SD"]], "0.[00000]")
  })
})

test_that("Download Table round-trips: the saved table re-imports unchanged", {
  ex <- stageCopy(system.file("extdata", "Example.xlsx",
                              package = "IntegrityAnalysis"))
  saved <- NULL
  firstP <- NULL
  shiny::testServer(app_server, {
    session$setInputs(upload = data.frame(
      name = "Example.xlsx", datapath = ex, stringsAsFactors = FALSE))
    d1 <- reactiveData()
    # what the Download Table button writes
    f <- tempfile(fileext = ".xlsx")
    write.xlsx(d1, f, keepNA = FALSE)
    saved <<- f
    firstP <<- reactiveDataValidated()
  })
  reup <- stageCopy(saved)
  shiny::testServer(app_server, {
    session$setInputs(upload = data.frame(
      name = "resaved.xlsx", datapath = reup, stringsAsFactors = FALSE))
    expect_false(is.null(reactiveDataValidated()))
    v <- reactiveDataValidated()
    # the validated frames agree - same rows, same values where defined
    expect_identical(dim(v), dim(firstP))
    expect_equal(v$MEAN, firstP$MEAN)
    expect_equal(v$N, firstP$N)
    expect_identical(as.character(v$ROW), as.character(firstP$ROW))
  })
})

test_that("several files in ONE selection combine with trial disambiguation", {
  ex <- system.file("extdata", "Example.xlsx",
                    package = "IntegrityAnalysis")
  a <- stageCopy(ex)
  b <- stageCopy(ex)
  shiny::testServer(app_server, {
    session$setInputs(upload = data.frame(
      name = c("Example.xlsx", "Copy.xlsx"), datapath = c(a, b),
      stringsAsFactors = FALSE))
    d <- reactiveData()
    tr <- unique(as.character(d$TRIAL))
    # the second file's clashing trials carry its file-name prefix
    expect_true(any(grepl("^Copy: ", tr)))
    expect_true(length(tr) %% 2 == 0)   # every trial twice, disambiguated
  })
})

test_that("degenerate continuous rows do not crash the engine", {
  runP <- function(d, m = 1000) {
    dqrng::dqset.seed(7); set.seed(7)
    suppressWarnings(shiny::isolate(P_Calc("T", d, NULL, m)))
  }
  base <- function(MEAN, SD, N) data.frame(
    TRIAL = "T", ROW = "X", N = N, MEAN = MEAN, SD = SD,
    ROUND_MEAN = 1, ROUND_OBSERVATION = 1, stringsAsFactors = FALSE)
  # zero-SD arms: every simulated subject identical - must not error
  expect_no_error(x <- runP(base(c(50, 50), c(0, 0), c(10, 10))))
  expect_true("Summary" %in% x$ROW)
  # an arm of N = 1: the SD bias correction has no leverage - no crash
  expect_no_error(x <- runP(base(c(50, 51), c(9, 9), c(1, 30))))
  expect_true("Summary" %in% x$ROW)
  # a single arm: nothing to compare - must not error
  expect_no_error(x <- runP(base(50, 9, 20)))
  expect_true("Summary" %in% x$ROW)
})
