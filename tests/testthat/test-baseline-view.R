# Issue 15: the journal-style reconstructed baseline table.
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-19,
# with R/baselineTable.R. The reconstruction is built from the VALIDATED
# data (validateData()$DATA), so every test round-trips through
# validateData() exactly as the app does.
suppressWarnings(suppressPackageStartupMessages({
  library(shiny)
}))

vd <- function(d) shiny::isolate(validateData(d))
build <- function(d) {
  v <- vd(d)
  stopifnot(!v$FAIL)
  buildBaselineTables(v$DATA, v$CategoryNames)
}

test_that("continuous variables print mean (SD) at the printed precision", {
  tabs <- build(data.frame(
    TRIAL = "T", ROW = rep(c("Age", "Weight"), each = 2),
    N = c(15, 17, 15, 17),
    MEAN = c(45.3, 46.1, 63, 68), SD = c(12.1, 11.8, 13, 12),
    ROUND_MEAN = c(1, 1, 0, 0), ROUND_OBSERVATION = 1,
    stringsAsFactors = FALSE))
  expect_length(tabs, 1)
  t1 <- tabs[["T"]]
  expect_identical(names(t1),
                   c("Variable", "Arm 1 (n = 15)", "Arm 2 (n = 17)"))
  age <- t1[t1$Variable == "Age, mean (SD)", ]
  expect_identical(age[[2]], "45.3 (12.1)")
  expect_identical(age[[3]], "46.1 (11.8)")
  wt <- t1[t1$Variable == "Weight, mean (SD)", ]
  expect_identical(wt[[2]], "63 (13)")
})

test_that("ROUND_DISPERSION controls the SD's decimals when present", {
  tabs <- build(data.frame(
    TRIAL = "T", ROW = c("Age", "Age"), N = c(20, 20),
    MEAN = c(39, 41), SD = c(4.06, 3.9),
    ROUND_MEAN = 0, ROUND_DISPERSION = c(2, 1), ROUND_OBSERVATION = 0,
    stringsAsFactors = FALSE))
  age <- tabs[["T"]][1, ]
  expect_identical(age[[2]], "39 (4.06)")   # the "39 (4.06)" case
  expect_identical(age[[3]], "41 (3.9)")
})

test_that("median/IQR variables print median [Q1, Q3] and say so", {
  tabs <- build(data.frame(
    TRIAL = "T", ROW = rep(c("Age", "Duration"), each = 2),
    N = c(15, 17, 15, 17),
    MEAN = c(45.3, 46.1, 127, 133), SD = c(12.1, 11.8, NA, NA),
    Q1 = c(NA, NA, 98, 101), Q3 = c(NA, NA, 160, 155),
    ROUND_MEAN = c(1, 1, 0, 0), ROUND_OBSERVATION = 1,
    stringsAsFactors = FALSE))
  t1 <- tabs[["T"]]
  dur <- t1[t1$Variable == "Duration, median [Q1, Q3]", ]
  expect_identical(nrow(dur), 1L)
  expect_identical(dur[[2]], "127 [98, 160]")
  # the mean/SD variable is unaffected by the trial having quartiles
  expect_identical(t1[t1$Variable == "Age, mean (SD)", ][[2]],
                   "45.3 (12.1)")
})

test_that("category variables become a header line plus indented counts", {
  tabs <- build(data.frame(
    TRIAL = "T", ROW = c("Age", "Age", "Sex", "Sex"),
    N = c(15, 17, NA, NA),
    MEAN = c(45.3, 46.1, NA, NA), SD = c(12.1, 11.8, NA, NA),
    MALE = c(NA, NA, 10, 12), FEMALE = c(NA, NA, 5, 5),
    ROUND_MEAN = 1, ROUND_OBSERVATION = 1,
    stringsAsFactors = FALSE))
  t1 <- tabs[["T"]]
  expect_true("Sex, n" %in% t1$Variable)
  male <- t1[t1$Variable == "    MALE", ]
  expect_identical(male[[2]], "10")
  expect_identical(male[[3]], "12")
  # the header line itself carries no numbers
  expect_identical(t1[t1$Variable == "Sex, n", ][[2]], "")
})

test_that("a line whose N differs from the arm's header N says so", {
  tabs <- build(data.frame(
    TRIAL = "T", ROW = rep(c("Age", "Weight", "Height"), each = 2),
    N = c(15, 17, 15, 17, 14, 17),
    MEAN = c(45.3, 46.1, 63, 68, 165, 167),
    SD = c(12.1, 11.8, 13, 12, 7, 7),
    ROUND_MEAN = c(1, 1, 0, 0, 0, 0), ROUND_OBSERVATION = 1,
    stringsAsFactors = FALSE))
  t1 <- tabs[["T"]]
  expect_identical(names(t1)[2], "Arm 1 (n = 15)")
  ht <- t1[t1$Variable == "Height, mean (SD)", ]
  expect_identical(ht[[2]], "165 (7); n = 14")
  expect_identical(ht[[3]], "167 (7)")
})

test_that("trials split into separate tables, and sheets, with clean names", {
  d <- data.frame(
    TRIAL = rep(c("A: study/1", "B"), each = 2),
    ROW = "Age", N = 20, MEAN = c(45.3, 46.1, 50.2, 49.8),
    SD = c(12.1, 11.8, 9.0, 9.2),
    ROUND_MEAN = 1, ROUND_OBSERVATION = 1, stringsAsFactors = FALSE)
  tabs <- build(d)
  expect_identical(names(tabs), c("A: study/1", "B"))
  f <- tempfile(fileext = ".xlsx")
  sheets <- writeBaselineTablesXlsx(tabs, f)
  expect_true(file.exists(f))
  expect_length(sheets, 2)
  expect_false(any(grepl("[:/]", sheets)))   # sanitized
  back <- openxlsx::read.xlsx(f, sheet = sheets[2])
  expect_identical(back[["Arm.1.(n.=.20)"]][1], "50.2 (9.0)")
})

test_that("the download button appears on validation success and hides after", {
  shiny::testServer(app_server, {
    good <- data.frame(
      TRIAL = "T", ROW = c("Age", "Age"), N = c(15, 17),
      MEAN = c(45.3, 46.1), SD = c(12.1, 11.8),
      ROUND_MEAN = 1, ROUND_OBSERVATION = 1, stringsAsFactors = FALSE)
    session$setInputs(dataGrid = good, applyEdits = 1)
    expect_false(is.null(reactiveDataValidated()))
    expect_match(as.character(output$journalButton$html), "journalTable")
    # a fresh blank table is not validated - the button must hide
    session$setInputs(blank = 1)
    expect_null(reactiveDataValidated())
  })
})
