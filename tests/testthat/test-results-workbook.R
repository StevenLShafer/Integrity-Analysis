# The three-tab results workbook (Steve's design, 2026-08-19):
# Test Results / Baseline Tables / Summary.
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-19,
# with writeResultsWorkbook() in R/baselineTable.R.
suppressWarnings(suppressPackageStartupMessages({
  library(shiny); library(openxlsx); library(Rfast); library(foreach)
  library(MBESS); library(dqrng)
}))

test_that("the results download carries all three tabs, correctly filled", {
  # a two-trial table: trial A continuous, trial B continuous + category
  d <- data.frame(
    TRIAL = c("A", "A", "B", "B", "B", "B"),
    ROW = c("Age", "Age", "Weight", "Weight", "Sex", "Sex"),
    N = c(15, 17, 20, 20, NA, NA),
    MEAN = c(45.3, 46.1, 70, 72, NA, NA),
    SD = c(12.1, 11.8, 10, 11, NA, NA),
    MALE = c(NA, NA, NA, NA, 12, 8),
    ROUND_MEAN = 1, ROUND_OBSERVATION = 1, stringsAsFactors = FALSE)
  v <- shiny::isolate(validateData(d))
  expect_false(v$FAIL)

  # accumulate results the way the server does: one P_Calc per trial
  dqrng::dqset.seed(42); set.seed(42)
  OUTPUT <- NULL
  for (tr in v$TRIALS) {
    x <- suppressWarnings(shiny::isolate(
      P_Calc(tr, v$DATA[v$DATA$TRIAL == tr, ], v$CategoryNames, 1000)))
    OUTPUT <- rbind(OUTPUT, x)
  }

  f <- tempfile(fileext = ".xlsx")
  writeResultsWorkbook(OUTPUT, v$DATA, v$CategoryNames, f)
  expect_identical(openxlsx::getSheetNames(f),
                   c("Test Results", "Baseline Tables", "Summary"))

  # tab 1: the sheet as it always was
  t1 <- openxlsx::read.xlsx(f, sheet = "Test Results")
  expect_identical(names(t1)[3], "P.(one-sided.toward.homogeneity)")
  expect_true("Summary" %in% t1$ROW)

  # tab 2: journal-style reconstructions with trial headers
  t2 <- openxlsx::read.xlsx(f, sheet = "Baseline Tables",
                            colNames = FALSE, skipEmptyRows = FALSE)
  flat <- unlist(t2)
  expect_true("Trial: A" %in% flat)
  expect_true("Trial: B" %in% flat)
  expect_true("Age, mean (SD)" %in% flat)
  expect_true("45.3 (12.1)" %in% flat)
  expect_true("Sex, n" %in% flat)

  # tab 3: one line per study, closed by the overall Stouffer row
  # (Steve's request, 2026-08-20 - the Carlisle-on-Fujii step)
  t3 <- openxlsx::read.xlsx(f, sheet = "Summary")
  expect_identical(nrow(t3), 3L)
  expect_identical(t3$TRIAL[1:2], c("A", "B"))
  expect_match(t3$TRIAL[3], "^ALL 2 TRIALS [(]Stouffer")
  p <- suppressWarnings(as.numeric(t3[[2]]))
  expect_true(all(!is.na(p)) && all(p > 0 & p < 1))
  # the overall row IS the Stouffer combination of the trial rows
  expect_equal(p[3], signif(sumz(p[1:2])$p, 4), tolerance = 1e-3)
  expect_identical(names(t3)[3], "95%.Monte.Carlo.interval")
})

test_that("a single trial gets no overall row", {
  d <- data.frame(
    TRIAL = "A", ROW = c("Age", "Age"), N = c(15, 17),
    MEAN = c(45.3, 46.1), SD = c(12.1, 11.8),
    ROUND_MEAN = 1, ROUND_OBSERVATION = 1, stringsAsFactors = FALSE)
  v <- shiny::isolate(validateData(d))
  dqrng::dqset.seed(7); set.seed(7)
  OUTPUT <- suppressWarnings(shiny::isolate(
    P_Calc("A", v$DATA, v$CategoryNames, 1000)))
  f <- tempfile(fileext = ".xlsx")
  writeResultsWorkbook(OUTPUT, v$DATA, v$CategoryNames, f)
  t3 <- openxlsx::read.xlsx(f, sheet = "Summary")
  expect_identical(nrow(t3), 1L)
  expect_false(any(grepl("ALL", t3$TRIAL)))
})
