# test-distribution-graphs.R - the issue-16 PowerPoint deck and the
# collector that feeds it.
#
# PROVENANCE: written by Claude Code (model Claude Opus 5), 2026-08-20.
suppressWarnings(suppressPackageStartupMessages({
  library(shiny); library(openxlsx); library(Rfast); library(foreach)
  library(MBESS); library(dqrng)
}))

twoTrials <- function() {
  # Trial A is honest; trial B is three arms with IDENTICAL means on
  # both variables - observed squared error exactly 0, which almost no
  # honest replicate ties (three arm means all rounding equal), so each
  # row's p sits at the Davison-Hinkley floor, well under the 0.01
  # slide cutoff. Two arms were not enough: two means tie after
  # rounding ~2% of the time, putting p right AT the cutoff.
  data.frame(
    TRIAL = c("A", "A", "A", "A", rep("B", 6)),
    ROW = c("Age", "Age", "Weight", "Weight",
            rep("Age", 3), rep("Weight", 3)),
    N = c(15, 17, 15, 17, rep(50, 6)),
    MEAN = c(45.3, 46.1, 70.2, 71.9, rep(50.0, 3), rep(60.0, 3)),
    SD = c(12.1, 11.8, 9.5, 10.2, rep(10, 3), rep(8, 3)),
    ROUND_MEAN = 1, ROUND_OBSERVATION = 1, stringsAsFactors = FALSE)
}

runBoth <- function(m = 1000) {
  d <- twoTrials()
  v <- shiny::isolate(validateData(d))
  out <- list()
  for (withGraphs in c(FALSE, TRUE)) {
    dqrng::dqset.seed(42); set.seed(42)
    g <- if (withGraphs) newGraphCollector() else NULL
    OUTPUT <- NULL
    for (tr in v$TRIALS)
      OUTPUT <- rbind(OUTPUT, suppressWarnings(shiny::isolate(
        P_Calc(tr, v$DATA, v$CategoryNames, m, graphs = g))))
    out[[if (withGraphs) "with" else "without"]] <-
      list(OUTPUT = OUTPUT, g = g, v = v)
  }
  out
}

test_that("collecting draws changes nothing in the results", {
  r <- runBoth()
  expect_identical(r$with$OUTPUT, r$without$OUTPUT)
})

test_that("the collector holds each simulated row's expected distribution", {
  r <- runBoth()
  rows <- r$with$g$rows
  expect_length(rows, 4)          # 2 trials x 2 continuous variables
  expect_setequal(vapply(rows, function(x) x$kind, character(1)),
                  "continuous")
  for (x in rows) {
    expect_length(x$draws, 1000)  # the first stage, kept in full
    expect_true(is.finite(x$obs))
    expect_true(x$p > 0 && x$p <= 1)
  }
  # trial B's identical arm means: observed squared error 0, p at the
  # Davison-Hinkley floor for m = 1000
  bAge <- Filter(function(x) x$trial == "B" && x$row == "Age", rows)[[1]]
  expect_identical(bAge$obs, 0)
  expect_lte(bAge$p, 0.01)
})

test_that("the deck holds title, overall, per-trial, and smoking-gun slides", {
  skip_if_not_installed("officer")
  skip_if_not_installed("rvg")
  r <- runBoth()
  f <- tempfile(fileext = ".pptx")
  n <- writeGraphsPptx(r$with$OUTPUT, r$with$g, f, rowCutoff = 0.01)
  expect_true(file.exists(f))
  # 1 title + 1 overall + 2 trials + 2 damning rows in trial B
  expect_identical(n, 6L)
  expect_identical(length(officer::read_pptx(f)), 6L)
})

test_that("a single quiet trial yields no overall slide and no row slides", {
  d <- twoTrials()
  d <- d[d$TRIAL == "A", ]
  v <- shiny::isolate(validateData(d))
  dqrng::dqset.seed(7); set.seed(7)
  g <- newGraphCollector()
  OUTPUT <- suppressWarnings(shiny::isolate(
    P_Calc("A", v$DATA, v$CategoryNames, 1000, graphs = g)))
  f <- tempfile(fileext = ".pptx")
  n <- writeGraphsPptx(OUTPUT, g, f)
  # 1 title + 1 per-trial (2 variables); no overall (1 trial), no rows
  # under the cutoff
  expect_identical(n, 2L)
})
