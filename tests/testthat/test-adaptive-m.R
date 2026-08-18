# The adaptive staged Monte Carlo and confidence-bounded reporting
# (Steve's decision, 2026-08-17; scheme described in docs/statistics.md).
suppressWarnings(suppressPackageStartupMessages({
  library(shiny); library(foreach); library(MBESS); library(Rfast)
  library(dqrng)
}))

meanrow <- function(mean, sd, n = 40) data.frame(
  TRIAL = "T", ROW = "X",
  N = n, MEAN = mean, SD = sd,
  ROUND_MEAN = 1, ROUND_OBSERVATION = 1, stringsAsFactors = FALSE)

runP <- function(d, m = 100000) suppressWarnings(shiny::isolate(
  P_Calc("T", d, NULL, m)))

test_that("unalarming rows stop at 1,000 replicates; alarming rows escalate", {
  dqrng::dqset.seed(11); set.seed(11)
  quiet <- runP(rbind(meanrow(54.1, 9.2), meanrow(51.0, 8.9)))
  i <- which(quiet$ROW == "X")[1]
  expect_equal(as.numeric(quiet$M[i]), 1000)

  alarm <- runP(rbind(meanrow(54.1, 9.2), meanrow(54.1, 9.2),
                      meanrow(54.1, 9.2)))
  i <- which(alarm$ROW == "X")[1]
  expect_gt(as.numeric(alarm$M[i]), 1000)
})

test_that("'<0.0001' appears only when the upper bound licenses it", {
  # licensing arithmetic, tested directly on the report builder
  r0 <- .rowReport(list(kLess = 0, kEq = 0, m = 100000))
  expect_identical(r0$disp, "<0.0001")          # upper 3.7e-5 < 1e-4
  r1 <- .rowReport(list(kLess = 0, kEq = 0, m = 10000))
  expect_false(identical(r1$disp, "<0.0001"))   # upper 3.7e-4 - NOT licensed
  expect_equal(as.numeric(r1$disp), 1 / 10001, tolerance = 1e-6)  # DH floor
  # one-sided 97.5% bound (= Gemini's two-sided 95% table): at m = 1e5
  # the licensing cutoff is k <= 3 (k=3 upper ~8.8e-5; k=4 ~1.02e-4)
  r4 <- .rowReport(list(kLess = 4, kEq = 0, m = 100000))
  expect_false(identical(r4$disp, "<0.0001"))
  r3 <- .rowReport(list(kLess = 3, kEq = 0, m = 100000))
  expect_identical(r3$disp, "<0.0001")
})

test_that("rows with p < 0.001 carry an explicit upper bound", {
  r <- .rowReport(list(kLess = 10, kEq = 0, m = 100000))
  expect_match(r$ci, "^<=")
  r <- .rowReport(list(kLess = 300, kEq = 0, m = 1000))
  expect_identical(r$ci, "")
})

test_that("the combined trial p is NOT floored and carries a bootstrap interval", {
  dqrng::dqset.seed(12); set.seed(12)
  # four strongly homogeneous variables: each row small, combined far
  # below the per-row 1e-4 floor - and that combined value must survive
  d <- do.call(rbind, lapply(1:4, function(k) {
    r <- rbind(meanrow(50 + k, 8.0), meanrow(50 + k, 8.0),
               meanrow(50 + k, 8.0))
    r$ROW <- paste0("V", k); r
  }))
  x <- runP(d)
  s <- which(x$ROW == "Summary")
  p <- as.numeric(x$P[s])
  expect_lt(p, 1e-4)
  expect_match(x$CI95[s], "to")   # bootstrap interval present
})

test_that("mid-p point estimates are unchanged in spirit: direction holds", {
  dqrng::dqset.seed(13); set.seed(13)
  hom <- runP(rbind(meanrow(54.1, 9.2), meanrow(54.1, 9.2),
                    meanrow(54.1, 9.2)), m = 10000)
  het <- runP(rbind(meanrow(40.0, 9.2), meanrow(54.1, 9.2),
                    meanrow(68.0, 9.2)), m = 10000)
  pH <- suppressWarnings(as.numeric(
    hom$P[hom$ROW == "Summary" & !is.na(hom$ROW)]))[1]
  pT <- suppressWarnings(as.numeric(
    het$P[het$ROW == "Summary" & !is.na(het$ROW)]))[1]
  expect_lt(pH, 0.05)
  expect_gt(pT, 0.9)
})
