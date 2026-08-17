# Median/IQR support (Steve's design, 2026-08-17): Q1/Q3 columns; when both
# are filled in, MEAN is read as the MEDIAN. The Monte Carlo null for such
# rows is a 3-term metalog matched to the pooled median and quartiles.
#
# PROVENANCE: written with the feature by Claude Code (model Claude
# Fable 5), 2026-08-17. The metalog-coefficient test below deliberately
# pins a1/a2/a3 by exact quantile recovery: a shared Gemini analysis of
# this problem printed a3 = 4(Q1+Q3-2m)/ln3, a factor-of-2 slip against
# its own derivation; the correct coefficient is 2(Q1+Q3-2m)/ln3.

suppressWarnings(suppressPackageStartupMessages({
  library(shiny); library(foreach); library(MBESS); library(Rfast)
  library(dqrng)
}))

metalog3 <- function(u, m, q1, q3) {
  a1 <- m
  a2 <- (q3 - q1) / (2 * log(3))
  a3 <- 2 * (q1 + q3 - 2 * m) / log(3)
  a1 + a2 * log(u / (1 - u)) + a3 * (u - 0.5) * log(u / (1 - u))
}

test_that("metalog coefficients recover the printed quantiles exactly", {
  # asymmetric quartiles - the case the factor-2 slip would corrupt
  m <- 12; q1 <- 8; q3 <- 20
  expect_equal(metalog3(0.50, m, q1, q3), m)
  expect_equal(metalog3(0.25, m, q1, q3), q1)
  expect_equal(metalog3(0.75, m, q1, q3), q3)
  # symmetric case reduces cleanly too
  expect_equal(metalog3(0.25, 10, 7, 13), 7)
  expect_equal(metalog3(0.75, 10, 7, 13), 13)
})

mkrow <- function(med, q1, q3, n = 30) data.frame(
  TRIAL = "T", ROW = "X",
  N = n, MEAN = med, SD = NA_real_, SE = NA_real_,
  Q1 = q1, Q3 = q3,
  ROUND_MEAN = 1, ROUND_DISPERSION = 1, ROUND_OBSERVATION = 1,
  stringsAsFactors = FALSE)

test_that("validateData accepts a well-formed median/IQR row", {
  d <- rbind(mkrow(12.0, 8.0, 17.0), mkrow(11.5, 8.5, 16.0))
  v <- shiny::isolate(validateData(d))
  expect_false(v$FAIL)
  expect_true(all(c("Q1", "Q3") %in% names(v$DATA)))
})

test_that("validateData rejects ambiguous and malformed quartile rows", {
  # quartiles AND an SD - ambiguous about what MEAN means
  d <- rbind(mkrow(12, 8, 17), mkrow(11.5, 8.5, 16))
  d$SD[1] <- 4.1
  expect_true(shiny::isolate(validateData(d))$FAIL)
  # only one quartile
  d <- rbind(mkrow(12, 8, 17), mkrow(11.5, 8.5, 16))
  d$Q3[2] <- NA
  expect_true(shiny::isolate(validateData(d))$FAIL)
  # median outside its quartiles
  d <- rbind(mkrow(12, 8, 17), mkrow(7.9, 8.5, 16))
  expect_true(shiny::isolate(validateData(d))$FAIL)
  # Q1/Q3 must never be swallowed as category columns
  d <- rbind(mkrow(12, 8, 17), mkrow(11.5, 8.5, 16))
  v <- shiny::isolate(validateData(d))
  expect_false("Q1" %in% v$CategoryNames)
})

pOf <- function(d, m = 4000) {
  x <- suppressWarnings(shiny::isolate(
    P_Calc("T", d, NULL, m)))
  suppressWarnings(as.numeric(
    x$P[x$ROW == "Summary" & !is.na(x$ROW)]))[1]
}

test_that("median rows point the right way: identical arms alarm, different arms do not", {
  dqrng::dqset.seed(7); set.seed(7)
  same <- rbind(mkrow(12.0, 8.0, 17.0, n = 40),
                mkrow(12.0, 8.0, 17.0, n = 40),
                mkrow(12.0, 8.0, 17.0, n = 40))
  diff <- rbind(mkrow(12.0, 8.0, 17.0, n = 40),
                mkrow(16.5, 12.5, 21.5, n = 40),
                mkrow(8.0, 4.0, 13.0, n = 40))
  expect_lt(pOf(same), 0.35)
  expect_gt(pOf(diff), 0.90)
})

test_that("a too-skewed row is refused, not mis-simulated", {
  # |a3/a2| > 1.667: Q3 - m much smaller than m - Q1, extreme
  d <- rbind(mkrow(12, 2, 12.4), mkrow(12, 2, 12.4))
  x <- suppressWarnings(shiny::isolate(P_Calc("T", d, NULL, 1000)))
  expect_true(any(grepl("skewed", x$P)))
})

test_that("calibration: honest median/IQR data give roughly uniform p", {
  # Draw genuinely-null trials (both arms from ONE skewed population),
  # summarize each arm the way a paper would (rounded median and
  # quartiles), and push the summaries through P_Calc. The p-values must
  # not pile up near either tail. Bounds are deliberately loose - this is
  # a smoke alarm for gross miscalibration, not a distribution test.
  dqrng::dqset.seed(42); set.seed(42)
  ps <- replicate(30, {
    pop <- function(n) round(rlnorm(n, meanlog = 2.3, sdlog = 0.45), 1)
    arm <- function() {
      x <- pop(45)
      c(med = round(median(x), 1),
        q1 = round(quantile(x, 0.25, names = FALSE), 1),
        q3 = round(quantile(x, 0.75, names = FALSE), 1))
    }
    a <- arm(); b <- arm()
    d <- rbind(mkrow(a["med"], a["q1"], a["q3"], n = 45),
               mkrow(b["med"], b["q1"], b["q3"], n = 45))
    pOf(d, m = 2000)
  })
  ps <- ps[!is.na(ps)]
  expect_gt(length(ps), 25)
  expect_gt(mean(ps), 0.30)
  expect_lt(mean(ps), 0.70)
  expect_gt(sum(ps > 0.5), 5)   # neither tail hoards everything
  expect_gt(sum(ps < 0.5), 5)
})
