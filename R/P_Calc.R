# P_Calc.R — the Monte Carlo analysis of one trial.
#
# PROVENANCE: moved out of app_server() in phase 2 of the package
# restructure (Claude Code, model Claude Fable 5, 2026-08-16); the function
# takes DATA, CategoryNames and m as explicit arguments so it can be called
# (and tested) without a running Shiny session. Every FIX comment from the
# 2026-08-14 bug-fix pass travels with its code. 2026-08-17: one-sided
# toward homogeneity (issue 6), median/IQR metalog branch (issue 12),
# degenerate-category refusal, and the ADAPTIVE STAGED scheme below
# (Steve's decision after the replicate-count discussion; independently
# convergent with a Gemini analysis he commissioned).
#
# THE STAGED SCHEME, in brief (full user documentation: docs/statistics.md):
#   - Each row simulates in stages: 1,000 -> 10,000 -> mMax (100,000)
#     replicates, advancing only while the running mid-p is < 0.01. Clean
#     rows stop at 1,000; only alarming rows pay for precision.
#   - Point estimate: mid-p (ties count half - the Carlisle-validated
#     convention), floored at 1/(m+1) (Davison & Hinkley: the Monte Carlo
#     test is exact-valid; a simulated p of literally 0 is never reported).
#   - Display: a row shows "<0.0001" ONLY when the one-sided 97.5%
#     Clopper-Pearson upper bound on the exceedance count clears 0.0001 -
#     the claim is licensed by the upper confidence limit, not the point
#     estimate. Ties count FULLY toward the bound (conservative).
#   - The trial p (Stouffer across rows) is closed-form arithmetic with no
#     simulation noise of its own and is NOT floored - accumulation across
#     rows is the fraud signal (Fujii). Its Monte Carlo uncertainty is
#     propagated from the rows' binomial counts by parametric bootstrap
#     and reported as a 95% interval whenever the trial p < 0.001.

#' Staged tail simulation: escalate replicates only when the row alarms
#'
#' Runs `simulate(n)` (a closure returning n simulated statistics) in
#' cumulative stages 1,000 / 10,000 / `mMax`, stopping early once the
#' running mid-p is >= 0.01 - precision there is worthless. Returns the
#' accumulated counts below (`kLess`) and tied with (`kEq`) `statObs`,
#' and the replicates actually used (`m`).
#' @noRd
.stagedTail <- function(simulate, statObs, mMax) {
  stages <- unique(pmin(c(1000, 10000, mMax), mMax))
  kLess <- 0; kEq <- 0; mDone <- 0
  for (s in stages) {
    n <- s - mDone
    if (n <= 0) next
    sims <- simulate(n)
    kLess <- kLess + sum(sims < statObs)
    kEq   <- kEq   + sum(sims == statObs)
    mDone <- s
    if ((kLess + kEq / 2) / mDone >= 0.01) break
  }
  list(kLess = kLess, kEq = kEq, m = mDone)
}

#' One-sided 97.5% Clopper-Pearson upper bound on a Monte Carlo p
#'
#' `kLE` counts simulations at or below the observed statistic (ties
#' fully - conservative for the bound). This is the number that licenses
#' a "<0.0001" claim: the claim is made only when this bound clears it.
#' @noRd
.mcUpper <- function(kLE, m) stats::qbeta(0.975, kLE + 1, pmax(m - kLE, 0))

#' Turn staged-tail counts into the row's report fields
#'
#' @return list: `p` (numeric mid-p, DH-floored), `disp` (display string:
#'   "<0.0001" when licensed, else the number), `ci` (upper-bound string,
#'   blank unless p < 0.001), `kLE`, `m`.
#' @noRd
.rowReport <- function(sc) {
  midp <- (sc$kLess + sc$kEq / 2) / sc$m
  p <- max(midp, 1 / (sc$m + 1))
  if (p >= 1) p <- 0.9999
  kLE <- sc$kLess + sc$kEq
  upper <- .mcUpper(kLE, sc$m)
  disp <- if (upper < 1e-4) "<0.0001" else as.character(signif(p, 4))
  ci <- if (p < 0.001) paste0("<=", signif(upper, 2)) else ""
  list(p = p, disp = disp, ci = ci, kLE = kLE, m = sc$m)
}

#' Monte Carlo integrity analysis of one trial's baseline table
#'
#' Reports a single **one-sided p-value toward excessive homogeneity**
#' (issue 6): P = the probability, under the null hypothesis of random
#' sampling, of baseline data **at least as homogeneous** as observed.
#' Small p = suspiciously homogeneous - the demonstrated fraud signal
#' (Fujii). Heterogeneity is deliberately not reported.
#'
#' Per row: continuous rows (all arms carry an N) simulate rounded
#' per-arm means under a common population (Carlisle-validated, issue 3);
#' median/IQR rows (Q1/Q3 filled; MEAN read as the median) use a 3-term
#' metalog matched to the pooled quartiles and compare rounded arm
#' MEDIANS; categorical rows simulate contingency tables under fixed
#' margins (`r2dtable`) and take the LOWER chi-square tail (counts more
#' alike than chance). All rows use the staged replicate scheme and
#' confidence-bounded reporting described at the top of this file, and
#' combine across the trial with Stouffer's [sumz()] - unfloored, with a
#' parametric-bootstrap 95% Monte Carlo interval when small.
#'
#' @param TRIAL the trial identifier (matched against `DATA$TRIAL`).
#' @param DATA the validated data table (all trials; see [validateData()]).
#' @param CategoryNames names of the category (count) columns, or `NULL`.
#' @param m maximum replicates per row (the final stage; default global
#'   is 100,000 - typical rows stop at 1,000).
#' @return a data.frame with columns TRIAL, ROW, P, CI95, M: one row per
#'   data ROW (M = replicates actually used; CI95 = upper bound, shown
#'   when P < 0.001), then a "Summary" row with the combined p and its
#'   bootstrap interval, then a blank spacer row.
#' @noRd
P_Calc <- function(TRIAL, DATA, CategoryNames, m)
{
  data <- DATA[DATA$TRIAL == TRIAL,]
  RowIDs <- unique(data$ROW)

  x <- foreach(
    j = 1:length(RowIDs),
    .combine = rbind
  ) %do%
    {
      Row <- RowIDs[j]
      ROWS <- data[data$ROW == Row,]

      Pdisp <- NA_character_; Pci <- ""; Pm <- NA_character_
      Pnum <- NA_real_; PkLE <- NA_real_

      # Greater than 1 line?
      if (nrow(ROWS) > 1)
      {
        isQuartile <- "Q1" %in% names(ROWS) &&
                      (any(!is.na(ROWS$Q1)) || any(!is.na(ROWS$Q3)))
        if (isQuartile && all(!is.na(ROWS$N)))
        {
          # Median/IQR row (issue 12): the common population is a 3-term
          # METALOG matched to the pooled median and quartiles (Keelin
          # 2016) - exact including asymmetry, closed-form sampling,
          # logistic in the symmetric case. Coefficients (NOTE: a shared
          # Gemini analysis printed a3 = 4(...)/ln3, a factor-of-2 slip
          # against its own derivation; exact quantile recovery is pinned
          # by a unit test):
          #   a1 = m,  a2 = IQR/(2 ln 3),  a3 = 2(Q1 + Q3 - 2m)/ln 3
          # |a3/a2| > 1.667 (Keelin validity) refuses the row.
          if (any(is.na(ROWS$Q1)) || any(is.na(ROWS$Q3)))
          {
            Pdisp <- "Mixed SD and quartile lines"
          } else {
          COLS <- nrow(ROWS)
          N <- sum(ROWS$N)
          medPool <- sum(ROWS$N * ROWS$MEAN) / N
          q1Pool  <- sum(ROWS$N * ROWS$Q1) / N
          q3Pool  <- sum(ROWS$N * ROWS$Q3) / N
          a1 <- medPool
          a2 <- (q3Pool - q1Pool) / (2 * log(3))
          a3 <- 2 * (q1Pool + q3Pool - 2 * medPool) / log(3)
          if (a2 <= 0 || abs(a3) / a2 > 1.667)
          {
            Pdisp <- "Quartiles too skewed to simulate"
          } else {
          center     <- sum(ROWS$N * ROWS$MEAN) / N
          DiffSample <- sum((ROWS$MEAN - center)^2)
          # Per-replication uncertainty in the common location: asymptotic
          # SD of a sample median is 1/(2 f(m) sqrt(n)); metalog density
          # at its median is 1/(4 a2), so SD_median = 2 a2 / sqrt(n).
          sdShift <- 2 * a2 / sqrt(mean(ROWS$N))
          simulate <- function(n) {
            out <- numeric(0); left <- n
            while (left > 0) {
              # chunk so chunk*N stays bounded (memory guard, successor
              # of the old fixed m1 <- 1e9/N cap)
              ch <- min(left, max(1, floor(1e8 / max(1, N))))
              shiftsim <- dqrnorm(ch, mean = 0, sd = sdShift)
              MCMed <- matrix(NA_real_, ch, COLS)
              for (i in 1:COLS)
              {
                U <- matrix(dqrunif(ROWS$N[i] * ch), nrow = ch)
                L <- log(U / (1 - U))
                X <- (a1 + shiftsim) + a2 * L + a3 * (U - 0.5) * L
                MCMed[,i] <- round(
                  Rfast::rowMedians(round(X, ROWS$ROUND_OBSERVATION[i])),
                  ROWS$ROUND_MEAN[i])
              }
              Nmat <- matrix(ROWS$N, ch, COLS, byrow = TRUE)
              MedC <- rowsums(MCMed * Nmat) / N
              out <- c(out, rowsums((MCMed - MedC)^2))
              left <- left - ch
            }
            out
          }
          rep <- .rowReport(.stagedTail(simulate, DiffSample, m))
          Pdisp <- rep$disp; Pci <- rep$ci; Pm <- as.character(rep$m)
          Pnum <- rep$p; PkLE <- rep$kLE
          }
          }
        }
        else if (all(!is.na(ROWS$N)))
        {
          COLS <- nrow(ROWS)
          N <- sum(ROWS$N)
          Meanmean <- sum(ROWS$N*ROWS$MEAN) / N
          # The calculation of Meanvar is OK. SD^2 is an unbiased estimate
          # of variance
          Meanvar <-  sum(ROWS$N*ROWS$SD^2) / N

          # However, this next calculatiion is biased. s.u. will correct it
          # If N > 30, then the correction is < 1 %. It blows up if N > 343!
          if (N < 30)
          {
            Meansd <- s.u(sqrt(Meanvar), N)
          } else {
            Meansd <- sqrt(Meanvar)
          }
          SEMsample <- Meansd/sqrt(mean(ROWS$N))
          DiffSample <- sum((ROWS$MEAN - Meanmean)^2) # Squared difference of column means
          # Monte Carlo Simulation. The simulation body is unchanged from
          # the Carlisle-validated implementation (issue 3, r = 0.991);
          # the staging wrapper only decides HOW MANY replications run.
          # FIX (2026-08-14, carried): meansim must have exactly as many
          # entries as replication rows, or the column-major fill would
          # misalign arms within a replication; simulated column means
          # round to ROUND_MEAN (the printed precision), observations to
          # ROUND_OBSERVATION.
          simulate <- function(n) {
            out <- numeric(0); left <- n
            while (left > 0) {
              ch <- min(left, max(1, floor(1e8 / max(1, N))))
              meansim <- dqrnorm(ch, mean = Meanmean, sd = SEMsample)
              MCMean <- matrix(NA_real_, ch, COLS)
              for (i in 1:COLS)
                MCMean[,i] <- round(
                  rowmeans(round(
                    matrix(rnorm(ROWS$N[i] * ch,
                                 rep(meansim, ROWS$N[i]), Meansd),
                           nrow = ch, byrow = FALSE),
                    ROWS$ROUND_OBSERVATION[i])),
                  ROWS$ROUND_MEAN[i])
              Nmat <- matrix(ROWS$N, ch, COLS, byrow = TRUE)
              MS <- rowsums(MCMean * Nmat) / N
              out <- c(out, rowsums((MCMean - MS)^2))
              left <- left - ch
            }
            out
          }
          rep <- .rowReport(.stagedTail(simulate, DiffSample, m))
          Pdisp <- rep$disp; Pci <- rep$ci; Pm <- as.character(rep$m)
          Pnum <- rep$p; PkLE <- rep$kLE
        } else {
          # FIX: drop = FALSE added. With a single category column,
          # ROWS[,CategoryNames] dropped to a bare vector and the
          # ROWS[,NAME] <- NULL loop below crashed with "incorrect number
          # of dimensions".
          ROWS <- ROWS[,CategoryNames, drop = FALSE]
          for (NAME in CategoryNames)
          {
            if (all(is.na(ROWS[,NAME])))
              ROWS[,NAME] <- NULL
          }
          # One-sided toward homogeneity (issue 6): the LOWER mid-p tail
          # of the chi-square statistic under fixed margins (r2dtable,
          # chisq.test's own null) - counts more alike than chance.
          tab <- as.matrix(ROWS)
          # FIX (2026-08-17, found by the corpus/TEST mass run):
          # degenerate tables (NA cells, zero-margin columns) made the
          # statistic NaN and crashed the analysis; refuse instead.
          if (any(is.na(tab)))
          {
            Pdisp <- "Incomplete category counts across arms"
          } else {
          tab <- tab[, colSums(tab) > 0, drop = FALSE]
          if (ncol(tab) < 2 || any(rowSums(tab) == 0))
          {
            Pdisp <- "Degenerate category table (an arm or every remaining category is empty)"
          } else {
          E <- outer(rowSums(tab), colSums(tab)) / sum(tab)
          statObs <- sum((tab - E)^2 / E)
          simulate <- function(n)
            vapply(r2dtable(n, rowSums(tab), colSums(tab)),
                   function(s) sum((s - E)^2 / E), numeric(1))
          rep <- .rowReport(.stagedTail(simulate, statObs, m))
          Pdisp <- rep$disp; Pci <- rep$ci; Pm <- as.character(rep$m)
          Pnum <- rep$p; PkLE <- rep$kLE
          }
          }
        }
      } else {
        Pdisp <- "Only 1 Row"
      }

      c(as.character(Row), Pdisp, Pci, Pm,
        as.character(Pnum), as.character(PkLE))
    }
  # Single-row trials come back as a bare vector (now length 6).
  if (length(x) == 6)
  {
    x <- as.data.frame(t(x))
  } else {
  x <- as.data.frame(x)
  }

  x <- cbind(NA, x)
  x[1,1] <- TRIAL
  x <- as.data.frame(x)
  names(x) <- c("TRIAL", "ROW", "P", "CI95", "M", ".PNUM", ".KLE")
  # FIX (carried): order x by RowIDs - for each RowID, find its position
  # in x$ROW; the reversed match() applied the inverse permutation.
  x <- x[match(RowIDs, x$ROW),]

  Pv   <- suppressWarnings(as.numeric(x$.PNUM))
  kLEv <- suppressWarnings(as.numeric(x$.KLE))
  Mv   <- suppressWarnings(as.numeric(x$M))
  use  <- !is.na(Pv)

  ciStr <- ""
  if (sum(use) > 1)
  {
    # Stouffer across rows: closed-form arithmetic on the row p-values -
    # NO simulation noise is added here, and the result is deliberately
    # NOT floored: accumulation across rows is the fraud signal (eight
    # innocuous rows at p = 0.01 legitimately combine to ~5e-9). The
    # Monte Carlo uncertainty that DOES exist - each row's binomial
    # count - is propagated by parametric bootstrap and reported as a
    # 95% interval whenever the combined p is small enough to matter.
    P <- signif(sumz(Pv[use])$p, 4)
    if (P < 0.001)
    {
      boots <- replicate(1000, {
        pStar <- (stats::rbinom(sum(use), Mv[use], Pv[use]) + 0.5) /
                 (Mv[use] + 1)
        sumz(pStar)$p
      })
      ciStr <- paste0(signif(stats::quantile(boots, 0.025, names = FALSE), 2),
                      " to ",
                      signif(stats::quantile(boots, 0.975, names = FALSE), 2))
    }
  } else {
    # FIX (carried): length(Pv[use]) == 1 vs the old length(x == 1) trap
    if (sum(use) == 1)
      P <- Pv[use]
    if (sum(use) == 0)
      P = "No values"
  }

  lastline <- data.frame(
    TRIAL = c(NA, NA),
    ROW = c("Summary", NA),
    P = c(as.character(P), NA),
    CI95 = c(ciStr, NA),
    M = c(NA, NA),
    .PNUM = c(NA, NA),
    .KLE = c(NA, NA)
  )

  x <- rbind(x, lastline)
  # internal bookkeeping columns stay out of the results
  x <- x[, c("TRIAL", "ROW", "P", "CI95", "M")]
  outputComments(
    paste0("Trial ", TRIAL,": p = ", P,
           if (nzchar(ciStr)) paste0(" (95% MC interval ", ciStr, ")")
           else "", "\n")
  )
  return(x)
}
