# P_Calc.R — the Monte Carlo analysis of one trial.
#
# PROVENANCE: moved out of app_server() in phase 2 of the package
# restructure (Claude Code, model Claude Fable 5, 2026-08-16). One
# deliberate change, verified bit-identical under fixed seeds (see the
# phase-2 PR): the function no longer reads DATA, CategoryNames and m from
# the enclosing server environment - they are explicit arguments, so the
# function can be called (and tested) without a running Shiny session.
# Every FIX comment from the 2026-08-14 bug-fix pass travels with its code.

#' Monte Carlo integrity analysis of one trial's baseline table
#'
#' Reports a single **one-sided p-value toward excessive homogeneity**
#' (issue 6, Steve's decision 2026-08-16, implemented 2026-08-17): P = the
#' probability, under the null hypothesis of random sampling, of baseline
#' data **at least as homogeneous** as observed. Small p = suspiciously
#' homogeneous - the demonstrated fraud signal (Fujii). Excessive
#' heterogeneity is deliberately NOT reported: it is not a known
#' fabrication signal, and under the mid-p tie rule it is exactly
#' 1 - P anyway, so the old second column carried no information.
#'
#' Per row: continuous rows (all arms carry an N) simulate `m`
#' replications of rounded per-arm means and take the lower mid-p tail of
#' the between-arm sum of squares (unchanged from the Carlisle-validated
#' implementation - issue 3, per-trial r = 0.995). Categorical rows
#' simulate `m` contingency tables under fixed margins (`r2dtable`, the
#' same null `chisq.test` uses) and take the lower mid-p tail of the
#' chi-square statistic - counts more similar across arms than chance.
#' NOTE this reverses the categorical direction: `chisq.test`'s p is the
#' UPPER tail (small = arms differ more than chance), which pointed the
#' wrong way for fraud detection and was directionally inconsistent with
#' the continuous rows it was Stouffer-combined with. Median/IQR rows
#' (Q1/Q3 filled in; MEAN read as the median) reconstruct the pooled
#' population with a 3-term metalog matched to the pooled median and
#' quartiles and take the lower mid-p tail of the between-arm scatter of
#' rounded arm MEDIANS. Rows are combined across the trial with
#' Stouffer's [sumz()].
#'
#' @param TRIAL the trial identifier (matched against `DATA$TRIAL`).
#' @param DATA the validated data table (all trials; see [validateData()]).
#' @param CategoryNames names of the category (count) columns, or `NULL`.
#' @param m number of Monte Carlo replications.
#' @return a data.frame with columns TRIAL, ROW, P: one row per data ROW,
#'   then a "Summary" row with the Stouffer-combined p, then a blank
#'   spacer row.
#' @noRd
P_Calc <- function(TRIAL, DATA, CategoryNames, m)
{
  data <- DATA[DATA$TRIAL == TRIAL,]
  RowIDs <- unique(data$ROW)

  x <- foreach(
    j = 1:length(RowIDs),
    .combine = rbind
    #  .options.future = list(seed = TRUE)
    #.export = c("CategoryNames", "m"),
    #.packages = c("Rfast", "dqrng")
  ) %do%
    {
      Row <- RowIDs[j]
      ROWS <- data[data$ROW == Row,]

      # Greater than 1 line?
      if (nrow(ROWS) > 1)
      {
        isQuartile <- "Q1" %in% names(ROWS) &&
                      (any(!is.na(ROWS$Q1)) || any(!is.na(ROWS$Q3)))
        # Is this categorical?
        if (isQuartile && all(!is.na(ROWS$N)))
        {
          # Median/IQR row (Steve's design decision 2026-08-17: quartiles
          # present means MEAN holds the MEDIAN). The null hypothesis is
          # the same as for means - all arms sampled from one common
          # population - but the population is reconstructed from the
          # pooled median and quartiles with a 3-term METALOG
          # distribution (Keelin 2016): it matches the median, Q1, and
          # Q3 EXACTLY including their asymmetry (papers report medians
          # precisely because the data are skewed; a normal fitted as
          # sigma = IQR/1.349 would throw the skew away), its quantile
          # function is closed-form so sampling is one vectorized
          # expression, and it reduces to a symmetric logistic when the
          # quartiles are symmetric. Coefficients (NOTE: the a3 here is
          # 2(Q1+Q3-2m)/ln3; a shared Gemini analysis of this problem
          # printed 4(...)/ln3, a factor-of-2 slip against its own
          # derivation - the unit test pins exact quantile recovery):
          #   a1 = m,  a2 = IQR/(2 ln 3),  a3 = 2(Q1 + Q3 - 2m)/ln 3
          #   X(u) = a1 + a2*logit(u) + a3*(u - 0.5)*logit(u)
          # Feasibility requires |a3/a2| <= 1.667 (Keelin); beyond that
          # the quantile function is non-monotone and the row is refused
          # rather than mis-simulated.
          if (any(is.na(ROWS$Q1)) || any(is.na(ROWS$Q3)))
          {
            P <- "Mixed SD and quartile lines"
          } else {
          COLS <- nrow(ROWS)
          N <- sum(ROWS$N)
          # pooled (N-weighted) median and quartiles define the common
          # population, parallel to the pooled mean/SD in the mean branch
          medPool <- sum(ROWS$N * ROWS$MEAN) / N
          q1Pool  <- sum(ROWS$N * ROWS$Q1) / N
          q3Pool  <- sum(ROWS$N * ROWS$Q3) / N
          a1 <- medPool
          a2 <- (q3Pool - q1Pool) / (2 * log(3))
          a3 <- 2 * (q1Pool + q3Pool - 2 * medPool) / log(3)
          if (a2 <= 0 || abs(a3) / a2 > 1.667)
          {
            P <- "Quartiles too skewed to simulate"
          } else {
          if ((m*N) < 1000000000)
          {
            m1 <- m
          } else {
            m1 <- floor(1000000000 / N)
          }
          # Per-replication uncertainty in the common location, parallel
          # to SEMsample in the mean branch. Asymptotic SD of a sample
          # median is 1/(2 f(m) sqrt(n)); the metalog density at its
          # median is 1/(4 a2), so SD_median = 2 a2 / sqrt(n).
          shiftsim <- dqrnorm(m1, mean = 0,
                              sd = 2 * a2 / sqrt(mean(ROWS$N)))
          MonteCarloMed <- matrix(NA, nrow = m1, ncol = COLS)
          for (i in 1:COLS)
          {
            U <- matrix(dqrunif(ROWS$N[i] * m1), nrow = m1)
            L <- log(U / (1 - U))
            X <- (a1 + shiftsim) + a2 * L + a3 * (U - 0.5) * L
            MonteCarloMed[,i] <-
              round(
                Rfast::rowMedians(
                  round(X, ROWS$ROUND_OBSERVATION[i])),
                ROWS$ROUND_MEAN[i])
          }
          Nmat <- matrix(ROWS$N, nrow = m1, ncol = COLS, byrow = TRUE)
          MedCenter   <- rowsums(MonteCarloMed * Nmat) / N
          DiffSamples <- rowsums((MonteCarloMed - MedCenter)^2)
          center     <- sum(ROWS$N * ROWS$MEAN) / N
          DiffSample <- sum((ROWS$MEAN - center)^2)
          PEQ <- sum(DiffSamples == DiffSample) / m1
          # lower mid-p tail toward homogeneity, same convention as the
          # mean branch
          P <- sum(DiffSamples < DiffSample)/m1 + PEQ/2
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
          # Protect size of simulation
          if ((m*N) < 1000000000) # One billion
          {
            m1 <- m
          } else {
            # FIX: floor() added. 1e9/N is rarely a whole number, and a
            # fractional replication count silently mis-sizes the matrix
            # dimensions below.
            m1 <- floor(1000000000 / N)
          }
          SEMsample <- Meansd/sqrt(mean(ROWS$N))
          DiffSample <- sum((ROWS$MEAN - Meanmean)^2) # Squared difference of column means
          # Monte Carlo Simulation
          # FIX: was dqrnorm(m, ...). meansim must have exactly m1 entries
          # (one simulated "true" mean per replication row). When N was
          # large enough that m1 < m, the extra entries misaligned the
          # column-major matrix fill below, so within one replication the
          # study arms were simulated from DIFFERENT true means - breaking
          # the null hypothesis the simulation is supposed to represent.
          meansim <- dqrnorm(m1,mean=Meanmean,sd=SEMsample) # Generate a new mean for each simulation
          MonteCarloMean <- matrix(NA, nrow = m1, ncol = COLS) # I want one row for each simulation
          # Need to do each column separately. Couldn't think of an efficient way to do this without
          # a loop.
          for (i in 1:COLS)
            MonteCarloMean[,i] <-
            round(
              rowmeans(
                round(
                  # The matrix below will have one row for each replication (m rows),
                  # and one column for each person (N[i] columns)
                  # Cannot use dqrnorm because it won't support the array
                  # of meansim needed for each replication
                  matrix(
                    rnorm(ROWS$N[i] * m1, rep(meansim, ROWS$N[i]), Meansd),
                    nrow = m1, byrow = FALSE
                  ),
                  ROWS$ROUND_OBSERVATION[i]  # observations rounded as recorded
                )
              ),
              # FIX: was ROWS$ROUND_OBSERVATION[i]. The simulated column
              # means must be rounded to the precision at which the
              # PUBLISHED means were reported (ROUND_MEAN) - that is the
              # value the validation code in the upload observer goes to
              # such lengths to derive, and it was never used. Using the
              # observation precision simulated the Monte Carlo
              # distribution at the wrong granularity whenever means are
              # reported more (or less) precisely than the observations.
              ROWS$ROUND_MEAN[i]
            )
          N <- matrix(ROWS$N, nrow = m1, ncol = COLS, byrow = TRUE)
          # Calculate the weighted mean, and then round
          MeanSamples <- rowsums (MonteCarloMean * N) / sum(ROWS$N)
          DiffSamples <- rowsums((MonteCarloMean - MeanSamples)^2)

          PEQ <- sum(DiffSamples == DiffSample) / m1
          # Mid-p convention (Steve's decision, 2026-08-16): simulated ties
          # count HALF, so P = P(<) + PEQ/2. Rounding makes DiffSamples
          # discrete, so ties are common and the choice matters. Evidence
          # for mid-p: the issue-3 pilot against Carlisle's stored 2017
          # values - full-tie counting disagreed with median |diff| 0.076,
          # always ours-higher (the tie-inflation signature); as mid-p,
          # per-trial r = 0.995 with 93% within 0.05. Carlisle's published
          # values are, in effect, mid-p, and mid-p is the standard
          # recommendation for discrete test statistics.
          # One-sided toward homogeneity (issue 6): this lower tail is the
          # only direction reported. This line is bit-identical to the
          # Carlisle-validated implementation.
          P <- sum(DiffSamples < DiffSample)/m1 + PEQ/2
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
        # One-sided toward homogeneity (issue 6, 2026-08-17): this
        # REVERSES the categorical direction. The previous code reported
        # chisq.test's simulated p, which is the UPPER tail - small when
        # the arms differ MORE than chance. For fraud detection the
        # alarming direction is the opposite one: fabricated categorical
        # baselines tend to be TOO SIMILAR across arms (a too-small
        # chi-square). So simulate the same null chisq.test uses -
        # contingency tables with the observed margins, via r2dtable -
        # and take the LOWER mid-p tail of the chi-square statistic,
        # exactly parallel to the continuous branch. (chisq.test itself
        # only offers the upper tail, and hides its tie counts, which is
        # why this simulates directly rather than transforming its p.)
        # Expected cells use the classical formula; ties are exact
        # because every statistic comes from the same integer margins.
          tab <- as.matrix(ROWS)
          # FIX (2026-08-17, found by the corpus/TEST mass run): degenerate
          # tables crashed the whole analysis. A category cell can be NA
          # (one arm's line carries a category the other arm's does not),
          # and a category column can sum to zero; either way the expected
          # counts contain zeros, the chi-square statistic is NaN, and
          # if (P == 1) on NaN is a fatal error. Refuse such rows with a
          # message instead - consistent with the app's refuse-rather-
          # than-guess rule - after dropping zero-margin columns, which
          # carry no information.
          if (any(is.na(tab)))
          {
            P <- "Incomplete category counts across arms"
          } else {
          tab <- tab[, colSums(tab) > 0, drop = FALSE]
          if (ncol(tab) < 2 || any(rowSums(tab) == 0))
          {
            P <- "Degenerate category table (an arm or every remaining category is empty)"
          } else {
          E <- outer(rowSums(tab), colSums(tab)) / sum(tab)
          statObs <- sum((tab - E)^2 / E)
          statSim <- vapply(
            r2dtable(m, rowSums(tab), colSums(tab)),
            function(s) sum((s - E)^2 / E), numeric(1))
          PEQ <- sum(statSim == statObs) / m
          P <- sum(statSim < statObs)/m + PEQ/2
          }
          }
        }
        # Need to be sure P != 0 or 1. (Guarded: the median/IQR branch can
        # refuse a row with a message string instead of a number.)
        if (is.numeric(P))
        {
          if(P == 1) P <- 0.999
          if(P == 0) P <- 0.001
          P = as.character(signif(P,4))
        }
      } else {
        P = "Only 1 Row"
      }

      c(as.character(Row), P)
    }
  # FIX: removed "%seed% TRUE" after the closing brace. %seed% is
  # doFuture's operator for seeding a %dofuture% loop; chained after %do%
  # it was applied to the already-computed result matrix, which is an
  # error. (Note dqrnorm draws from dqrng's own RNG stream; use
  # dqset.seed() if reproducible simulations are ever needed.)

  # This bizarre code is because if there is only 1 row, R creates a data.frame
  # with 2 columns and 1 row. (Was length 3 before issue 6 dropped the
  # heterogeneity column.)
  if (length(x) == 2)
  {
    x <- as.data.frame(t(x))
  } else {
  x <- as.data.frame(x)
  }

  x <- cbind(NA, x)
  x[1,1] <- TRIAL
  x <- as.data.frame(x)
  names(x) <- c("TRIAL", "ROW", "P")
  # FIX: was x[match(x$ROW, RowIDs),] - the arguments were reversed, which
  # applies the INVERSE permutation. Harmless today only because %do%
  # returns results already in RowIDs order; it would silently scramble row
  # labels against p-values the day this loop is parallelized. To order x
  # by RowIDs: for each RowID, find its position in x$ROW.
  # (Also removed the leftover cat()/print() debugging output here.)
  x <- x[match(RowIDs, x$ROW),]

  Pvalues <- as.numeric(x$P)
  Pvalues <- Pvalues[!is.na(Pvalues)]

  if (length(Pvalues) > 1)
  {
    P <- signif(sumz(Pvalues)$p,4)
  } else {
    # FIX: was length(Pvalues == 1), i.e. the length of a comparison
    # vector, not a comparison of the length. It worked by coincidence
    # (length 1 -> 1 -> truthy; length 0 -> 0 -> falsy) but was a trap.
    if (length(Pvalues) == 1)
      P <- Pvalues
    if (length(Pvalues)==0)
      P = "No values"
  }

  lastline <- data.frame(
    TRIAL = c(NA, NA),
    ROW = c("Summary", NA),
    P = c(as.character(P), NA)
  )

  x <- rbind(x, lastline)
  outputComments(
    paste0("Trial ", TRIAL,": p = ", P, "\n")
  )
  return(x)
}
