# compareResults.R - compare the mass-test ActualResults against the
# Carlisle-derived ExpectedResults, ON THE LOG SCALE.
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-17.
# The log transform is Steve's specification: "0.0001 is as far from
# 0.001 as the latter is from 0.01" - p-values live on a multiplicative
# scale, and agreement must be measured there, or the comparison is
# dominated by trivial absolute differences among mid-range values while
# ignoring order-of-magnitude disagreements in the tail that actually
# matter. Both sides are log10-transformed before any statistic.
#
# Usage: Rscript corpus/compareResults.R
# Reads  corpus/ActualResults.xlsx (the mass-test run) and
#        corpus/ExpectedResults.xlsx (built from the Carlisle files alone)
# Writes corpus/Comparison.xlsx and prints the agreement summary.
suppressMessages(library(openxlsx))
repo <- "C:/dev/IntegrityAnalysis"

# Optional args: [actualFile] [comparisonOut] (paths under corpus/),
# so the continuous-only run compares without clobbering the full-table
# comparison: Rscript corpus/compareResults.R ActualResults_continuous.xlsx
#             Comparison_continuous.xlsx
cargs <- commandArgs(TRUE)
actFile <- if (length(cargs) >= 1) cargs[1] else "ActualResults.xlsx"
outFile <- if (length(cargs) >= 2) cargs[2] else "Comparison.xlsx"

act <- read.xlsx(file.path(repo, "corpus", actFile),
                 sheet = "Trials")
exp <- read.xlsx(file.path(repo, "corpus", "ExpectedResults.xlsx"),
                 sheet = "Trials")
m <- merge(act, exp, by = "FILE")
m$actual   <- suppressWarnings(as.numeric(m$P_TRIAL))
m$expected <- suppressWarnings(as.numeric(m$CARLISLE_P_TRIAL))
ok <- !is.na(m$actual) & !is.na(m$expected) &
      m$actual > 0 & m$expected > 0
cat("trials compared:", sum(ok), "of", nrow(m), "\n")
d <- m[ok, ]
d$logA <- log10(d$actual)
d$logE <- log10(d$expected)
d$logDiff <- d$logA - d$logE

cat(sprintf("log10 scale: r = %.4f  median |diff| = %.3f  (a diff of 1 = one order of magnitude)\n",
            cor(d$logA, d$logE), median(abs(d$logDiff))))
cat(sprintf("within 0.25 log10 (factor 1.8): %.0f%%   within 0.5 (factor 3.2): %.0f%%   within 1.0 (factor 10): %.0f%%\n",
            100*mean(abs(d$logDiff) <= 0.25),
            100*mean(abs(d$logDiff) <= 0.5),
            100*mean(abs(d$logDiff) <= 1.0)))
# alarm concordance on the raw scale still matters for the verdict
aF <- d$actual < 0.05; eF <- d$expected < 0.05
cat(sprintf("alarm concordance (p < 0.05): %.0f%%  both: %d  actual-only: %d  expected-only: %d\n",
            100*mean(aF == eF), sum(aF & eF), sum(aF & !eF), sum(!aF & eF)))

d <- d[order(-abs(d$logDiff)),
       c("FILE", "PMID", "actual", "expected", "logDiff")]
wb <- createWorkbook()
addWorksheet(wb, "Comparison"); writeData(wb, "Comparison", d)
saveWorkbook(wb, file.path(repo, "corpus", outFile),
             overwrite = TRUE)
cat("written corpus/", outFile, " (sorted worst-first on the log scale)\n")
