# validateCarlisle2017.R - the issue-3 end-to-end validation: run the
# shipped engine over Carlisle's 2017 raw data and compare every trial
# p against his stored values.
#
# PROVENANCE: written by Claude Code (model Claude Opus 5), 2026-08-21,
# reconstructing the 2026-08-16/17 pilot whose scripts lived only in a
# session scratchpad (carlisle_pilot.R etc., recorded in ISSUES.md
# issue 3 and the session memory). That work already validated the
# then-current build once - 5,080 trials, r = 0.991 - but left no
# committed artifact; this script makes the validation REPRODUCIBLE and
# runs it against the CURRENT engine (adaptive staged replicates,
# mid-p, one-sided toward homogeneity - the convention Steve ratified
# 2026-08-20).
#
# INPUTS (working data, never committed):
#   "One Sheet Carlisle Data.xlsx"       - the raw rows (72,151):
#       TRIAL "Anaesthesia 1" style, MEASURE/GROUP/N/MEAN/SD/DECM/DECSD.
#       validateData()'s Carlisle aliases handle every column natively.
#   "Carlisle Data with PMIDs and DOIs.xlsx" sheet "All Data" - his
#       stored trial p.value (RAW one-sided; the folded copy is the
#       2-sided column, never used here). The REPAIRED copy: the
#       pristine file's 542 Anesthesiology rows are column-shifted.
#
# THE JOIN (pilot's findings, carried verbatim):
#   - key = paste(full journal name, trial number); the One Sheet
#     prefixes map to the wide file's full names (including the literal
#     "Anesthesia &amp; Analgesia" HTML entity).
#   - Anesthesia & Analgesia numbering drift: One Sheet A&A trials
#     >= 1235 correspond to wide trial t+1 (the One Sheet sequence
#     skips ~1234). Applied here; the One Sheet file itself is NOT
#     repaired.
#   - the wide file's formula columns have no cached values - never
#     read them.
#
# WHAT "AGREES" MEANS: his stored values are effectively mid-p, which
# is what P_Calc has computed since 2026-08-17, so the comparison is
# direct. Trials where his p = 1 (a variable p of exactly 1 -> z = +inf
# under Stouffer) are a known artifact class (~12) reported separately.
#
# Usage:
#   Rscript corpus/validateCarlisle2017.R --pilot        (100 seeded trials, m = 15,000)
#   Rscript corpus/validateCarlisle2017.R                (all trials, m = 100,000)
# Resumable: per-trial results accumulate in
# .NewCarlisle/validation2017/results.csv; rerunning skips done trials.
# Runtime state (RNG) is NOT resumable mid-trial - each trial is atomic.

suppressWarnings(suppressPackageStartupMessages({
  library(shiny); library(openxlsx); library(Rfast); library(foreach)
  library(MBESS); library(dqrng)
}))
options(ECHO_OUTPUT_COMMENTS = NA)   # P_Calc narrates; a 5,088-trial run must not

args  <- commandArgs(trailingOnly = TRUE)
pilot <- "--pilot" %in% args
mMax  <- if (pilot) 15000 else 100000
root  <- "C:/dev/IntegrityAnalysis"
outDir <- file.path(root, ".NewCarlisle", "validation2017")
dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
resPath <- file.path(outDir, if (pilot) "pilot.csv" else "results.csv")

pkgload::load_all(root, quiet = TRUE)

## ------------------------------------------------------------ the data

os <- read.xlsx(file.path(root, "One Sheet Carlisle Data.xlsx"))
cat("One Sheet rows:", nrow(os), "\n")

wide <- read.xlsx(file.path(root, "Carlisle Data with PMIDs and DOIs.xlsx"),
                  sheet = "All Data")
wide$p <- suppressWarnings(as.numeric(wide$p.value))
wide$key <- paste(wide$Journal, wide$trial)
cat("Wide trials:", nrow(wide), "\n")

# One Sheet TRIAL -> wide key. Prefix = TRIAL minus its trailing number.
osTrial <- as.character(os$TRIAL)
num <- as.integer(gsub("[^0-9]", "", osTrial))   # prefixes carry no digits
prefix <- trimws(gsub("[0-9 ]+$", "", osTrial))
prefMap <- c(
  "Anaesthesia"              = "Anaesthesia",
  "Anesthesiology"           = "Anesthesiology",
  "Anesthesia and Analgesia" = "Anesthesia &amp; Analgesia",
  "BJA"  = "British Journal of Anaesthesia",
  "CJA"  = "Canadian Journal of Anesthesia",
  "EJA"  = "European Journal of Anesthesia",
  "NEJM" = "New England Journal of Medicine",
  "JAMA" = "Journal of the American Medical Association")
unknown <- setdiff(unique(prefix), names(prefMap))
if (length(unknown))
  stop("unmapped One Sheet journal prefix(es): ",
       paste(unknown, collapse = ", "))
# the A&A numbering drift (see header)
shift <- prefix == "Anesthesia and Analgesia" & num >= 1235
num[shift] <- num[shift] + 1L
osKey <- paste(prefMap[prefix], num)

## ------------------------------------------------------- validate once

v <- shiny::isolate(validateData(os))
if (isTRUE(v$FAIL)) stop("validateData refused the One Sheet data")
DATA <- v$DATA
# validateData may relabel trials; carry the wide-file key by original
# TRIAL string (validateData preserves TRIAL values for this input)
keyOf <- setNames(osKey, osTrial)
trials <- unique(as.character(DATA$TRIAL))
cat("Validated:", nrow(DATA), "rows,", length(trials), "trials\n")

if (pilot) {
  set.seed(20260816)          # the pilot's spirit: a fixed random 100
  trials <- sample(trials, 100)
}

done <- if (file.exists(resPath)) {
  read.csv(resPath, colClasses = "character")
} else {
  data.frame(TRIAL = character(), ours = character(),
             carlisle = character(), stringsAsFactors = FALSE)
}
todo <- setdiff(trials, done$TRIAL)
cat("Already computed:", nrow(done), " To do:", length(todo), "\n")

## ------------------------------------------------------------- the run

# One fixed seed pair for the whole run; per-trial draws then follow the
# engine's own stream. Reproducible end to end by rerunning from empty.
dqrng::dqset.seed(2017); set.seed(2017)

t0 <- Sys.time()
for (i in seq_along(todo)) {
  tr <- todo[i]
  out <- tryCatch(
    suppressWarnings(shiny::isolate(
      P_Calc(tr, DATA[DATA$TRIAL == tr, , drop = FALSE],
             v$CategoryNames, mMax))),
    error = function(e) NULL)
  ours <- if (is.null(out)) NA_real_ else {
    srow <- which(!is.na(out$ROW) & out$ROW == "Summary")
    suppressWarnings(as.numeric(out$P[srow[1]]))
  }
  done <- rbind(done, data.frame(
    TRIAL = tr, ours = as.character(ours),
    carlisle = as.character(wide$p[match(keyOf[[tr]], wide$key)]),
    stringsAsFactors = FALSE))
  if (i %% 25 == 0 || i == length(todo)) {
    write.csv(done, resPath, row.names = FALSE)
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    cat(sprintf("  %d/%d trials  (%.1f min elapsed, ~%.0f min left)\n",
                i, length(todo), el, el / i * (length(todo) - i)))
  }
}
write.csv(done, resPath, row.names = FALSE)

## ------------------------------------------------------------ the stats

ours <- suppressWarnings(as.numeric(done$ours))
his  <- suppressWarnings(as.numeric(done$carlisle))
ok   <- !is.na(ours) & !is.na(his)
art  <- ok & his >= 1            # the z = +inf artifact class
use  <- ok & !art
cat("\n== Carlisle 2017 validation (", if (pilot) "PILOT" else "FULL",
    ", mMax = ", mMax, ") ==\n", sep = "")
cat("joined trials:", sum(ok), " (his-p=1 artifacts:", sum(art),
    "; unjoined/refused:", nrow(done) - sum(ok), ")\n")
cat(sprintf("r = %.4f\n", cor(ours[use], his[use])))
cat(sprintf("median |diff| = %.4f\n", median(abs(ours[use] - his[use]))))
cat(sprintf("within 0.05: %.1f%%\n",
            100 * mean(abs(ours[use] - his[use]) <= 0.05)))
cat(sprintf("alarm concordance (p<0.05 both ways): %.1f%%\n",
            100 * mean((ours[use] < 0.05) == (his[use] < 0.05))))
cat("results:", resPath, "\n")
