# runMassTest.R - the resumable mass-test runner for corpus/TEST.
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-17,
# at Steve Shafer's request (his item 3). This is the answer to "should
# we add a testing flag that stores intermediate results?": rather than a
# flag inside the Shiny app (which would sit awkwardly beside the
# purge-on-exit guarantee, and still lose everything if the BROWSER
# session dies), the mass test runs headlessly with a CHECKPOINT PER PDF.
# A crash after hours costs at most one PDF; re-running resumes where it
# stopped. The pipeline is exactly the app's: parseBaselineTableFiles ->
# validateData -> P_Calc, so results are comparable to an in-app run.
#
# Usage:
#   Rscript corpus/runMassTest.R [workDir] [mode]
#     workDir  checkpoint directory (default C:/temp/MassTest_work);
#              existing checkpoints are SKIPPED - delete the directory
#              for a fresh run.
#     mode     "all" (default) or "continuous": continuous drops the
#              categorical rows before analysis, matching the scope of
#              Carlisle's stored p-values (continuous variables only) for
#              an apples-to-apples comparison (Steve, 2026-08-18). Output
#              goes to ActualResults_continuous.xlsx in that mode.
# Output: corpus/ActualResults.xlsx - per-line and per-trial one-sided P
# for every TEST PDF, ready to compare against corpus/ExpectedResults.xlsx.

# Guard against the wrong R: this machine's default Rscript is R 4.6,
# whose library is nearly empty. The package lives in the R 4.5.3 user
# library (AGENTS.md runbook). runMassTest.bat pins the right one.
if (!requireNamespace("IntegrityAnalysis", quietly = TRUE))
  stop("IntegrityAnalysis is not installed in THIS R (",
       R.version.string, ").\nRun with R 4.5.3 instead:\n",
       '  "C:\\Program Files\\R\\R-4.5.3\\bin\\Rscript.exe" ',
       "corpus/runMassTest.R\nor double-click corpus\\runMassTest.bat")
suppressWarnings(suppressPackageStartupMessages({
  library(IntegrityAnalysis); library(shiny)
  library(openxlsx); library(Rfast); library(foreach); library(MBESS)
  library(dqrng)
}))
a <- commandArgs(TRUE)
workDir <- if (length(a) >= 1) a[1] else "C:/temp/MassTest_work"
mode <- if (length(a) >= 2) a[2] else "all"
stopifnot(mode %in% c("all", "continuous"))
dir.create(workDir, showWarnings = FALSE, recursive = TRUE)
testDir <- file.path("C:/dev/IntegrityAnalysis", "corpus", "TEST")
pdfs <- list.files(testDir, "[.]pdf$", full.names = TRUE)
cat("TEST corpus:", length(pdfs), "PDFs; checkpoints in", workDir, "\n")

t0 <- Sys.time()
for (i in seq_along(pdfs)) {
  f <- pdfs[i]
  ck <- file.path(workDir, paste0(tools::file_path_sans_ext(basename(f)),
                                  ".rds"))
  if (file.exists(ck)) next
  res <- parseBaselineTableFiles(f, ai = "never", timeout = 60,
                                 quiet = TRUE)
  r <- res$result[[1]]
  out <- if (is.null(r) || nrow(r$data) == 0) {
    list(file = basename(f), ok = FALSE, error = res$error[1])
  } else {
    d <- r$data
    d$TRIAL <- tools::file_path_sans_ext(basename(f))
    v <- shiny::isolate(IntegrityAnalysis:::validateData(d))
    if (v$FAIL) {
      list(file = basename(f), ok = FALSE,
           error = "parsed table failed validation")
    } else {
      DATA <- v$DATA
      catNames <- v$CategoryNames
      if (mode == "continuous") {
        # Carlisle's stored p covers continuous variables only; in the
        # validated table, categorical lines are exactly those with NA
        # MEAN. Drop them and analyze with no category columns so the
        # comparison is scope-matched.
        DATA <- DATA[!is.na(DATA$MEAN), , drop = FALSE]
        catNames <- NULL
      }
      if (nrow(DATA) == 0) {
        list(file = basename(f), ok = FALSE,
             error = "no continuous rows after filtering")
      } else {
      x <- suppressWarnings(shiny::isolate(IntegrityAnalysis:::P_Calc(
        v$TRIALS[1], DATA, catNames, IntegrityAnalysis:::m)))
      list(file = basename(f), ok = TRUE, result = x)
      }
    }
  }
  saveRDS(out, ck)
  cat(sprintf("%d/%d %s %s  (%.1f min elapsed)\n", i, length(pdfs),
              basename(f), if (isTRUE(out$ok)) "ok" else "FAILED",
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

## assemble ActualResults.xlsx --------------------------------------------
cks <- list.files(workDir, "[.]rds$", full.names = TRUE)
lines <- list(); trials <- list()
for (ck in cks) {
  o <- readRDS(ck)
  if (!isTRUE(o$ok)) {
    trials[[length(trials) + 1]] <- data.frame(
      FILE = o$file, P_TRIAL = NA_real_, STATUS = o$error,
      stringsAsFactors = FALSE)
    next
  }
  x <- o$result
  keep <- !x$ROW %in% c("Summary") & !is.na(x$ROW)
  lines[[length(lines) + 1]] <- data.frame(
    FILE = o$file, ROW = x$ROW[keep],
    P = suppressWarnings(as.numeric(x$P[keep])), stringsAsFactors = FALSE)
  trials[[length(trials) + 1]] <- data.frame(
    FILE = o$file,
    P_TRIAL = suppressWarnings(as.numeric(
      x$P[x$ROW == "Summary" & !is.na(x$ROW)]))[1],
    STATUS = "analyzed", stringsAsFactors = FALSE)
}
wb <- createWorkbook()
addWorksheet(wb, "Lines");  writeData(wb, "Lines",  do.call(rbind, lines))
addWorksheet(wb, "Trials"); writeData(wb, "Trials", do.call(rbind, trials))
outName <- if (mode == "continuous") "ActualResults_continuous.xlsx"
           else "ActualResults.xlsx"
saveWorkbook(wb, file.path("C:/dev/IntegrityAnalysis", "corpus",
                           outName), overwrite = TRUE)
cat("written corpus/", outName, " -",
    sum(vapply(trials, function(t) t$STATUS[1] == "analyzed", logical(1))),
    "of", length(cks), "analyzed\nMASS TEST DONE\n")
