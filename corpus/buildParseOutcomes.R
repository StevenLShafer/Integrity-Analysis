# buildParseOutcomes.R — regenerate the parser master sheet over a corpus
# of trial PDFs.
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-17,
# at Steve Shafer's request, as part of folding ParsePDF into this
# repository (ISSUES.md issue 9). Purpose and workflow: see
# corpus/README.md and AGENTS.md "The parser optimization loop".
#
# Usage:
#   Rscript corpus/buildParseOutcomes.R <corpusDir> <workDir> [chunkSize]
#
#   corpusDir  folder tree of article PDFs (e.g. C:/temp/journals)
#   workDir    where per-chunk .rds intermediates live; the run is
#              RESUMABLE - existing chunks are skipped, so a crash or an
#              interrupt costs at most one chunk
#   chunkSize  PDFs per chunk (default 100)
#
# Output: corpus/ParseOutcomes.csv next to this script - one row per PDF:
#   PDF        path relative to corpusDir
#   OUTCOME    "successfully parsed" / "not successfully parsed"
#              (successfully parsed = the deterministic engine returned a
#              baseline table; the AI fallback is never used here, so the
#              sheet measures exactly the code in R/)
#   COMMENTS   success: the steps (table page, layout, arms and how many
#              carry an N, lines -> variables, continuous rows, skipped
#              lines, runtime); failure: where the process stopped
#   plus the raw diagnostic columns (PAGE, LAYOUT, ARMS, ARMS_WITH_N,
#   LINES, VARIABLES, CONTINUOUS, SKIPPED, SECONDS) so analyses do not
#   have to re-parse COMMENTS.
#
# Every PDF goes through parseBaselineTableFiles() - one subprocess per
# file with an OS-level timeout - NEVER a plain loop: roughly 2% of real
# PDFs hang poppler indefinitely and R cannot interrupt it in-process.

suppressMessages(library(IntegrityAnalysis))

a         <- commandArgs(trailingOnly = TRUE)
corpusDir <- a[1]
workDir   <- a[2]
chunk     <- if (length(a) >= 3) as.integer(a[3]) else 100L
dir.create(workDir, showWarnings = FALSE, recursive = TRUE)

pdfs <- list.files(corpusDir, pattern = "[.]pdf$", full.names = TRUE,
                   recursive = TRUE)
cat("corpus:", length(pdfs), "PDFs; chunks of", chunk, "\n", file = stderr())

grp <- split(pdfs, ceiling(seq_along(pdfs) / chunk))
for (k in seq_along(grp)) {
  sumFile <- file.path(workDir, sprintf("outcomes_%03d.rds", k))
  if (file.exists(sumFile)) next
  res <- parseBaselineTableFiles(grp[[k]], ai = "never", timeout = 40,
                                 quiet = TRUE)
  res$path <- grp[[k]]
  res$result <- NULL
  saveRDS(res, sumFile)
  cat(sprintf("chunk %d/%d done\n", k, length(grp)), file = stderr())
}

S <- do.call(rbind, lapply(sort(list.files(workDir, "^outcomes_.*rds$",
                                           full.names = TRUE)), readRDS))

# The comments column: a human- and LLM-readable narrative per PDF.
comment <- function(r) {
  if (isTRUE(r$ok)) {
    paste0(
      "Table page ", r$page, " (", r$layout, " layout); ",
      r$arms, " arm(s), ", r$armsWithN, " with N; ",
      r$lines, " table lines -> ", r$variables, " variables (",
      r$continuous, " continuous rows); ",
      r$skipped, " line(s) skipped as unusable; ",
      r$engine, " engine, ", sprintf("%.1f", r$seconds), " s")
  } else {
    msg <- r$error
    stage <- if (is.na(r$page))
      "Failed at table-page identification: " else
      paste0("Table page ", r$page, " identified, but parsing failed: ")
    paste0(stage, if (is.na(msg)) "(no error message)" else msg)
  }
}
S$COMMENTS <- vapply(seq_len(nrow(S)), function(i) comment(S[i, ]),
                     character(1))
out <- data.frame(
  PDF        = sub(paste0("^", gsub("([\\^$.|?*+()\\[\\]{}])", "\\\\\\1",
                                    normalizePath(corpusDir, winslash = "/")),
                          "/?"), "",
                   normalizePath(S$path, winslash = "/", mustWork = FALSE)),
  OUTCOME    = ifelse(S$ok, "successfully parsed", "not successfully parsed"),
  COMMENTS   = S$COMMENTS,
  PAGE       = S$page,       LAYOUT     = S$layout,
  ARMS       = S$arms,       ARMS_WITH_N = S$armsWithN,
  LINES      = S$lines,      VARIABLES  = S$variables,
  CONTINUOUS = S$continuous, SKIPPED    = S$skipped,
  SECONDS    = S$seconds,
  stringsAsFactors = FALSE)

# PMID column: from the filename where the file is named PMID_<n>.pdf,
# otherwise from corpus/pmid_map.csv (the committed lookup - it holds the
# PMIDs recovered by OCR of printed citation lines for scanned papers,
# which no regeneration could rediscover from the outcomes alone).
out$PMID <- NA_character_
i <- grepl("^PMID_\\d+\\.pdf$", basename(out$PDF))
out$PMID[i] <- sub("^PMID_(\\d+)\\.pdf$", "\\1", basename(out$PDF[i]))
pmidMapPath <- "pmid_map.csv"
selfDir <- dirname(sub("--file=", "",
             grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
if (!is.na(selfDir) && nzchar(selfDir))
  pmidMapPath <- file.path(selfDir, "pmid_map.csv")
if (file.exists(pmidMapPath)) {
  pm <- read.csv(pmidMapPath, stringsAsFactors = FALSE,
                 colClasses = "character")
  j <- match(out$PDF, pm$PDF)
  out$PMID[is.na(out$PMID) & !is.na(j)] <-
    pm$PMID[j[is.na(out$PMID) & !is.na(j)]]
}
out <- out[, c("PDF", "PMID", setdiff(names(out), c("PDF", "PMID")))]
out <- out[order(out$PDF), ]

dest <- file.path(dirname(sub("--file=", "",
        grep("^--file=", commandArgs(FALSE), value = TRUE)[1])),
        "ParseOutcomes.csv")
if (is.na(dest) || !nzchar(dirname(dest))) dest <- "ParseOutcomes.csv"
write.csv(out, dest, row.names = FALSE)
cat("written:", dest, "-", nrow(out), "PDFs;",
    sum(out$OUTCOME == "successfully parsed"), "parsed\n", file = stderr())
