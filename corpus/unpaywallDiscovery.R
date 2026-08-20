# unpaywallDiscovery.R - METADATA-ONLY pass over the Carlisle DOIs:
# where does a legal open copy of each paper live, and under what
# license? Downloads nothing; writes .NewCarlisle/unpaywall.csv.
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-19.
# Unpaywall is the standard index of LEGAL open-access locations,
# built exactly for this question. Its API asks for an email and fair
# pacing; this script sends one request per second. The output
# distinguishes licensed copies (a CC license somewhere - candidates
# for legitimate retrieval) from "bronze" (free to read on the
# publisher's site, no license - reading is fine, systematic
# downloading is not).
#
# Usage:
#   "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" corpus/unpaywallDiscovery.R
# Resumable: skips DOIs already in the output.

suppressPackageStartupMessages({
  library(openxlsx)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
root <- "C:/dev/IntegrityAnalysis"
absolute <- function(p) if (grepl("^([A-Za-z]:|/)", p)) p else file.path(root, p)
# Default: the Carlisle lookup, which this script was written for. It
# takes arguments so the same census can run over the Boldt and Fujii
# lists (2026-08-19), whose PMIDs corpus/resolveCitationList.R recovers.
srcArg <- absolute(if (length(args) >= 1) args[1] else
  "Carlisle PMID to DOI lookup.xlsx")
outDir <- absolute(if (length(args) >= 2) args[2] else ".NewCarlisle")
outPath <- file.path(outDir, "unpaywall.csv")
dir.create(outDir, showWarnings = FALSE)

lookup <- if (grepl("[.]csv$", srcArg, ignore.case = TRUE))
  read.csv(srcArg, colClasses = "character") else read.xlsx(srcArg)
lookup$PMID <- as.character(lookup$PMID)
lookup <- lookup[!is.na(lookup$PMID) & nzchar(lookup$PMID) &
                   lookup$PMID != "NA", ]

# Unpaywall is keyed on DOI, and the fraud lists arrive with PMIDs only
# (their source spreadsheets carry neither). PubMed knows the DOI, so
# fill the gap here rather than making every caller do it: esummary in
# batches of 200, one request per second.
if (!"DOI" %in% names(lookup)) lookup$DOI <- ""
lookup$DOI <- ifelse(is.na(lookup$DOI), "", lookup$DOI)
need <- !nzchar(lookup$DOI)
if (any(need)) {
  cat("DOIs to look up from PubMed:", sum(need), "\n")
  ids <- unique(lookup$PMID[need])
  found <- setNames(rep("", length(ids)), ids)
  # Batches of 50 with retries, not one big request: a single transient
  # failure once wiped out an entire 185-PMID batch and the census then
  # ran on zero DOIs, reporting "nothing open access" for a corpus that
  # simply had not been looked up. Small batches bound that damage, the
  # retry usually removes it, and a batch that still fails says so.
  failed <- 0
  for (ch in split(ids, ceiling(seq_along(ids) / 50))) {
    u <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi",
                "?db=pubmed&retmode=json&tool=IntegrityAnalysis",
                "&email=steven.shafer%40stanford.edu&id=",
                paste(ch, collapse = ","))
    j <- NULL
    for (attempt in 1:3) {
      j <- tryCatch(fromJSON(u, simplifyVector = FALSE),
                    error = function(e) NULL)
      if (!is.null(j) && !is.null(j$result)) break
      Sys.sleep(2)
    }
    if (is.null(j) || is.null(j$result)) {
      failed <- failed + length(ch)
      cat("  !! esummary failed for a batch of", length(ch), "PMIDs\n")
      next
    }
    for (id in ch) {
      r <- j$result[[id]]
      if (is.null(r) || is.null(r$articleids)) next
      for (a in r$articleids)
        if (identical(a$idtype, "doi")) found[[id]] <- a$value
    }
    cat("  DOIs found so far:", sum(nzchar(found)), "/", length(ids), "\n")
    Sys.sleep(1)
  }
  if (failed > 0)
    cat("WARNING:", failed, "PMIDs could not be looked up - rerun to retry\n")
  lookup$DOI[need] <- found[lookup$PMID[need]]
}

lookup <- lookup[!is.na(lookup$DOI) & nzchar(lookup$DOI), ]
lookup <- lookup[!duplicated(lookup$DOI), ]
cat("Source:", srcArg, "\nOutput:", outPath,
    "\nDOIs to query:", nrow(lookup), "\n")

done <- if (file.exists(outPath)) {
  read.csv(outPath, colClasses = "character")
} else {
  data.frame(PMID = character(), DOI = character(),
             is_oa = character(), oa_status = character(),
             license = character(), host_type = character(),
             url = character(), stringsAsFactors = FALSE)
}
todo <- lookup[!lookup$DOI %in% done$DOI, ]
cat("Already queried:", nrow(done), " To do:", nrow(todo), "\n")

n <- 0
for (i in seq_len(nrow(todo))) {
  doi <- todo$DOI[i]
  u <- paste0("https://api.unpaywall.org/v2/",
              utils::URLencode(doi, reserved = TRUE),
              "?email=steven.shafer%40stanford.edu")
  j <- tryCatch(fromJSON(u), error = function(e) NULL)
  row <- data.frame(
    PMID = as.character(todo$PMID[i]), DOI = doi,
    is_oa = "", oa_status = "", license = "", host_type = "", url = "",
    stringsAsFactors = FALSE)
  if (is.null(j)) {
    row$is_oa <- "query_failed"
  } else {
    row$is_oa <- as.character(isTRUE(j$is_oa))
    row$oa_status <- ifelse(is.null(j$oa_status), "", j$oa_status)
    b <- j$best_oa_location
    if (!is.null(b)) {
      row$license   <- ifelse(is.null(b$license) || is.na(b$license),
                              "", b$license)
      row$host_type <- ifelse(is.null(b$host_type), "", b$host_type)
      row$url       <- ifelse(is.null(b$url_for_pdf) ||
                                is.na(b$url_for_pdf),
                              ifelse(is.null(b$url), "", b$url),
                              b$url_for_pdf)
    }
  }
  done <- rbind(done, row)
  n <- n + 1
  if (n %% 50 == 0) {
    write.csv(done, outPath, row.names = FALSE)
    cat(sprintf("  %d/%d  oa so far: %d (licensed: %d)\n", n, nrow(todo),
                sum(done$is_oa == "TRUE"),
                sum(nzchar(done$license))))
  }
  Sys.sleep(1)
}
write.csv(done, outPath, row.names = FALSE)

cat("\n== Summary ==\n")
cat("OA anywhere:", sum(done$is_oa == "TRUE"), "of", nrow(done), "\n")
cat("\nBy oa_status:\n"); print(table(done$oa_status, useNA = "ifany"))
cat("\nBy license (best location):\n")
print(table(ifelse(nzchar(done$license), done$license, "(none)")))
cat("\nBy host type:\n")
print(table(ifelse(nzchar(done$host_type), done$host_type, "(none)")))
