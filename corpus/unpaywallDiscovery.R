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

root <- "C:/dev/IntegrityAnalysis"
outPath <- file.path(root, ".NewCarlisle", "unpaywall.csv")
dir.create(dirname(outPath), showWarnings = FALSE)

lookup <- read.xlsx(file.path(root, "Carlisle PMID to DOI lookup.xlsx"))
lookup <- lookup[!is.na(lookup$DOI) & nzchar(lookup$DOI), ]
lookup <- lookup[!duplicated(lookup$DOI), ]
cat("DOIs to query:", nrow(lookup), "\n")

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
