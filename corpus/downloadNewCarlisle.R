# downloadNewCarlisle.R - fetch the Carlisle papers that are LEGALLY
# bulk-downloadable, into C:/dev/IntegrityAnalysis/.NewCarlisle.
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-19,
# at Steve Shafer's request, to rebuild the ParsePDF corpus from better
# sources than the old local scans (pre-2002 Anesthesiology PDFs are
# photographs of physical journals, folds and all).
#
# WHAT THIS DOWNLOADS - AND DELIBERATELY DOES NOT:
# PubMed hosts no full text; "free on PubMed" means PubMed Central. PMC
# splits into (a) the OPEN ACCESS SUBSET, explicitly licensed for bulk
# retrieval through PMC's OA service - that is what this script
# downloads - and (b) "free to read" deposits, which PMC's terms forbid
# retrieving in bulk (the same category as Lane Library's "no").
# Publisher free-after-embargo archives are likewise excluded: their
# terms of use ban systematic downloading. Every PMID's outcome is
# recorded in .NewCarlisle/manifest.csv so the coverage - and the gap -
# is explicit.
#
# Usage:
#   "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" corpus/downloadNewCarlisle.R
# Resumable: re-running skips PMIDs already resolved in the manifest.
# NCBI etiquette: <= 3 requests/second without an API key (set
# NCBI_API_KEY for 10/s); this script sleeps between requests.

suppressPackageStartupMessages({
  library(rentrez)
  library(openxlsx)
  library(xml2)
})

root   <- "C:/dev/IntegrityAnalysis"
outDir <- file.path(root, ".NewCarlisle")
dir.create(outDir, showWarnings = FALSE)
manifestPath <- file.path(outDir, "manifest.csv")

lookup <- read.xlsx(file.path(root, "Carlisle PMID to DOI lookup.xlsx"))
pmids <- unique(as.character(lookup$PMID))
pmids <- pmids[!is.na(pmids) & nzchar(pmids)]
cat("PMIDs in the lookup:", length(pmids), "\n")

manifest <- if (file.exists(manifestPath)) {
  m <- read.csv(manifestPath, colClasses = "character")
  if (!"license" %in% names(m)) m$license <- ""
  m
} else {
  data.frame(PMID = character(), PMCID = character(),
             status = character(), file = character(),
             license = character(), stringsAsFactors = FALSE)
}
todo <- setdiff(pmids, manifest$PMID)
cat("Already resolved:", nrow(manifest), " To do:", length(todo), "\n")

saveManifest <- function() write.csv(manifest, manifestPath,
                                     row.names = FALSE)
# No rush (Steve, 2026-08-19): pace well below NCBI's ceiling - one
# request per second - and be identifiable (tool/email on idconv).
pause <- function() Sys.sleep(1)

## Phase 1 - map PMIDs to PMC IDs (NCBI ID Converter, batched) ------------
# The ID Converter API is the purpose-built batch tool for this mapping
# (up to 200 ids per request, keyed responses - no positional
# assumptions, unlike elink's by_id list, which broke here).
cat("\n== Phase 1: PubMed -> PMC id mapping ==\n")
pmcOf <- setNames(rep(NA_character_, length(todo)), todo)
chunks <- split(todo, ceiling(seq_along(todo) / 200))
for (ci in seq_along(chunks)) {
  ch <- chunks[[ci]]
  u <- paste0(
    "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/",
    "?tool=IntegrityAnalysis&email=steven.shafer%40stanford.edu",
    "&format=json&ids=", paste(ch, collapse = ","))
  j <- tryCatch(jsonlite::fromJSON(u),
                error = function(e) { message("idconv chunk ", ci, ": ",
                                              e$message); NULL })
  if (!is.null(j) && !is.null(j$records) && "pmcid" %in% names(j$records)) {
    r <- j$records
    hit <- !is.na(r$pmcid) & nzchar(r$pmcid)
    pmcOf[as.character(r$pmid[hit])] <- r$pmcid[hit]
  }
  cat(sprintf("  chunk %d/%d - PMC ids so far: %d\n", ci, length(chunks),
              sum(!is.na(pmcOf))))
  pause()
}
cat("PMIDs with a PMC record:", sum(!is.na(pmcOf)), "of", length(todo), "\n")

## Phase 2 - OA-subset check and download --------------------------------
cat("\n== Phase 2: PMC Open Access subset check + download ==\n")
oaCheck <- function(pmcid) {
  u <- paste0("https://www.ncbi.nlm.nih.gov/pmc/utils/oa/oa.fcgi?id=", pmcid)
  x <- tryCatch(read_xml(u), error = function(e) NULL)
  if (is.null(x)) return(list(status = "oa_service_error"))
  err <- xml_find_first(x, ".//error")
  if (!inherits(err, "xml_missing"))
    return(list(status = paste0("not_in_oa_subset")))
  links <- xml_find_all(x, ".//link")
  fmt  <- xml_attr(links, "format")
  href <- xml_attr(links, "href")
  href <- sub("^ftp://ftp\\.ncbi\\.nlm\\.nih\\.gov",
              "https://ftp.ncbi.nlm.nih.gov", href)
  # PMC's per-article PDF paths on the FTP are often stale (404) - keep
  # BOTH links and let the fetcher fall back from pdf to the tgz package
  rec <- xml_find_first(x, ".//record")
  list(status = "oa",
       license = if (!inherits(rec, "xml_missing"))
                   xml_attr(rec, "license") else "",
       pdf = if ("pdf" %in% fmt) href[match("pdf", fmt)] else NULL,
       tgz = if ("tgz" %in% fmt) href[match("tgz", fmt)] else NULL)
}

# fetch order: PMC FTP pdf -> PMC FTP tgz package -> Europe PMC render.
# The FTP hrefs that oa.fcgi reports are stale (404) for many older
# records; Europe PMC mirrors the OA subset and supports programmatic
# retrieval, so it is the reliable last resort for the same
# OA-licensed article.
fetchPdf <- function(pmid, pmcid, oa) {
  dest <- file.path(outDir, paste0("PMID_", pmid, ".pdf"))
  if (!is.null(oa$pdf)) {
    ok <- tryCatch({
      suppressWarnings(download.file(oa$pdf, dest, mode = "wb",
                                     quiet = TRUE)); TRUE
    }, error = function(e) FALSE)
    if (ok && file.exists(dest) && file.size(dest) > 10000) return(dest)
    unlink(dest)
    pause()
  }
  if (!is.null(oa$tgz)) {
    tgz <- tempfile(fileext = ".tgz")
    ok <- tryCatch({
      suppressWarnings(download.file(oa$tgz, tgz, mode = "wb",
                                     quiet = TRUE)); TRUE
    }, error = function(e) FALSE)
    if (ok) {
      got <- extractPdfFromTgz(tgz, dest)
      if (!is.na(got)) return(got)
    }
    pause()
  }
  # Europe PMC mirror of the same OA article
  ok <- tryCatch({
    suppressWarnings(download.file(
      paste0("https://europepmc.org/articles/", pmcid, "?pdf=render"),
      dest, mode = "wb", quiet = TRUE,
      headers = c("User-Agent" =
        "IntegrityAnalysis/1.0 (steven.shafer@stanford.edu)"))); TRUE
  }, error = function(e) FALSE)
  if (ok && file.exists(dest) && file.size(dest) > 10000) return(dest)
  unlink(dest)
  NA_character_
}

extractPdfFromTgz <- function(tgz, dest) {
  exd <- tempfile(); dir.create(exd)
  files <- tryCatch(untar(tgz, list = TRUE),
                    error = function(e) character(0))
  pdfs <- files[grepl("\\.pdf$", files, ignore.case = TRUE)]
  if (length(pdfs) == 0) { unlink(c(tgz, exd), recursive = TRUE)
                           return(NA_character_) }
  untar(tgz, files = pdfs, exdir = exd)
  sizes <- file.size(file.path(exd, pdfs))
  file.copy(file.path(exd, pdfs[which.max(sizes)]), dest, overwrite = TRUE)
  unlink(c(tgz, exd), recursive = TRUE)
  if (file.exists(dest) && file.size(dest) > 10000) dest else NA_character_
}

n <- 0
for (pmid in todo) {
  n <- n + 1
  pmcid <- pmcOf[[pmid]]
  row <- data.frame(PMID = pmid, PMCID = ifelse(is.na(pmcid), "", pmcid),
                    status = "", file = "", license = "",
                    stringsAsFactors = FALSE)
  if (is.na(pmcid)) {
    row$status <- "no_pmc_record"
  } else {
    oa <- oaCheck(pmcid); pause()
    if (oa$status == "oa") {
      f <- fetchPdf(pmid, pmcid, oa); pause()
      row$license <- oa$license
      if (!is.na(f)) { row$status <- "downloaded"; row$file <- basename(f) }
      else row$status <- "download_failed"
    } else row$status <- oa$status
  }
  manifest <- rbind(manifest, row)
  if (n %% 25 == 0) {
    saveManifest()
    cat(sprintf("  %d/%d  downloaded so far: %d\n", n, length(todo),
                sum(manifest$status == "downloaded")))
  }
}
saveManifest()

cat("\n== Summary ==\n")
print(table(manifest$status))
cat("PDFs in", outDir, ":",
    length(list.files(outDir, "[.]pdf$")), "\n")
