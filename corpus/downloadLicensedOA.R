# downloadLicensedOA.R - fetch the Carlisle papers whose open-access
# copies carry a license that PERMITS retrieval, wherever they live.
#
# PROVENANCE: written by Claude Code (model Claude Opus 5), 2026-08-19,
# at Steve Shafer's request, as the second (and last) automated pass.
# corpus/downloadNewCarlisle.R covered PMC's own open-access subset and
# found 5 papers. corpus/unpaywallDiscovery.R then showed that a further
# handful of articles are openly licensed somewhere ELSE - a university
# repository, a publisher's own site - which the PMC pass could never
# see. This script fetches those, and only those.
#
# WHAT "LICENSED" MEANS HERE, narrowly:
#   cc-by, cc-by-sa, cc-by-nc, cc-by-nc-nd, cc0, public-domain
# A Creative Commons or public-domain license is an explicit grant to
# copy the work, so a script may. Everything else is excluded, and one
# exclusion is worth naming: Unpaywall's **"other-oa"** (65 of the 126
# licensed-looking census rows) means "open access, license unstated" -
# free to READ, not licensed to retrieve. Those stay in the manual
# queue with bronze. Erring the other way would be helping ourselves to
# 65 papers on a technicality.
#
# WHY IT RE-QUERIES UNPAYWALL: the census stored only each DOI's BEST
# location, and that is usually a landing page (an /abstract URL, a
# repository handle) rather than a file. The full record lists every
# open location with its own license and, where one exists, a direct
# url_for_pdf. So this asks again for the ~60 candidate DOIs, keeps the
# locations whose OWN license is CC/public-domain, and tries their
# direct PDF links. A DOI with no licensed direct PDF is recorded as
# such and stays in the manual queue - no landing-page scraping.
#
# Usage:
#   "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" corpus/downloadLicensedOA.R
# Resumable: re-running skips DOIs already resolved in
# .NewCarlisle/licensed_manifest.csv. One request per second, and the
# User-Agent identifies the project, as Unpaywall asks.

suppressPackageStartupMessages({
  library(jsonlite)
})

args    <- commandArgs(trailingOnly = TRUE)
root    <- "C:/dev/IntegrityAnalysis"
absolute <- function(p) if (grepl("^([A-Za-z]:|/)", p)) p else file.path(root, p)
# Default .NewCarlisle; takes an argument so the same pass runs over the
# Boldt and Fujii corpora, which keep their own directories.
outDir  <- absolute(if (length(args) >= 1) args[1] else ".NewCarlisle")
manifestPath <- file.path(outDir, "licensed_manifest.csv")
dir.create(outDir, showWarnings = FALSE)

email <- "steven.shafer%40stanford.edu"
ua <- "IntegrityAnalysis/1.0 (steven.shafer@stanford.edu)"

# The license values that grant permission to copy. Matched exactly
# (after lower-casing): "other-oa" must never slip in through a prefix
# match, and a versioned CC string ("cc-by-4.0") is normalised first.
LICENSED <- c("cc0", "public-domain", "cc-by", "cc-by-sa", "cc-by-nc",
              "cc-by-nc-sa", "cc-by-nd", "cc-by-nc-nd")
isLicensed <- function(x) {
  x <- tolower(ifelse(is.na(x), "", as.character(x)))
  x <- sub("-[0-9].*$", "", x)          # "cc-by-4.0" -> "cc-by"
  x %in% LICENSED
}

pause <- function() Sys.sleep(1)

## Candidates ------------------------------------------------------------

censusPath <- file.path(outDir, "unpaywall.csv")
if (!file.exists(censusPath))
  stop("run corpus/unpaywallDiscovery.R first - no unpaywall.csv")
census <- read.csv(censusPath, colClasses = "character")
cand <- census[isLicensed(census$license), c("PMID", "DOI", "license")]
cand <- cand[!duplicated(cand$DOI), ]
cat("Census rows:", nrow(census),
    " with a permissive license:", nrow(cand), "\n")

manifest <- if (file.exists(manifestPath)) {
  read.csv(manifestPath, colClasses = "character")
} else {
  data.frame(PMID = character(), DOI = character(),
             census_license = character(), used_license = character(),
             status = character(), file = character(), url = character(),
             stringsAsFactors = FALSE)
}
todo <- cand[!cand$DOI %in% manifest$DOI, ]
cat("Already resolved:", nrow(manifest), " To do:", nrow(todo), "\n")

saveManifest <- function() write.csv(manifest, manifestPath,
                                     row.names = FALSE)

## Fetch -----------------------------------------------------------------

# A PDF, not an error page dressed as one: publishers and repositories
# happily return 200 with an HTML "choose your institution" page, so
# check the magic bytes rather than trusting the extension.
isPdf <- function(path) {
  if (!file.exists(path) || file.size(path) < 10000) return(FALSE)
  con <- file(path, "rb"); on.exit(close(con))
  identical(rawToChar(readBin(con, "raw", 5)), "%PDF-")
}

# Every open location Unpaywall knows for this DOI whose OWN license
# permits copying and which offers a direct file, best first (Unpaywall
# already orders oa_locations by quality).
licensedPdfUrls <- function(doi) {
  u <- paste0("https://api.unpaywall.org/v2/",
              utils::URLencode(doi, reserved = TRUE), "?email=", email)
  j <- tryCatch(fromJSON(u), error = function(e) NULL)
  if (is.null(j) || is.null(j$oa_locations) || !is.data.frame(j$oa_locations))
    return(NULL)
  loc <- j$oa_locations
  if (!"license" %in% names(loc)) return(NULL)
  keep <- isLicensed(loc$license)
  if (!any(keep)) return(NULL)
  loc <- loc[keep, , drop = FALSE]
  url <- if ("url_for_pdf" %in% names(loc)) loc$url_for_pdf else NA_character_
  ok <- !is.na(url) & nzchar(url)
  if (!any(ok)) return(NULL)
  data.frame(url = url[ok], license = loc$license[ok],
             stringsAsFactors = FALSE)
}

n <- 0
for (i in seq_len(nrow(todo))) {
  pmid <- todo$PMID[i]
  dest <- file.path(outDir, paste0("PMID_", pmid, ".pdf"))
  row <- data.frame(PMID = pmid, DOI = todo$DOI[i],
                    census_license = todo$license[i], used_license = "",
                    status = "", file = "", url = "",
                    stringsAsFactors = FALSE)
  n <- n + 1

  if (file.exists(dest)) {
    # The PMC pass already got this one.
    row$status <- "already_have"; row$file <- basename(dest)
  } else {
    cands <- licensedPdfUrls(todo$DOI[i]); pause()
    if (is.null(cands)) {
      row$status <- "no_licensed_pdf_url"
    } else {
      for (k in seq_len(nrow(cands))) {
        ok <- tryCatch({
          suppressWarnings(download.file(
            cands$url[k], dest, mode = "wb", quiet = TRUE,
            headers = c("User-Agent" = ua))); TRUE
        }, error = function(e) FALSE)
        pause()
        if (ok && isPdf(dest)) {
          row$status <- "downloaded"; row$file <- basename(dest)
          row$url <- cands$url[k]; row$used_license <- cands$license[k]
          break
        }
        unlink(dest)
        row$status <- "download_failed"; row$url <- cands$url[k]
      }
    }
  }

  manifest <- rbind(manifest, row)
  if (n %% 10 == 0) {
    saveManifest()
    cat(sprintf("  %d/%d  downloaded so far: %d\n", n, nrow(todo),
                sum(manifest$status == "downloaded")))
  }
}
saveManifest()

cat("\n== Summary ==\n")
print(table(manifest$status))
cat("PDFs in", outDir, ":", length(list.files(outDir, "[.]pdf$")), "\n")
cat("\nRerun corpus/buildDownloadList.R to fold these into the queue.\n")
