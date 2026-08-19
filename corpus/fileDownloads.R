# fileDownloads.R - file the PDFs Steve downloads by hand into
# .NewCarlisle under the names the pipeline expects.
#
# PROVENANCE: written by Claude Code (model Claude Opus 5), 2026-08-19,
# at Steve Shafer's request the first time he worked the queue: fifteen
# papers landed in his Downloads folder carrying publisher names
# ("NEJMoa063186.pdf", "1-s2.0-S0007091217363808-main.pdf",
# "the-dose-range-effects-of-propofol-....pdf"), and every one of them
# has to become PMID_<pmid>.pdf. Renaming a handful a day by hand is
# exactly the sort of clerical work that goes wrong quietly, so this
# identifies each file from its own contents instead.
#
# HOW A FILE IS IDENTIFIED, best evidence first:
#   1. a DOI in the PDF's text, matched against the master sheet;
#   2. a PMID printed in the text (PubMed stamps many publisher PDFs);
#   3. the DOI recovered from the file NAME (publishers name files after
#      the DOI suffix: NEJMoa063186 -> 10.1056/NEJMoa063186);
#   4. the title: the longest line of page 1 matched against the
#      citations in the master sheet, accepted only on a strong and
#      UNAMBIGUOUS match (one candidate clearly ahead of the rest).
# Anything still unidentified is left where it is and listed, with what
# was found, so a human can place it. Guessing is the one thing this
# must not do: a mis-filed PDF becomes a wrong baseline table later.
#
# Usage:
#   "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" corpus/fileDownloads.R [sourceDir] [--copy] [--dry-run]
#
#   sourceDir   folder to file FROM. Default .NewCarlisle/inbox - drop
#               the day's downloads there and run this. The default is
#               deliberately NOT the Downloads folder: Steve reviews for
#               many journals, so that folder holds hundreds of PDFs,
#               among them CONFIDENTIAL MANUSCRIPTS this script has no
#               business opening (and reading them all is slow). A
#               holding folder keeps the two apart.
#   --days N    ignore PDFs older than N days. Default 0 (no limit),
#               which is right for a dedicated inbox; pass it when
#               pointing this at a busier folder.
#   --copy      copy instead of moving (default is to MOVE, so the
#               inbox empties as the queue advances)
#   --dry-run   report what would happen, touch nothing
#
# Safe to re-run: a PDF whose destination already exists is reported as
# a duplicate and left alone - never overwritten.

suppressPackageStartupMessages({
  library(openxlsx)
  library(pdftools)
})

args <- commandArgs(trailingOnly = TRUE)
dryRun <- "--dry-run" %in% args
doCopy <- "--copy" %in% args
days <- 0
i <- match("--days", args)
if (!is.na(i) && length(args) > i) {
  days <- suppressWarnings(as.numeric(args[i + 1]))
  if (is.na(days)) stop("--days needs a number")
  args <- args[-c(i, i + 1)]
}
srcDir <- setdiff(args, c("--dry-run", "--copy"))
srcDir <- if (length(srcDir)) srcDir[1] else
  "C:/dev/IntegrityAnalysis/.NewCarlisle/inbox"

root   <- "C:/dev/IntegrityAnalysis"
outDir <- file.path(root, ".NewCarlisle")
dir.create(outDir, showWarnings = FALSE)

## The master sheet - what we are matching against ----------------------
d <- read.xlsx(file.path(root, "Carlisle Data with PMIDs and DOIs.xlsx"),
               sheet = "All Data")
d$PMID <- as.character(d$PMID)
d$DOI  <- tolower(gsub("&amp;", "&", ifelse(is.na(d$DOI), "", d$DOI)))
d$Full.citation <- gsub("&amp;", "&", d$Full.citation)
byDoi  <- d$PMID[!duplicated(d$DOI)]
names(byDoi) <- d$DOI[!duplicated(d$DOI)]
allPmids <- unique(d$PMID)

# Citations reduced to comparable word bags, for the title fallback.
normWords <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9 ]+", " ", x)
  x <- gsub("\\s+", " ", trimws(x))
  x
}
stop_ <- c("a","an","and","the","of","in","for","on","to","with","by",
           "after","versus","vs","during","from","is","are","study",
           "randomized","randomised","controlled","trial","double",
           "blind","placebo","patients","effect","effects")
bagOf <- function(x) {
  w <- strsplit(normWords(x), " ")[[1]]
  unique(w[nchar(w) > 3 & !w %in% stop_])
}

# Match on the TITLE, not the whole citation. A citation is
# "Authors. Title. Journal. Year;vol(iss):pages. doi:..." - scoring a
# page against all of that can never clear a sensible threshold, because
# the author list and journal name are mostly absent from the article's
# own text. Splitting on ". " puts the title second.
titleOf <- function(cit) {
  parts <- strsplit(cit, "\\. ")[[1]]
  if (length(parts) >= 3) parts[2] else
    parts[which.max(nchar(parts))]
}
titleBags <- lapply(d$Full.citation, function(x) bagOf(titleOf(x)))

## Identification -------------------------------------------------------

DOI_RE <- "10\\.[0-9]{4,9}/[^ \t\n\"'<>,;()\\[\\]]+"

cleanDoi <- function(x) {
  x <- tolower(x)
  sub("[.,;:)\\]]+$", "", x)            # trailing punctuation from prose
}

fromDoi <- function(txt) {
  m <- unlist(regmatches(txt, gregexpr(DOI_RE, txt)))
  for (doi in cleanDoi(m)) {
    if (doi %in% names(byDoi)) return(list(pmid = byDoi[[doi]],
                                           how = paste("DOI in text:", doi)))
    # publisher PDFs sometimes print the DOI with a trailing suffix
    hit <- names(byDoi)[startsWith(doi, names(byDoi))]
    if (length(hit) == 1) return(list(pmid = byDoi[[hit]],
                                      how = paste("DOI in text:", hit)))
  }
  NULL
}

fromPmid <- function(txt) {
  m <- unlist(regmatches(txt, gregexpr("\\b[12][0-9]{6,7}\\b", txt)))
  hit <- intersect(m, allPmids)
  if (length(hit) == 1) return(list(pmid = hit,
                                    how = paste("PMID in text:", hit)))
  NULL
}

fromFileName <- function(f) {
  base <- tools::file_path_sans_ext(basename(f))
  # Elsevier "1-s2.0-<PII>-main", Wiley/NEJM/Springer "<doi suffix>"
  key <- tolower(gsub("^1-s2[.]0-|-main$", "", base))
  hit <- names(byDoi)[endsWith(names(byDoi), paste0("/", key))]
  if (length(hit) == 1) return(list(pmid = byDoi[[hit]],
                                    how = paste("file name ->", hit)))
  NULL
}

fromTitle <- function(txt) {
  # Score each candidate title by how much of it appears anywhere in the
  # article's first pages. Two-column PDFs scramble line order, so this
  # deliberately ignores layout and asks only "are these words here?".
  page <- unique(strsplit(normWords(txt), " ")[[1]])
  if (length(page) < 30) return(NULL)
  score <- vapply(titleBags, function(b)
    if (length(b) < 3) 0 else length(intersect(b, page)) / length(b),
    numeric(1))
  o <- order(-score)
  best <- score[o[1]]
  runnerUp <- if (length(o) > 1) score[o[2]] else 0

  # Corroboration: an article's own volume and page numbers are almost
  # always printed on page 1 ("British Journal of Anaesthesia 86 (6):
  # 879-81"). Word boundaries are written out because a page number is
  # often glued to punctuation ("879±81").
  inText <- function(v)
    nzchar(v) && !is.na(v) &&
      grepl(paste0("(^|[^0-9])", v, "([^0-9]|$)"), txt)
  corroborated <- function(i)
    inText(as.character(d$volume[i])) && inText(as.character(d$page[i]))

  # Titles in this corpus repeat: Fujii published a dozen near-identical
  # diaphragm papers, so the top scorers are often tied or a few points
  # apart, and the plain "best, well clear of the rest" rule refuses
  # them all. Volume and page break those ties - but only when exactly
  # ONE contender carries them, which is what makes it evidence rather
  # than a preference.
  near <- o[score[o] >= max(0.6, best - 0.25)]
  near <- near[seq_len(min(10, length(near)))]
  corr <- near[vapply(near, corroborated, logical(1))]

  if (length(corr) == 1) {
    i <- corr[1]
    return(list(pmid = d$PMID[i],
                how = sprintf("title %.0f%% + vol/page: %s", 100 * score[i],
                              substr(titleOf(d$Full.citation[i]), 1, 45))))
  }
  if (best >= 0.75 && best - runnerUp >= 0.15)
    return(list(pmid = d$PMID[o[1]],
                how = sprintf("title %.0f%% (next %.0f%%): %s",
                              100 * best, 100 * runnerUp,
                              substr(titleOf(d$Full.citation[o[1]]), 1, 45))))
  NULL
}

identify <- function(f) {
  txt <- tryCatch(paste(pdf_text(f)[seq_len(min(2, pdf_info(f)$pages))],
                        collapse = "\n"),
                  error = function(e) "")
  for (fn in list(function() fromDoi(txt), function() fromPmid(txt),
                  function() fromFileName(f), function() fromTitle(txt))) {
    r <- fn()
    if (!is.null(r)) return(r)
  }
  list(pmid = NA_character_,
       how = if (nchar(txt) < 200) "no text layer (scan?)" else "no match")
}

## Run ------------------------------------------------------------------

pdfs <- list.files(srcDir, pattern = "[.]pdf$", full.names = TRUE,
                   ignore.case = TRUE)
nAll <- length(pdfs)
if (days > 0) {
  age <- as.numeric(difftime(Sys.time(), file.mtime(pdfs), units = "days"))
  pdfs <- pdfs[!is.na(age) & age <= days]
}
cat("Source:", srcDir, "-", length(pdfs), "PDF(s)",
    if (days > 0) sprintf("modified in the last %g day(s), of %d present",
                          days, nAll) else "", "\n")
cat("Destination:", outDir, if (dryRun) " (DRY RUN)" else "", "\n\n")

filed <- 0; dup <- 0; unmatched <- character()
for (f in pdfs) {
  id <- identify(f)
  if (is.na(id$pmid)) {
    cat(sprintf("  ?  %-55s %s\n", substr(basename(f), 1, 55), id$how))
    unmatched <- c(unmatched, basename(f))
    next
  }
  dest <- file.path(outDir, paste0("PMID_", id$pmid, ".pdf"))
  if (file.exists(dest)) {
    cat(sprintf("  =  %-55s already have PMID_%s.pdf\n",
                substr(basename(f), 1, 55), id$pmid))
    dup <- dup + 1
    next
  }
  cat(sprintf("  -> PMID_%s.pdf  <- %-40s (%s)\n", id$pmid,
              substr(basename(f), 1, 40), id$how))
  if (!dryRun) {
    ok <- file.copy(f, dest)
    if (ok && !doCopy) unlink(f)
    if (!ok) { cat("     FAILED to write\n"); next }
  }
  filed <- filed + 1
}

cat(sprintf("\nFiled: %d   already had: %d   unidentified: %d\n",
            filed, dup, length(unmatched)))
if (length(unmatched))
  cat("Left in place for a human:\n  ",
      paste(unmatched, collapse = "\n   "), "\n")
cat("PDFs in", outDir, ":", length(list.files(outDir, "[.]pdf$")), "\n")
cat("\nRerun corpus/buildDownloadList.R to refresh the queue.\n")
