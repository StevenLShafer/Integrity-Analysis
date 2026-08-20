# mapCorpusPmids.R - identify the local corpus PDFs that carry no PMID
# yet, and extend corpus/pmid_map.csv with what it can prove.
#
# PROVENANCE: written by Claude Code (model Claude Opus 5), 2026-08-19,
# at Steve Shafer's request. Of the 1,865 PDFs at C:/temp/journals, only
# 1,486 are mapped to a PMID; the other 379 are anonymous as far as
# every script here is concerned. That gap matters beyond tidiness:
# "do we already have this paper?" is answered through pmid_map.csv, so
# an unmapped PDF is invisible - we would re-download a paper that is
# already sitting on the disk. With the Boldt and Fujii lists now
# resolved to PMIDs, and those men having published in exactly these
# journals, the gap is worth closing before anyone downloads anything.
#
# HOW A PDF IS IDENTIFIED - the same evidence order as
# corpus/fileDownloads.R, matched against a reference table that is the
# UNION of the Carlisle master sheet and the two fraud lists:
#   1. a DOI printed in the text;
#   2. a PMID printed in the text;
#   3. the title, matched on word overlap and CONFIRMED by the volume
#      and first page printed on page 1. Nothing is accepted on a title
#      alone: a wrong mapping here would quietly mis-attribute a paper.
#
# Usage:
#   "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" corpus/mapCorpusPmids.R [--limit N] [--apply]
#
#   --apply   append the confirmed mappings to corpus/pmid_map.csv.
#             Without it the script only writes its findings to
#             .NewCarlisle/corpusPmidCandidates.csv for inspection -
#             pmid_map.csv is committed data, so changing it is opt-in.
#   --limit N stop after N PDFs (for a quick look).
#
# Progress is written as it goes, so a poppler hang - about 2% of real
# PDFs - costs only the file it happened on.

suppressPackageStartupMessages({
  library(openxlsx)
  library(pdftools)
})

args <- commandArgs(trailingOnly = TRUE)
apply_ <- "--apply" %in% args
limit <- NA_integer_
i <- match("--limit", args)
if (!is.na(i) && length(args) > i) limit <- as.integer(args[i + 1])

root <- "C:/dev/IntegrityAnalysis"
corpusDir <- "C:/temp/journals"
mapPath <- file.path(root, "corpus", "pmid_map.csv")
outPath <- file.path(root, ".NewCarlisle", "corpusPmidCandidates.csv")

## --------------------------------------------------- reference table

ref <- list()

ms <- read.xlsx(file.path(root, "Carlisle Data with PMIDs and DOIs.xlsx"),
                sheet = "All Data")
ref[[1]] <- data.frame(
  PMID = as.character(ms$PMID),
  journal = gsub("&amp;", "&", ms$Journal),
  DOI = tolower(gsub("&amp;", "&", ifelse(is.na(ms$DOI), "", ms$DOI))),
  title = vapply(ms$Full.citation, function(c) {
    p <- strsplit(c, "[.] ")[[1]]
    if (length(p) >= 3) p[2] else p[which.max(nchar(p))]
  }, character(1)),
  volume = as.character(ms$volume), page = as.character(ms$page),
  source = "Carlisle", stringsAsFactors = FALSE)

for (d in c(".Boldt", ".Fujii")) {
  f <- file.path(root, d, "pmids.csv")
  if (!file.exists(f)) next
  x <- read.csv(f, colClasses = "character")
  x <- x[!is.na(x$PMID) & nzchar(x$PMID) & x$PMID != "NA", ]
  if (!nrow(x)) next
  ref[[length(ref) + 1]] <- data.frame(
    PMID = x$PMID,
    journal = x$journal,
    DOI = if ("DOI" %in% names(x)) tolower(x$DOI) else "",
    title = x$title, volume = x$volume, page = x$page,
    source = sub("^[.]", "", d), stringsAsFactors = FALSE)
}
ref <- do.call(rbind, ref)
ref <- ref[!duplicated(paste(ref$PMID, ref$source)), ]
cat("reference records:", nrow(ref),
    " (", paste(names(table(ref$source)), table(ref$source),
                collapse = ", "), ")\n")

# The corpus tree is <journal>/<year>/<n.m>.pdf, so a file's directory
# says which journal it is - a constraint worth spending, because
# without it a title match can land on a paper from another journal
# entirely (it did: a Boldt paper from Intensive Care Medicine was
# "found" inside an Anesthesiology file).
journalCode <- function(x) {
  x <- tolower(ifelse(is.na(x), "", x))
  ifelse(grepl("american medical|jama", x), "JAMA",
  ifelse(grepl("british journal of ana|br j ana", x), "bja",
  ifelse(grepl("canadian|can j", x), "CJA",
  ifelse(grepl("european journal of ana|eur j ana", x), "eja",
  ifelse(grepl("anesthesiology", x), "Anesthesiology",
  ifelse(grepl("new england", x), "NEJM",
  ifelse(grepl("analgesia|anesth analg", x), "A&A",
  ifelse(grepl("^anaesthesia|[^f] anaesthesia$|^the anaesthesia", x),
         "Anaesthesia", "other"))))))))
}

normWords <- function(x) {
  x <- tolower(x); x <- gsub("[^a-z0-9 ]+", " ", x)
  gsub("[ ]+", " ", trimws(x))
}
bagOf <- function(x) {
  w <- strsplit(normWords(x), " ")[[1]]
  unique(w[nchar(w) > 3])
}
refBags <- lapply(ref$title, bagOf)
refCode <- journalCode(ref$journal)
# A "title" that is really a journal locator, or that reduces to a
# handful of generic words, cannot support an identification. Six
# distinctive words is the floor; below it, a 100% match means nothing.
refUsable <- vapply(refBags, length, integer(1)) >= 6 &
  !grepl(LOC_RE_REF, ref$title)

byDoi <- ref$PMID[nzchar(ref$DOI)]
names(byDoi) <- ref$DOI[nzchar(ref$DOI)]

## ------------------------------------------------------------ the work

po <- read.csv(file.path(root, "corpus", "ParseOutcomes.csv"),
               colClasses = "character")
pm <- read.csv(mapPath, colClasses = "character")
todo <- po$PDF[!po$PDF %in% pm$PDF]
todo <- todo[file.exists(file.path(corpusDir, todo))]
if (!is.na(limit)) todo <- head(todo, limit)
cat("unmapped PDFs to identify:", length(todo), "\n")

DOI_RE <- "10[.][0-9]{4,9}/[^ \t\n\"'<>,;()]+"
inText <- function(v, txt)
  nzchar(v) && !is.na(v) &&
    grepl(paste0("(^|[^0-9])", v, "([^0-9]|$)"), txt)

identify <- function(path, rel) {
  txt <- tryCatch({
    info <- pdf_info(path)
    paste(pdf_text(path)[seq_len(min(2, info$pages))], collapse = "\n")
  }, error = function(e) "")
  if (nchar(txt) < 200) return(list(pmid = "", how = "no text layer"))
  dirCode <- sub("/.*$", "", rel)     # the journal this file sits under

  m <- unique(tolower(unlist(regmatches(txt, gregexpr(DOI_RE, txt)))))
  m <- sub("[.,;:)\\]]+$", "", m)
  for (doi in m) if (doi %in% names(byDoi))
    return(list(pmid = byDoi[[doi]], how = paste("DOI:", doi)))

  ids <- unlist(regmatches(txt, gregexpr("\\b[12][0-9]{6,7}\\b", txt)))
  hit <- intersect(ids, ref$PMID)
  if (length(unique(hit)) == 1)
    return(list(pmid = unique(hit), how = "PMID printed in text"))

  page <- unique(strsplit(normWords(txt), " ")[[1]])
  if (length(page) < 30) return(list(pmid = "", how = "too little text"))

  # Only reference records that CAN be matched: a usable title, and the
  # journal this file actually sits under. Volume and first page are
  # two- and three-digit numbers, so on their own they corroborate
  # almost anything in a paper full of numbers - the journal is what
  # makes the corroboration mean something.
  ok <- which(refUsable & refCode == dirCode)
  if (!length(ok)) return(list(pmid = "", how = "no candidate in journal"))
  score <- vapply(refBags[ok], function(b)
    length(intersect(b, page)) / length(b), numeric(1))
  o <- ok[order(-score)]
  sorted <- sort(score, decreasing = TRUE)
  near <- o[sorted >= max(0.6, sorted[1] - 0.25)]
  near <- near[seq_len(min(10, length(near)))]
  corr <- near[vapply(near, function(k)
    inText(ref$volume[k], txt) && inText(ref$page[k], txt), logical(1))]
  if (length(corr) == 1) {
    sc <- length(intersect(refBags[[corr]], page)) / length(refBags[[corr]])
    return(list(pmid = ref$PMID[corr],
                how = sprintf("title %.0f%% + vol/page [%s]",
                              100 * sc, ref$source[corr])))
  }
  list(pmid = "", how = if (sorted[1] >= 0.6)
    "title matched but not confirmed" else "no match")
}

res <- data.frame(PDF = character(), PMID = character(), HOW = character(),
                  stringsAsFactors = FALSE)
for (k in seq_along(todo)) {
  id <- identify(file.path(corpusDir, todo[k]), todo[k])
  res <- rbind(res, data.frame(PDF = todo[k], PMID = id$pmid, HOW = id$how,
                               stringsAsFactors = FALSE))
  if (k %% 25 == 0 || k == length(todo)) {
    write.csv(res, outPath, row.names = FALSE)
    cat(sprintf("  %d/%d  identified: %d\n", k, length(todo),
                sum(nzchar(res$PMID))))
  }
}

cat("\n== how they resolved ==\n")
print(table(sub(":.*$", "", res$HOW)))
found <- res[nzchar(res$PMID), ]
# Two files cannot be the same paper. When that happens at least one is
# wrong and there is no way to tell which, so drop both and say so.
contested <- unique(found$PMID[duplicated(found$PMID)])
if (length(contested)) {
  cat("DROPPED", sum(found$PMID %in% contested), "identification(s):",
      length(contested), "PMID(s) claimed by more than one file\n")
  found <- found[!found$PMID %in% contested, ]
}
cat("identified:", nrow(found), "of", nrow(res), "\n")

# Which corpus do the newly identified papers belong to? This is the
# question that prompted the exercise.
src <- ref$source[match(found$PMID, ref$PMID)]
if (length(src)) { cat("\nby corpus:\n"); print(table(src)) }

if (apply_ && nrow(found)) {
  add <- data.frame(PDF = found$PDF, PMID = found$PMID,
                    stringsAsFactors = FALSE)
  add <- add[!add$PDF %in% pm$PDF, ]
  # A PMID already claimed by another file is a contradiction, not a
  # new fact - leave those out and say so.
  dup <- add$PMID %in% pm$PMID
  if (any(dup)) cat("skipped", sum(dup),
                    "whose PMID is already mapped to another file\n")
  add <- add[!dup, ]
  write.csv(rbind(pm[, c("PDF", "PMID")], add), mapPath, row.names = FALSE)
  cat("appended", nrow(add), "mappings to", mapPath, "\n")
} else {
  cat("\n(dry run - pass --apply to extend corpus/pmid_map.csv)\n")
}
cat("candidates written to", outPath, "\n")
