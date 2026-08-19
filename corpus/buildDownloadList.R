# buildDownloadList.R - build the PRIORITIZED MANUAL DOWNLOAD LIST for
# the Carlisle corpus: .NewCarlisle/DownloadPriorityList.xlsx.
#
# PROVENANCE: written by Claude Code (model Claude Opus 5), 2026-08-19,
# to Steve Shafer's design, after the automated routes were exhausted.
# The automated passes (corpus/downloadNewCarlisle.R for the PMC open
# access subset, corpus/unpaywallDiscovery.R for the license census)
# together reach only a few hundred of the 5,088 trials legally. The
# rest have to be fetched BY HAND, a handful per day, through Stanford's
# Lane Library proxy - individual downloads are within Lane's terms;
# bulk retrieval is not, and no script here does it.
#
# So the deliverable is a worklist, not a downloader: one row per trial,
# SORTED ASCENDING BY THE CARLISLE TRIAL P-VALUE, because the lowest
# p-values are the trials whose baseline tables are most worth having.
# Steve works down the list; every row carries the links he needs (DOI,
# PubMed, the Lane proxy, and any free open-access copy) and the exact
# file name to save the PDF under.
#
# Usage:
#   "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" corpus/buildDownloadList.R
# Reads only; safe to re-run at any time (and worth re-running as the
# unpaywall census and the manual downloads progress).

suppressPackageStartupMessages({
  library(openxlsx)
})

root    <- "C:/dev/IntegrityAnalysis"
outDir  <- file.path(root, ".NewCarlisle")
outPath <- file.path(outDir, "DownloadPriorityList.xlsx")
# Where the already-owned corpus PDFs live (see corpus/README.md).
corpusDir <- "C:/temp/journals"
dir.create(outDir, showWarnings = FALSE)

# Excel stores these strings HTML-escaped ("Anesthesia &amp; Analgesia",
# "?otool=stanford&amp;holding=..."); an escaped ampersand in a URL
# breaks the link, so undo it everywhere.
unescape <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x
}

# ---------------------------------------------------------------- inputs

# The trial-level master sheet. Use the "with PMIDs and DOIs" copy: the
# pristine "Carlisle Data.xlsx" still has the 542 Anesthesiology rows
# shifted one column right of their headers (p.value would read as a
# trial number). See the repair note on A1689 of the repaired copy.
sheetPath <- file.path(root, "Carlisle Data with PMIDs and DOIs.xlsx")
d <- read.xlsx(sheetPath, sheet = "All Data")
cat("Trials in the master sheet:", nrow(d), "\n")

d$PMID <- as.character(d$PMID)
d$DOI  <- unescape(d$DOI)
d$p    <- suppressWarnings(as.numeric(d$p.value))
stopifnot(!any(is.na(d$p)), all(d$p >= 0 & d$p <= 1))

# License census (metadata only - where a legal free copy lives).
upPath <- file.path(outDir, "unpaywall.csv")
up <- if (file.exists(upPath)) {
  read.csv(upPath, colClasses = "character")
} else {
  data.frame(PMID = character(), DOI = character(), is_oa = character(),
             oa_status = character(), license = character(),
             host_type = character(), url = character(),
             stringsAsFactors = FALSE)
}
up <- up[!duplicated(up$DOI), ]           # a crash mid-write can interleave rows
cat("Unpaywall records:", nrow(up), "\n")

# What the PMC open-access pass actually fetched into .NewCarlisle.
mfPath <- file.path(outDir, "manifest.csv")
mf <- if (file.exists(mfPath)) {
  read.csv(mfPath, colClasses = "character")
} else {
  data.frame(PMID = character(), status = character(), file = character(),
             stringsAsFactors = FALSE)
}
mf <- mf[!duplicated(mf$PMID), ]
cat("PMC manifest rows:", nrow(mf),
    " downloaded:", sum(grepl("^downloaded", mf$status)), "\n")

# PDFs already in the local corpus, and how the parser did on them.
pm <- read.csv(file.path(root, "corpus", "pmid_map.csv"),
               colClasses = "character")
pm <- pm[nzchar(pm$PMID) & !duplicated(pm$PMID), ]
po <- read.csv(file.path(root, "corpus", "ParseOutcomes.csv"),
               colClasses = "character")
pm$OUTCOME <- po$OUTCOME[match(pm$PDF, po$PDF)]
cat("Local corpus PDFs mapped to a PMID:", nrow(pm), "\n")

# ----------------------------------------------------------------- joins

# Unpaywall was queried by DOI; 132 trials have no DOI, so fall back to
# its PMID column for those.
iu <- match(d$DOI, up$DOI)
iu[!nzchar(d$DOI)] <- NA
fallback <- is.na(iu)
iu[fallback] <- match(d$PMID[fallback], up$PMID)

d$OA.status <- ifelse(is.na(iu), "", up$oa_status[iu])
d$License   <- ifelse(is.na(iu), "", up$license[iu])
d$Free.URL  <- ifelse(is.na(iu), "", up$url[iu])
d$queried   <- !is.na(iu)

# "Licensed" means a license that grants permission to copy - CC or
# public domain. Unpaywall's "other-oa" is NOT one: it means "open
# access, license unstated", which is free to read, not licensed to
# retrieve. Keep this list identical to corpus/downloadLicensedOA.R,
# which is what actually fetches these.
LICENSED <- c("cc0", "public-domain", "cc-by", "cc-by-sa", "cc-by-nc",
              "cc-by-nc-sa", "cc-by-nd", "cc-by-nc-nd")
d$Licensed <- sub("-[0-9].*$", "", tolower(d$License)) %in% LICENSED

im <- match(d$PMID, mf$PMID)
d$PMC.status <- ifelse(is.na(im), "", mf$status[im])
d$PMC.file   <- ifelse(is.na(im), "", mf$file[im])

# What is actually on disk in .NewCarlisle is the truth about what we
# have, and it outranks any manifest: the automated passes write their
# own manifests, but Steve's hand-downloaded papers (filed by
# corpus/fileDownloads.R) appear only as files. Without this the queue
# would never shrink as he works it.
d$Have.new <- file.exists(file.path(outDir, paste0("PMID_", d$PMID, ".pdf")))

# What the licensed-OA pass tried and could not get. A CC license is
# permission, not access: Wiley, JAMA and the ASA front their licensed
# PDFs with bot protection a script does not get through, and some
# licensed copies exist only as landing pages. Those rows must NOT stay
# classified "auto-eligible" - no script will ever fetch them, so they
# would sit outside the queue forever. They go back to Steve, who can
# simply click them: the copy is free and openly licensed, it just
# wants a browser.
loPath <- file.path(outDir, "licensed_manifest.csv")
lo <- if (file.exists(loPath)) read.csv(loPath, colClasses = "character") else
  data.frame(PMID = character(), status = character(), url = character(),
             stringsAsFactors = FALSE)
il <- match(d$PMID, lo$PMID)
d$Licensed.tried  <- !is.na(il)
d$Licensed.failed <- !is.na(il) &
  lo$status[il] %in% c("download_failed", "no_licensed_pdf_url")
# Prefer the exact PDF link the pass found over the census's best guess.
hasUrl <- !is.na(il) & nzchar(ifelse(is.na(il), "", lo$url[il]))
d$Free.URL[hasUrl] <- lo$url[il][hasUrl]

ip <- match(d$PMID, pm$PMID)
d$Local.PDF     <- ifelse(is.na(ip), "", pm$PDF[ip])
d$Parse.outcome <- ifelse(is.na(ip), "", ifelse(is.na(pm$OUTCOME[ip]), "",
                                                pm$OUTCOME[ip]))
# The mapping is the record of what we own; confirm it is still on disk.
d$Local.exists <- nzchar(d$Local.PDF) &
  file.exists(file.path(corpusDir, d$Local.PDF))

# ---------------------------------------------------------------- status
#
# Precedence, most-resolved first. Only the last two categories are work
# for Steve; "auto-eligible" rows carry a CC license, so a future
# scripted pass may legitimately fetch them - they do not belong in a
# manual queue.
d$Status <- ifelse(
  d$Have.new, "have PDF (.NewCarlisle)",
  ifelse(nzchar(d$Local.PDF), "have PDF (local corpus)",
  ifelse(grepl("^downloaded", d$PMC.status), "have PDF (PMC open access)",
  ifelse(d$Licensed.failed, "manual: openly licensed, needs a browser",
  ifelse(d$Licensed, "auto-eligible: licensed open access",
  ifelse(nzchar(d$OA.status) & d$OA.status != "closed",
         "manual: free copy online",
         "manual: subscription (Lane proxy)"))))))
# Trials the census could not speak to - no DOI in the master sheet, or a
# DOI not yet queried - are unclassified rather than "subscription"; say
# so instead of guessing. With the census complete these are exactly the
# 132 trials that carry no DOI, so the label names that reason.
unresolved <- !d$queried & !d$Have.new & !nzchar(d$Local.PDF) &
  !grepl("^downloaded", d$PMC.status)
d$Status[unresolved] <- ifelse(nzchar(d$DOI[unresolved]),
                               "manual: license unknown (not yet queried)",
                               "manual: no DOI, license never looked up")

cat("\nStatus of the 5,088 trials:\n")
print(table(d$Status))

# ------------------------------------------------------------ the sheets

# Lowest Carlisle p first - the whole point of the ordering.
d <- d[order(d$p), ]

out <- data.frame(
  Priority   = seq_len(nrow(d)),
  p          = d$p,
  Journal    = unescape(d$Journal),
  Year       = d$year,
  Volume     = d$volume,
  Page       = d$page,
  Trial      = d$trial,
  # PMID and DOI stay PLAIN TEXT so the sheet can be read back by a
  # script; the clickable versions live in their own columns, because a
  # HYPERLINK() formula has no value until Excel recalculates it and
  # read.xlsx() would see NA.
  PMID       = d$PMID,
  DOI        = d$DOI,
  PubMed     = "",          # -> pubmed.ncbi.nlm.nih.gov/<pmid>/
  DOI.link   = "",          # -> doi.org/<doi>  (the publisher's page)
  Stanford   = "",          # -> the Lane proxy link Steve should use
  Free       = "",          # -> an open-access copy, where one exists
  Status     = d$Status,
  OA.status  = d$OA.status,
  License    = d$License,
  Parse      = d$Parse.outcome,
  Local.PDF  = d$Local.PDF,
  SaveAs     = paste0("PMID_", d$PMID, ".pdf"),
  Citation   = unescape(d$Full.citation),
  Lane.URL   = unescape(d$Download.link),
  Free.URL   = d$Free.URL,
  stringsAsFactors = FALSE
)

queue <- out[grepl("^manual", out$Status), ]
queue$Priority <- seq_len(nrow(queue))
cat("\nQueue (needs a manual download):", nrow(queue), "trials\n")
cat("Full list:", nrow(out), "trials\n")

# Four trials share a PMID with another trial (two trials reported in one
# paper), so they share a SaveAs name - one download serves both rows.
dupes <- sum(duplicated(queue$SaveAs))
if (dupes > 0) cat("Note:", dupes,
                   "queue row(s) share a PDF with an earlier row\n")

# ------------------------------------------------------------- write xlsx

wb <- createWorkbook()
hdr <- createStyle(textDecoration = "bold", halign = "center",
                   fgFill = "#DDEBF7", border = "bottom")
pStyle <- createStyle(numFmt = "0.00000")
linkStyle <- createStyle(fontColour = "#0563C1", textDecoration = "underline")

addListSheet <- function(name, df) {
  addWorksheet(wb, name)
  writeData(wb, name, df, headerStyle = hdr)
  # Show a readable label, link to the target: PMID -> PubMed, DOI ->
  # doi.org, and the two URL columns under short link text.
  n <- nrow(df)
  if (n > 0) {
    rows <- 2:(n + 1)
    # writeFormula writes a whole column at once, so rows with no URL
    # get a quoted literal - a formula that is just its own text. The
    # HYPERLINK string is built by hand because makeHyperlinkString()
    # takes one link at a time.
    q <- function(x) gsub("\"", "'", x, fixed = TRUE)
    hyper <- function(col, text, url) {
      s <- ifelse(nzchar(url),
                  sprintf("HYPERLINK(\"%s\",\"%s\")", q(url), q(text)),
                  sprintf("\"%s\"", q(text)))
      writeFormula(wb, name, x = s, startCol = col, startRow = 2)
      addStyle(wb, name, linkStyle, rows = rows,
               cols = col, gridExpand = TRUE, stack = TRUE)
    }
    hyper(which(names(df) == "PubMed"), rep("PubMed", n),
          ifelse(nzchar(df$PMID),
                 paste0("https://pubmed.ncbi.nlm.nih.gov/", df$PMID, "/"), ""))
    hyper(which(names(df) == "DOI.link"), rep("publisher", n),
          ifelse(nzchar(df$DOI), paste0("https://doi.org/", df$DOI), ""))
    hyper(which(names(df) == "Stanford"), rep("Stanford", n), df$Lane.URL)
    hyper(which(names(df) == "Free"), rep("free copy", n), df$Free.URL)
    addStyle(wb, name, pStyle, rows = rows,
             cols = which(names(df) == "p"), gridExpand = TRUE, stack = TRUE)
  }
  freezePane(wb, name, firstActiveRow = 2)
  setColWidths(wb, name, cols = seq_along(df),
               widths = c(8, 10, 30, 6, 8, 8, 6, 11, 34, 10, 10, 10,
                          10, 34, 10, 26, 22, 30, 22, 60, 60, 60))
}

addListSheet("Queue", queue)
addListSheet("Full list", out)

# A short read-me sheet: the file is meant to be opened by a human who
# was not in this conversation.
notes <- data.frame(Notes = c(
  "Prioritized manual download list for the Carlisle corpus.",
  paste("Built by corpus/buildDownloadList.R from",
        "'Carlisle Data with PMIDs and DOIs.xlsx' (sheet 'All Data'),",
        ".NewCarlisle/unpaywall.csv, .NewCarlisle/manifest.csv,",
        "corpus/pmid_map.csv and corpus/ParseOutcomes.csv."),
  "",
  "Queue      = trials with no legal automated route and no PDF yet,",
  "             sorted with the LOWEST Carlisle p-value first.",
  "Full list  = every trial, same ordering, including the ones already",
  "             in hand or reachable automatically.",
  "",
  "p          = Carlisle's raw one-sided trial p-value; small = baseline",
  "             tables more homogeneous than chance.",
  "Status     = have PDF (.NewCarlisle - downloaded or filed by hand) /",
  "             have PDF (local corpus) / have PDF (PMC open access) /",
  "             auto-eligible: licensed open access (leave to a script) /",
  "             manual: openly licensed, needs a browser (free - the",
  "             publisher blocks scripts, so just click the Free link) /",
  "             manual: free copy online / manual: subscription (Lane",
  "             proxy) / manual: no DOI, license never looked up.",
  "SaveAs     = file name to save into C:/dev/IntegrityAnalysis/",
  "             .NewCarlisle - the parser pipeline keys on PMID.",
  "",
  "Download a handful per day through the Stanford link. Individual",
  "downloads are within Lane Library's terms; bulk retrieval is not."),
  stringsAsFactors = FALSE)
addWorksheet(wb, "Read me")
writeData(wb, "Read me", notes, headerStyle = hdr)
setColWidths(wb, "Read me", cols = 1, widths = 78)

saveWorkbook(wb, outPath, overwrite = TRUE)
cat("\nWrote", outPath, "\n")
