# buildFraudDownloadList.R - the manual download worklist for a
# fraud corpus: .Boldt/DownloadList.xlsx or .Fujii/DownloadList.xlsx.
#
# PROVENANCE: written by Claude Code (model Claude Opus 5), 2026-08-19,
# at Steve Shafer's request, as the Boldt/Fujii counterpart to
# corpus/buildDownloadList.R.
#
# WHY A SEPARATE SCRIPT rather than a switch inside the Carlisle one:
# the two lists are ordered by different things and cannot share a
# ranking. The Carlisle queue sorts by the trial's p-value, because the
# question there is "which baseline tables look too good?". These
# corpora have no p-value - every paper is already suspect, and Steve
# wants them for CORPUS SIZE, to refine the parser against real
# retracted papers. So the ordering here is by how easily a paper can
# be got: free copies first, then the subscription ones grouped by
# journal so one Lane session sweeps a run of them, and the rows with no
# PMID last, where a human has to look.
#
# Both corpora are 100% manual: the PMC open-access pass found nothing
# (0 of 185 Fujii, 0 of 98 Boldt - none of these papers are in PMC), and
# the license census found one CC-licensed Boldt paper whose only copy
# is a landing page. There is no automated route left to try.
#
# Usage:
#   "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" corpus/buildFraudDownloadList.R <boldt|fujii>
# Reads only. Re-run it as papers arrive - it counts the PDFs on disk.

suppressPackageStartupMessages({
  library(openxlsx)
})

args <- commandArgs(trailingOnly = TRUE)
which_ <- tolower(args[1])
if (!length(args) || !which_ %in% c("boldt", "fujii"))
  stop("say which corpus: boldt or fujii")

root <- "C:/dev/IntegrityAnalysis"
dir_ <- file.path(root, if (which_ == "boldt") ".Boldt" else ".Fujii")
outPath <- file.path(dir_, "DownloadList.xlsx")
newCarlisle <- file.path(root, ".NewCarlisle")
corpusDir <- "C:/temp/journals"

d <- read.csv(file.path(dir_, "pmids.csv"), colClasses = "character")
cat(which_, "rows:", nrow(d), "\n")
d$PMID[d$PMID == "NA"] <- NA

upPath <- file.path(dir_, "unpaywall.csv")
up <- if (file.exists(upPath)) read.csv(upPath, colClasses = "character") else
  data.frame(PMID = character(), DOI = character(), oa_status = character(),
             license = character(), url = character(),
             stringsAsFactors = FALSE)
up <- up[!duplicated(up$PMID), ]
iu <- match(d$PMID, up$PMID)
d$DOI       <- ifelse(is.na(iu), "", up$DOI[iu])
d$OA.status <- ifelse(is.na(iu), "", up$oa_status[iu])
d$License   <- ifelse(is.na(iu), "", up$license[iu])
d$Free.URL  <- ifelse(is.na(iu), "", up$url[iu])

## ------------------------------------------------------ what we have

# Three places a copy can already be: this corpus's own directory, the
# Carlisle download directory (Fujii's diaphragm papers arrived there,
# because they are in the Carlisle queue too), and the old local corpus.
pm <- read.csv(file.path(root, "corpus", "pmid_map.csv"),
               colClasses = "character")
here <- !is.na(d$PMID) &
  file.exists(file.path(dir_, paste0("PMID_", d$PMID, ".pdf")))
inNew <- !is.na(d$PMID) & !here &
  file.exists(file.path(newCarlisle, paste0("PMID_", d$PMID, ".pdf")))
ip <- match(d$PMID, pm$PMID)
inOld <- !is.na(ip) & !here & !inNew &
  file.exists(file.path(corpusDir, pm$PDF[ip]))

d$Have <- ifelse(here, dir_,
          ifelse(inNew, ".NewCarlisle",
          ifelse(inOld, file.path(corpusDir, pm$PDF[ip]), "")))

## --------------------------------------------------------- the status

freeToRead <- nzchar(d$OA.status) & d$OA.status != "closed"
d$Status <- ifelse(
  nzchar(d$Have), "have PDF",
  ifelse(is.na(d$PMID), "unresolved: no PubMed record",
  ifelse(freeToRead, "manual: free copy online",
         "manual: subscription (Lane proxy)")))

cat("\nStatus:\n"); print(table(d$Status))

## ----------------------------------------------------------- ordering

# Easiest first, then grouped by journal and year so a single library
# session can sweep a run of them.
rank <- match(d$Status, c("manual: free copy online",
                          "manual: subscription (Lane proxy)",
                          "unresolved: no PubMed record", "have PDF"))
o <- order(rank, d$journal, suppressWarnings(as.numeric(d$year)))
d <- d[o, ]

lane <- ifelse(is.na(d$PMID), "",
               paste0("https://pubmed-ncbi-nlm-nih-gov.laneproxy.stanford.edu/",
                      d$PMID, "/?otool=stanford&holding=F1000,F1000M"))

out <- data.frame(
  Priority = seq_len(nrow(d)),
  Status   = d$Status,
  Journal  = d$journal,
  Year     = d$year,
  Volume   = d$volume,
  Page     = d$page,
  PMID     = ifelse(is.na(d$PMID), "", d$PMID),
  DOI      = d$DOI,
  PubMed   = "", DOI.link = "", Stanford = "", Free = "",
  SaveAs   = ifelse(is.na(d$PMID), "", paste0("PMID_", d$PMID, ".pdf")),
  Have     = d$Have,
  OA.status = d$OA.status,
  Resolved.by = d$HOW,
  Citation = d$citation,
  Lane.URL = lane,
  Free.URL = d$Free.URL,
  stringsAsFactors = FALSE)

# Each source spreadsheet carries its own judgement columns; keep them,
# so Steve can re-sort by suspicion or by whether the paper was affirmed.
for (col in intersect(c("Suspicion", "InCarlisleAppendix", "Affirmed"),
                      names(d)))
  out[[col]] <- d[[col]]

queue <- out[grepl("^manual|^unresolved", out$Status), ]
queue$Priority <- seq_len(nrow(queue))
cat("\nQueue:", nrow(queue), " Full list:", nrow(out), "\n")

## -------------------------------------------------------------- write

wb <- createWorkbook()
hdr <- createStyle(textDecoration = "bold", halign = "center",
                   fgFill = "#FCE4D6", border = "bottom")
linkStyle <- createStyle(fontColour = "#0563C1", textDecoration = "underline")

addListSheet <- function(name, df) {
  addWorksheet(wb, name)
  writeData(wb, name, df, headerStyle = hdr)
  n <- nrow(df)
  if (n > 0) {
    rows <- 2:(n + 1)
    q <- function(x) gsub("\"", "'", x, fixed = TRUE)
    hyper <- function(col, text, url) {
      s <- ifelse(nzchar(url),
                  sprintf("HYPERLINK(\"%s\",\"%s\")", q(url), q(text)),
                  sprintf("\"%s\"", q(text)))
      writeFormula(wb, name, x = s, startCol = col, startRow = 2)
      addStyle(wb, name, linkStyle, rows = rows, cols = col,
               gridExpand = TRUE, stack = TRUE)
    }
    hyper(which(names(df) == "PubMed"), rep("PubMed", n),
          ifelse(nzchar(df$PMID),
                 paste0("https://pubmed.ncbi.nlm.nih.gov/", df$PMID, "/"), ""))
    hyper(which(names(df) == "DOI.link"), rep("publisher", n),
          ifelse(nzchar(df$DOI), paste0("https://doi.org/", df$DOI), ""))
    hyper(which(names(df) == "Stanford"), rep("Stanford", n), df$Lane.URL)
    hyper(which(names(df) == "Free"), rep("free copy", n), df$Free.URL)
  }
  freezePane(wb, name, firstActiveRow = 2)
  widths <- rep(14, ncol(df))
  widths[match(c("Status", "Journal", "Citation", "Lane.URL", "Free.URL",
                 "Have", "DOI", "Resolved.by"), names(df), nomatch = 0)] <-
    c(32, 34, 80, 60, 60, 30, 30, 22)[seq_len(sum(c("Status", "Journal",
      "Citation", "Lane.URL", "Free.URL", "Have", "DOI",
      "Resolved.by") %in% names(df)))]
  setColWidths(wb, name, cols = seq_along(df), widths = widths)
}

addListSheet("Queue", queue)
addListSheet("Full list", out)

notes <- data.frame(Notes = c(
  paste0("Manual download list for the ",
         if (which_ == "boldt") "Boldt" else "Fujii", " corpus."),
  "Built by corpus/buildFraudDownloadList.R from pmids.csv (PMIDs",
  "recovered from the citations by corpus/resolveCitationList.R) and",
  "unpaywall.csv (the license census).",
  "",
  "There is NO automated route for this corpus: none of these papers",
  "are in PMC's open access subset, and the license census found no",
  "openly licensed copy that a script may retrieve. Every row is",
  "manual work.",
  "",
  "Ordering is by how easily a paper can be got - free copies first,",
  "then subscription rows grouped by journal so one library session",
  "sweeps a run of them, then the rows with no PubMed record.",
  "",
  "Status = have PDF (already in hand - see the Have column) /",
  "         manual: free copy online (click Free) /",
  "         manual: subscription (Lane proxy) (click Stanford) /",
  "         unresolved: no PubMed record.",
  "",
  "SaveAs is informational: save the PDFs into",
  "  C:/dev/IntegrityAnalysis/.NewCarlisle/inbox",
  "under any name and run corpus/fileDownloads.R - it identifies each",
  "paper from its own contents and files it.",
  "",
  "The source spreadsheet's own columns (Suspicion, or the Carlisle",
  "appendix and affirmation flags) are kept on the right, so the list",
  "can be re-sorted by those in Excel."),
  stringsAsFactors = FALSE)
addWorksheet(wb, "Read me")
writeData(wb, "Read me", notes, headerStyle = hdr)
setColWidths(wb, "Read me", cols = 1, widths = 76)

saveWorkbook(wb, outPath, overwrite = TRUE)
cat("Wrote", outPath, "\n")
