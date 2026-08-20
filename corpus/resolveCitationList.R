# resolveCitationList.R - turn a list of CITATIONS into PMIDs, and say
# which of the papers we already hold.
#
# PROVENANCE: written by Claude Code (model Claude Opus 5), 2026-08-19,
# at Steve Shafer's request, to start the Boldt and Fujii corpora
# (corpus/Boldt.xlsx, corpus/Fujii.xlsx - the two serial fraudsters at
# the top of the Retraction Watch leaderboard). Those spreadsheets are
# human documents: they carry a citation string per paper and no
# identifier at all, so nothing downstream - not the PMC pass, not the
# Unpaywall census - can touch them until each row has a PMID.
#
# HOW A ROW IS RESOLVED, cheapest and most certain first:
#   1. the Carlisle master sheet, offline. Many of these papers are
#      already in it, with a PMID we trust, and journal + year + volume
#      + first page identify a paper exactly.
#   2. NCBI's ecitmatch, the utility built for this: it takes
#      journal|year|volume|first-page|author| and returns the PMID.
#   3. esearch on the title, for rows whose volume or page is missing or
#      mangled in the source spreadsheet.
# Rows that survive all three are reported unresolved rather than
# guessed at.
#
# It also reports, for every resolved row, whether the PDF is ALREADY
# IN HAND - in .NewCarlisle, or in the local corpus at C:/temp/journals
# via corpus/pmid_map.csv. Steve asked what is already downloaded before
# anything is fetched, and for Fujii in particular the answer is "quite
# a lot": his diaphragm papers came in with the Carlisle queue.
#
# Usage:
#   "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" corpus/resolveCitationList.R <boldt|fujii> [--offline]
#
#   --offline   skip the two NCBI steps (step 1 only) - useful for a
#               quick overlap count without hitting the network.
#
# Output: .Boldt/pmids.csv or .Fujii/pmids.csv, one row per source row:
#   PMID, HOW (which step resolved it), HAVE (where the PDF already is,
#   or ""), plus the parsed fields and the original citation.
# Resumable in the sense that matters: re-running re-resolves from
# scratch in a couple of minutes, and never re-downloads anything.

suppressPackageStartupMessages({
  library(openxlsx)
  library(jsonlite)
  library(xml2)
})

args <- commandArgs(trailingOnly = TRUE)
offline <- "--offline" %in% args
which_ <- tolower(setdiff(args, "--offline"))
if (!length(which_) || !which_[1] %in% c("boldt", "fujii"))
  stop("say which list: boldt or fujii")
which_ <- which_[1]

root <- "C:/dev/IntegrityAnalysis"
outDir <- file.path(root, if (which_ == "boldt") ".Boldt" else ".Fujii")
dir.create(outDir, showWarnings = FALSE)
newCarlisle <- file.path(root, ".NewCarlisle")
corpusDir <- "C:/temp/journals"

email <- "steven.shafer@stanford.edu"
tool  <- "IntegrityAnalysis"
pause <- function() Sys.sleep(1)

## ---------------------------------------------------------------- parse

# Both sheets end their citation with the journal's own locator -
# "2010;24:399-407", "1997;41:1167-70" - which is the part that
# identifies the paper. Volume is sometimes followed by an issue in
# parentheses, and the source has typos ("2009;26(:1020-5"), so the
# pattern skips non-digits between the volume and the colon.
LOC_RE <- "(19|20)[0-9]{2}[;][^0-9]*[0-9]+[^0-9:]*[:][ ]*[0-9]+"
parseLoc <- function(s) {
  m <- regmatches(s, regexpr(LOC_RE, s))
  out <- data.frame(year = NA_integer_, volume = NA_integer_,
                    page = NA_integer_)
  if (!length(m)) return(out)
  nums <- as.integer(unlist(regmatches(m, gregexpr("[0-9]+", m))))
  # year, then volume, then first page (an issue number, if present,
  # sits between volume and page and is dropped by taking first/last)
  out$year <- nums[1]
  out$volume <- nums[2]
  out$page <- nums[length(nums)]
  out
}

# First author's surname: the citation opens with it in both sheets
# ("Boldt J, Mayer J, ...", "Fujii Y, Tanaka H, ...", and occasionally
# a full-name form, "Joachim Boldt Stephan Suttner ...").
surnameOf <- function(s) {
  s <- trimws(s)
  first <- sub("[ ,.].*$", "", s)
  if (nchar(first) < 2) return("")
  first
}

# The title, for the esearch fallback: the sentence between the author
# list and the journal name. Authors end at ": " (Boldt's style) or at
# the first ". " that follows an initial.
titleOf <- function(s) {
  x <- sub("^.*?(: |\\. )", "", s)     # drop the author list
  x <- sub("\\.[^.]*$", "", x)          # drop the trailing journal+locator
  parts <- strsplit(x, "\\. ")[[1]]
  parts[which.max(nchar(parts))]
}

if (which_ == "boldt") {
  src <- read.xlsx(file.path(root, "corpus", "Boldt.xlsx"))
  cite <- src$Article
  journalFull <- src$Journal
  extra <- data.frame(Suspicion = src$Suspicion, stringsAsFactors = FALSE)
} else {
  src <- read.xlsx(file.path(root, "corpus", "Fujii.xlsx"),
                   sheet = "Fujii Papers Status")
  keep <- !is.na(src$Paper) & nzchar(src$Paper)
  src <- src[keep, ]
  cite <- src$Paper
  journalFull <- src$Journal
  extra <- data.frame(InCarlisleAppendix = src$X2,
                      Affirmed = src$`Affirmed?`, stringsAsFactors = FALSE)
}

loc <- do.call(rbind, lapply(cite, parseLoc))
# Fujii's sheet also carries the locator in its own column, which is
# cleaner than the prose; prefer it where the prose failed.
if (which_ == "fujii" && "Sort.Field" %in% names(src)) {
  alt <- do.call(rbind, lapply(paste0(src$Sort.Field), parseLoc))
  fix <- is.na(loc$volume) & !is.na(alt$volume)
  loc[fix, ] <- alt[fix, ]
}

work <- data.frame(
  citation = cite, journal = journalFull,
  author = vapply(cite, surnameOf, character(1)),
  title = vapply(cite, titleOf, character(1)),
  year = loc$year, volume = loc$volume, page = loc$page,
  PMID = NA_character_, HOW = "", stringsAsFactors = FALSE)
work <- cbind(work, extra)
cat(which_, "rows:", nrow(work),
    " with a parsed volume/page:", sum(!is.na(work$volume)), "\n")

## ------------------------------------------- step 1: the master sheet

ms <- read.xlsx(file.path(root, "Carlisle Data with PMIDs and DOIs.xlsx"),
                sheet = "All Data")
ms$PMID <- as.character(ms$PMID)
key <- function(y, v, p) paste(y, v, p, sep = "|")
msKey <- key(ms$year, ms$volume, ms$page)
hit <- match(key(work$year, work$volume, work$page), msKey)
# A year/volume/page collision across two different journals is
# possible, so require the journal to agree on its first word too.
firstWord <- function(x) tolower(sub("[^A-Za-z].*$", "",
                                     sub("^(The|A) ", "", x)))
agree <- !is.na(hit) &
  firstWord(work$journal) == firstWord(ms$Journal[hit])
work$PMID[agree] <- ms$PMID[hit[agree]]
work$HOW[agree] <- "Carlisle master sheet"
cat("resolved from the master sheet:", sum(agree), "\n")

## ----------------------------------------------- step 2: NCBI ecitmatch

ecitmatch <- function(rows) {
  # bdata lines: journal|year|volume|first_page|author|key|
  lines <- sprintf("%s|%s|%s|%s|%s|%d|",
                   rows$journal, rows$year, rows$volume, rows$page,
                   rows$author, rows$idx)
  u <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/ecitmatch.cgi",
              "?db=pubmed&retmode=xml&tool=", tool, "&email=",
              utils::URLencode(email, reserved = TRUE),
              "&bdata=", utils::URLencode(paste(lines, collapse = "\r"),
                                          reserved = TRUE))
  txt <- tryCatch(paste(readLines(u, warn = FALSE), collapse = "\n"),
                  error = function(e) "")
  # each response line echoes the query and appends the PMID (or
  # NOT_FOUND / AMBIGUOUS)
  out <- setNames(rep(NA_character_, nrow(rows)), rows$idx)
  for (ln in strsplit(txt, "\n")[[1]]) {
    f <- strsplit(ln, "\\|")[[1]]
    if (length(f) < 7) next
    k <- trimws(f[6]); v <- trimws(f[7])
    if (grepl("^[0-9]+$", v) && k %in% names(out)) out[[k]] <- v
  }
  out
}

## Step 3 - the author's output for that year, matched on title.
#
# A title query cannot be trusted on these lists. Boldt's spellings drift
# from PubMed's ("hydroxyethylstarch" vs "hydroxyethyl starch"), and an
# exact-phrase search fails outright once punctuation is stripped - tried
# both, both returned nothing for papers that are certainly indexed. So
# instead: ask PubMed for everything this author published that year (a
# handful of records), read their titles, and match locally. Volume and
# page, when the spreadsheet has them, settle it outright.

eget <- function(u) {
  j <- tryCatch(fromJSON(u), error = function(e) NULL)
  pause()
  j
}

esearchIds <- function(q, retmax = 60) {
  j <- eget(paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
    "?db=pubmed&retmode=json&retmax=", retmax, "&tool=", tool,
    "&email=", utils::URLencode(email, reserved = TRUE),
    "&term=", utils::URLencode(q, reserved = TRUE)))
  if (is.null(j)) character(0) else j$esearchresult$idlist
}

esummaries <- function(ids) {
  if (!length(ids)) return(NULL)
  j <- eget(paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi",
    "?db=pubmed&retmode=json&tool=", tool,
    "&email=", utils::URLencode(email, reserved = TRUE),
    "&id=", paste(ids, collapse = ",")))
  if (is.null(j) || is.null(j$result)) return(NULL)
  recs <- j$result[setdiff(names(j$result), "uids")]
  do.call(rbind, lapply(names(recs), function(id) {
    r <- recs[[id]]
    data.frame(PMID = id,
               title = if (is.null(r$title)) "" else r$title,
               volume = if (is.null(r$volume)) "" else r$volume,
               pages = if (is.null(r$pages)) "" else r$pages,
               stringsAsFactors = FALSE)
  }))
}

bagOf <- function(x) {
  w <- strsplit(tolower(gsub("[^A-Za-z0-9 ]", " ", x)), " +")[[1]]
  unique(w[nchar(w) > 3])
}

resolveByAuthorYear <- function(i) {
  # Every paper on these lists is by the man the list is about, so fall
  # back to him when the citation does not open with a parseable
  # surname ("Joachim Boldt Stephan Suttner ...", or a co-author first).
  author <- work$author[i]
  if (!nzchar(author)) author <- if (which_ == "boldt") "Boldt" else "Fujii"
  year <- work$year[i]
  if (is.na(year)) return(NULL)

  # Narrow by volume where we have it. "Fujii[Author] AND 2006[DP]"
  # alone matches hundreds of papers - Fujii is a common surname - so a
  # 60-record window would rarely contain the right one. Volume [VI]
  # cuts that to a handful.
  q <- paste0(author, "[Author] AND ", year, "[DP]")
  ids <- character(0)
  if (!is.na(work$volume[i]))
    ids <- esearchIds(paste0(q, " AND ", work$volume[i], "[VI]"))
  if (!length(ids)) ids <- esearchIds(q, retmax = 200)
  if (!length(ids)) return(NULL)
  # esummary in one request: keep the candidate set to something a
  # single call can carry.
  ids <- head(ids, 200)
  s <- esummaries(ids)
  if (is.null(s) || !nrow(s)) return(NULL)

  # Volume and first page identify a paper outright when we have them.
  if (!is.na(work$volume[i]) && !is.na(work$page[i])) {
    firstPage <- sub("[^0-9].*$", "", s$pages)
    exact <- which(s$volume == as.character(work$volume[i]) &
                     firstPage == as.character(work$page[i]))
    if (length(exact) == 1)
      return(list(pmid = s$PMID[exact], how = "author+year, vol/page"))
  }

  want <- bagOf(work$title[i])
  if (length(want) < 3) return(NULL)
  score <- vapply(s$title, function(t) {
    got <- bagOf(t)
    if (!length(got)) 0 else length(intersect(want, got)) / length(want)
  }, numeric(1))
  o <- order(-score)
  best <- score[o[1]]
  runnerUp <- if (length(o) > 1) score[o[2]] else 0
  if (best >= 0.6 && best - runnerUp >= 0.2)
    return(list(pmid = s$PMID[o[1]],
                how = sprintf("author+year, title %.0f%%", 100 * best)))
  NULL
}

if (!offline) {
  todo <- which(is.na(work$PMID) & !is.na(work$volume) & nzchar(work$author))
  if (length(todo)) {
    cat("ecitmatch on", length(todo), "rows...\n")
    chunks <- split(todo, ceiling(seq_along(todo) / 50))
    for (ch in chunks) {
      rows <- work[ch, c("journal", "year", "volume", "page", "author")]
      rows$idx <- ch
      got <- ecitmatch(rows); pause()
      for (k in names(got)) if (!is.na(got[[k]])) {
        i <- as.integer(k)
        work$PMID[i] <- got[[k]]; work$HOW[i] <- "ecitmatch"
      }
      cat("  resolved so far:", sum(!is.na(work$PMID)), "\n")
    }
  }
  todo <- which(is.na(work$PMID))
  if (length(todo)) {
    cat("author+year lookup for", length(todo), "rows...\n")
    for (i in todo) {
      r <- resolveByAuthorYear(i)
      if (!is.null(r)) { work$PMID[i] <- r$pmid; work$HOW[i] <- r$how }
    }
    cat("  resolved so far:", sum(!is.na(work$PMID)), "\n")
  }
}
work$HOW[is.na(work$PMID)] <- "UNRESOLVED"

## --------------------------------------------- what we already have

pm <- read.csv(file.path(root, "corpus", "pmid_map.csv"),
               colClasses = "character")
have <- rep("", nrow(work))
inNew <- !is.na(work$PMID) &
  file.exists(file.path(newCarlisle, paste0("PMID_", work$PMID, ".pdf")))
have[inNew] <- ".NewCarlisle"
ip <- match(work$PMID, pm$PMID)
inCorpus <- !is.na(ip) & !inNew
have[inCorpus] <- file.path(corpusDir, pm$PDF[ip[inCorpus]])
work$HAVE <- have

## ------------------------------------------------------------- report

cat("\n== ", toupper(which_), " ==\n", sep = "")
cat("rows:", nrow(work), "\n")
print(table(work$HOW))
cat("\nalready in hand:", sum(nzchar(work$HAVE)),
    "  (", sum(work$HAVE == ".NewCarlisle"), "in .NewCarlisle,",
    sum(nzchar(work$HAVE) & work$HAVE != ".NewCarlisle"),
    "in the local corpus )\n")
cat("resolved but NOT in hand - to download:",
    sum(!is.na(work$PMID) & !nzchar(work$HAVE)), "\n")
cat("unresolved (no PMID; need a human or a better citation):",
    sum(is.na(work$PMID)), "\n")

outPath <- file.path(outDir, "pmids.csv")
write.csv(work, outPath, row.names = FALSE)
cat("\nWrote", outPath, "\n")
