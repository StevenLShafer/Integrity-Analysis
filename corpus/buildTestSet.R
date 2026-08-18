# Build corpus/TEST (Steve's item 3, 2026-08-17): every PDF that
#   (a) fully parses deterministically (all arms carry N, >= 3 continuous
#       rows, so the app can analyze it without hand-editing),
#   (b) has a known PMID present in BOTH Carlisle files, and
#   (c) is VALUE-VERIFIED: every parsed continuous (MEAN, SD) pair matches
#       a pair in Carlisle's One Sheet for that PMID.
# Also writes corpus/ExpectedResults.xlsx: the line-by-line expectation
# built from the Carlisle spreadsheets ALONE (no IntegrityAnalysis run) -
# per-variable values from the One Sheet, Carlisle's stored per-variable p
# (V columns) and trial p from the repaired wide file.
suppressMessages(library(openxlsx))
repo <- "C:/dev/IntegrityAnalysis"
old  <- "C:/Users/steve/AppData/Local/Temp/claude/C--dev-stanpumpR/538a0e78-bd25-420c-81ef-4fe378af46d5/scratchpad"

## inputs ------------------------------------------------------------------
S <- do.call(rbind, lapply(sort(list.files(file.path(old, "full3"),
       "^summary_.*rds$", full.names = TRUE)), readRDS))
R <- unlist(lapply(sort(list.files(file.path(old, "full3"),
       "^rows_.*rds$", full.names = TRUE)), readRDS), recursive = FALSE)
byPath <- stats::setNames(R, vapply(R, function(d) d$.path[1], character(1)))

pm <- read.csv(file.path(repo, "corpus", "pmid_map.csv"),
               colClasses = "character")
one <- read.xlsx(file.path(repo, "One Sheet Carlisle Data.xlsx"))
one$PMID <- as.character(one$PMID)
wide <- read.xlsx(file.path(repo, "Carlisle Data with PMIDs.xlsx"),
                  sheet = "All Data")
wide$pmid <- as.character(wide$PMID)
wide$p.value <- suppressWarnings(as.numeric(wide$p.value))
vcols <- grep("^V\\d+$", names(wide), value = TRUE)

key <- function(m, s) paste(sprintf("%.6g", m), sprintf("%.6g", s))

## selection ---------------------------------------------------------------
S$rel <- sub("^C:/temp/journals/", "", S$path)
S <- merge(S, pm, by.x = "rel", by.y = "PDF", all.x = TRUE)
cand <- S[S$ok & !is.na(S$PMID) & S$arms >= 2 & S$armsWithN == S$arms &
          S$continuous >= 3 & S$path %in% names(byPath), ]
cand <- cand[cand$PMID %in% one$PMID & cand$PMID %in% wide$pmid, ]
cat("candidates after parse/PMID filters:", nrow(cand), "\n")

selected <- character(0); pmids <- character(0)
for (i in seq_len(nrow(cand))) {
  d <- byPath[[cand$path[i]]]
  cont <- d[!is.na(d$MEAN) & !is.na(d$SD), , drop = FALSE]
  if (nrow(cont) < 3 || any(is.na(cont$N))) next
  carl <- one[one$PMID == cand$PMID[i], ]
  if (nrow(carl) == 0) next
  ok <- key(cont$MEAN, cont$SD) %in% key(carl$MEAN, carl$SD)
  if (all(ok)) {
    selected <- c(selected, cand$path[i])
    pmids <- c(pmids, cand$PMID[i])
  }
}
cat("fully value-verified PDFs:", length(selected), "\n")

## copy to corpus/TEST -----------------------------------------------------
dir.create(file.path(repo, "corpus", "TEST"), showWarnings = FALSE)
for (p in selected)
  file.copy(p, file.path(repo, "corpus", "TEST", basename(p)),
            overwrite = TRUE)
cat("copied to corpus/TEST:", length(selected), "PDFs\n")

## expected results, from Carlisle alone -----------------------------------
lines <- list(); trials <- list()
for (j in seq_along(selected)) {
  pmid <- pmids[j]
  carl <- one[one$PMID == pmid, ]
  w <- wide[wide$pmid == pmid, ][1, ]
  vp <- suppressWarnings(as.numeric(unlist(w[vcols])))
  lines[[j]] <- data.frame(
    FILE = basename(selected[j]), PMID = pmid,
    VARIABLE = carl$MEASURE, GROUP = carl$GROUP,
    N = carl$NUMBER.IN.GROUP, MEAN = carl$MEAN, SD = carl$SD,
    DECM = carl$DECM,
    CARLISLE_P_VARIABLE = vp[carl$MEASURE],
    stringsAsFactors = FALSE)
  trials[[j]] <- data.frame(
    FILE = basename(selected[j]), PMID = pmid,
    JOURNAL = w$Journal, TRIAL_NO = w$trial, YEAR = w$year,
    VARIABLES = sum(!is.na(vp)),
    CARLISLE_P_TRIAL = w$p.value, stringsAsFactors = FALSE)
}
wb <- createWorkbook()
addWorksheet(wb, "Lines");  writeData(wb, "Lines",  do.call(rbind, lines))
addWorksheet(wb, "Trials"); writeData(wb, "Trials", do.call(rbind, trials))
addWorksheet(wb, "README")
writeData(wb, "README", data.frame(NOTE = c(
  "Expected results for the corpus/TEST mass analysis, built from the",
  "Carlisle spreadsheets ALONE (no IntegrityAnalysis run).",
  "Lines: per-variable values (One Sheet) with Carlisle's stored",
  "per-variable p (V columns of the repaired 'with PMIDs' file, matched",
  "by variable number).",
  "Trials: Carlisle's stored one-sided trial p.value.",
  "Comparison caveats: Carlisle's p-values predate the mid-p adoption's",
  "exact tie handling and are a second independent Monte Carlo - expect",
  "agreement like the issue-3 validation (median |diff| ~0.01), not",
  "identity. TEST PDFs analyze from their PARSED tables, which may hold",
  "fewer variables than Carlisle entered by hand - compare per-variable",
  "where both exist.")))
saveWorkbook(wb, file.path(repo, "corpus", "ExpectedResults.xlsx"),
             overwrite = TRUE)
cat("ExpectedResults.xlsx:", length(unique(unlist(pmids))), "trials,",
    nrow(do.call(rbind, lines)), "lines\n")
