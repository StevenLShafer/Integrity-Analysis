# parseBaselineTableHeuristics.R - the deterministic extraction engine.
#
############################################################################
# Provenance                                                               #
# Ported 2026-08-15 by Claude Code (model: Claude Opus 5, Anthropic) from  #
# parseCovariateTable() in the Integrity-Analysis repository, drafted      #
# 2026-08-14 by Claude Code (model: Claude Fable 5) at Steve Shafer's      #
# request. The cell-level parsing logic is unchanged from that draft.      #
#                                                                          #
# Revised the same day, after runs against samples drawn from the corpus   #
# of 1,865 real journal PDFs in C:/temp/journals (5 articles, then 60,     #
# then a 250-article random sample - NOT the whole corpus) showed that the #
# original table-finding stage failed on most of them. Two defects, both   #
# fatal on real articles and both invisible against the synthetic          #
# fixtures:                                                                #
#                                                                          #
#   1. Journals are typeset in two columns, and a table usually sits in    #
#      one of them with body prose beside it. Clustering words into lines  #
#      by y across the whole page glued each table row onto a sentence of  #
#      prose, so the parser saw neither a caption nor a table. Lines are   #
#      now built inside one typographic column at a time                   #
#      (.ppPageBands() in pageLayout.R).                                   #
#   2. The caption was located with a regex over the joined line text and  #
#      only matched Arabic numerals, so "TABLE I Demographic data" - the   #
#      house style of Anaesthesia and CJA - never matched. Captions are    #
#      now found as adjacent words, Roman or Arabic                        #
#      (.ppCaptionAnchors()).                                              #
#                                                                          #
# Consequently the engine no longer picks one page by vocabulary and hopes #
# the table is on it. It enumerates every captioned table in the document, #
# scores each caption for baseline-ness, parses the most promising ones,   #
# and keeps whichever parse comes out best (.ppParseScore()).              #
#                                                                          #
# This file makes NO calls to any AI service. All table recognition is     #
# word-coordinate heuristics and regular expressions over the text layer   #
# extracted by pdftools (poppler), so the same PDF always gives the same   #
# answer and every number can be traced back to a printed cell.           #
#                                                                          #
# A third defect was found the same way and fixed here: a placebo arm       #
# headed "P" was being discarded as a p-value column, which also corrupted  #
# every row label in that table, since the label is everything left of the  #
# first surviving cell.                                                     #
#                                                                          #
# Status: run and verified against the synthetic PDFs in                   #
# tests/testthat/helper-syntheticPdf.R, the regression fixtures in         #
# tests/testthat/test-real-layouts.R, and a 250-article random sample of   #
# the corpus in C:/temp/journals, scored against Carlisle's hand-extracted #
# values. On that sample 70% of articles yield a table and 45% of the      #
# known mean/SD pairs are recovered exactly (see README "Validation").     #
# That is a drafting aid, not a substitute for reading the table: review   #
# every parsed value against the printed table before analyzing a          #
# submission.                                                              #
#                                                                          #
# Revised 2026-08-20 by Claude Code (model: Claude Fable 5) at Steve       #
# Shafer's request, after screening 654 RCT submissions from the A&A       #
# manuscript corpus (C:/Temp/AA) showed that SUBMITTED MANUSCRIPTS -       #
# the input the deployed app actually receives - defeated the engine in    #
# ways journal typography never does: margin line-number rails, legend     #
# sentences between the caption and the table, captions on a different     #
# page than their table, tables running over page breaks, and the gutter   #
# detector splitting a wide Word table into a labels band and a values     #
# band. Each repair is marked "2026-08-20" in place, and every layout is   #
# pinned as a synthetic fixture in test-manuscript-layouts.R. Run and      #
# verified against the 60-submission random sample and a 150-article      #
# corpus regression sample (see the PR for the measured numbers).          #
############################################################################

# How good is a candidate parse? Used to choose between the tables in a
# document, and between a column-segmented and a full-width reading of the
# same page. Rewards arms with a known N and variables actually extracted;
# penalises lines the parser had to skip.
#
# The three terms added 2026-08-20 (measured on the A&A submitted-manuscript
# corpus) steer the choice toward the reading that kept labels and values
# together. On a manuscript table the gutter detector can split the TABLE
# itself - labels in one band, values in the other - and the values-only
# band parses into nameless rows that used to outscore the full-width
# reading. Demographic vocabulary in the row labels is direct evidence of a
# baseline table; "Unnamed" rows and implausibly many arms are evidence of a
# mangled one.
.ppParseScore <- function(res) {
  if (is.null(res) || inherits(res, "error") || nrow(res$data) == 0) return(-Inf)
  contRows <- unique(res$data$ROW[!is.na(res$data$MEAN)])
  nCont    <- length(contRows)
  allRows  <- unique(res$data$ROW)
  nCat     <- length(setdiff(allRows, contRows))
  demo <- sum(grepl(paste0("(?i)\\bage\\b|\\bsex\\b|gender|\\bmale\\b|female|",
                           "weight|height|\\bbmi\\b|body\\s+mass|\\basa\\b"),
                    allRows, perl = TRUE))
  clusters <- if (is.null(res$clusters)) nrow(res$arms) else res$clusters
  # An arm N printed in the table is strong evidence this really is the
  # table; a RECOVERED N (n (%) derivation or document text) is worth
  # NOTHING here, deliberately: recovery text applies to every candidate
  # of the document, and any score credit for it lets recovery decide
  # WHICH table wins - it flipped one corpus file from Table 1 to a
  # results table whose clusters happened to match the document's numbers
  # (2026-08-21). Recovered Ns still reach the output; they just carry no
  # weight in choosing between candidate tables.
  headerN <- if (is.null(res$armNSource)) !is.na(res$arms$N) else
    !is.na(res$arms$N) & is.na(res$armNSource)
  # A row refused by the unique-count bracket ("47%" of n = 702) is a
  # CORRECT reading of a percent table, not evidence of a mangled parse -
  # penalising it like a parse error flipped candidate selection on two
  # corpus files (2026-08-21). Only the other skips count against a parse.
  hardSkips <- sum(!grepl("unique count", res$skipped$reason, fixed = TRUE))
  3 * sum(headerN) +
    2 * nCont + nCat +
    2 * (nrow(res$arms) >= 2) +
    2 * min(demo, 3) -
    2 * hardSkips -
    sum(grepl("^Unnamed", allRows)) -
    max(0, clusters - 6)
}

# Parse one prepared block of lines, starting at the caption line `capIdx`.
# This is the original engine, steps 3-10, with page selection and the
# two-column hack lifted out: by the time it is called, `lines` already
# contains only the lines of one typographic column.
.ppParseBlock <- function(lines, lineTexts, capIdx, trial, parenIsSD,
                          roundObsDelta, say,
                          textCands = NULL, textTotals = NULL,
                          pctApprox = FALSE) {

  # Footnote / end-of-table patterns. Checked BEFORE tokenizing, because a
  # footnote like "Values are mean +/- SD" itself contains a mean+/-SD-shaped
  # token.
  stopPattern <- paste0(
    "(?i)^(values|data|results|numbers|figures)\\s+(are|were)",
    "|presented\\s+as|expressed\\s+as|given\\s+as|shown\\s+as",
    "|^abbreviations?|^definition\\s+of",
    "|^(figure|fig\\.)\\s*\\d",
    "|^(\\*|\u2020|\u2021|\u00a7)\\s")
  footnoteInfo <- character(0)   # kept to help disambiguate "a (b)" cells

  # Walk the lines after the caption; classify each one.
  #   header - contains "n = 25"-style arm sizes
  #   data   - has at least one numeric token
  #   label  - no numbers: a category header, an arm name, or prose
  # The block ends at a footnote, another table caption, or sustained prose.
  tokensByLine <- vector("list", length(lines))
  kind         <- rep("pre", length(lines))
  blankRun     <- 0
  seenData     <- FALSE
  if (capIdx >= length(lines)) return(NULL)
  for (i in seq(capIdx + 1, length(lines))) {
    txt <- lineTexts[i]
    if (grepl(stopPattern, txt, perl = TRUE) ||
        grepl("(?i)^(table|tab\\.?)\\s+([0-9]{1,2}|[IVXLivxl]{1,4})\\b", txt,
              perl = TRUE)) {
      kind[i] <- "stop"
      footnoteInfo <- c(footnoteInfo, txt)
      extra <- seq(i + 1, min(i + 4, length(lines)))
      footnoteInfo <- c(footnoteInfo, lineTexts[extra])
      break
    }
    if (grepl("(?i)\\(?\\s*n\\s*=\\s*\\d+", txt, perl = TRUE)) {
      kind[i] <- "header"
      next
    }
    toks <- .ppTokenizeLine(lines[[i]])
    tokensByLine[[i]] <- toks
    if (nrow(toks) > 0) {
      kind[i] <- "data"
      blankRun <- 0
      seenData <- TRUE
    } else {
      kind[i] <- "label"
      blankRun <- blankRun + 1
      # A long numberless line inside a column is prose, not a row label -
      # but only once data has begun. Before the first data line, the
      # numberless lines are the caption's own legend sentences ("Values
      # are represented as mean - SD or numbers (percentages)."), which
      # manuscripts print between the caption and the table body; stopping
      # on them cost whole tables (2026-08-20, A&A submission corpus).
      if (seenData) {
        if (nrow(lines[[i]]) > 8 || blankRun >= 3) {
          kind[i] <- "stop"
          break
        }
      } else if (blankRun >= 6) {
        kind[i] <- "stop"
        break
      }
    }
  }

  # Some Word-converted manuscripts print the plus-minus sign as a plain
  # hyphen: the caption says "mean - SD" and the cells read "40.79-11.97",
  # which the tokenizer sees as a number followed by a negative number
  # inside one word. When the block itself announces that notation, such
  # contiguous pairs are re-read as mean +/- SD - but only on lines with at
  # least TWO pairs, so a lone "(0-100 scale)" annotation cannot fabricate
  # a baseline value (2026-08-20; compare the BJA plus-minus-as-dash repair
  # in utils.R, which is this defect in the opposite direction).
  dashSD <- any(grepl("(?i)mean\\s*[-\u2013\u2212]+\\s*s\\.?d\\b",
                      lineTexts, perl = TRUE))
  if (dashSD) for (i in which(kind == "data")) {
    t <- tokensByLine[[i]]
    if (is.null(t) || nrow(t) < 2) next
    j <- seq_len(nrow(t) - 1)
    pair <- j[t$type[j] == "plain" & t$type[j + 1] == "plain" &
              t$start[j + 1] == t$start[j] + nchar(t$text[j]) &
              !is.na(t$num1[j]) & t$num1[j] >= 0 &
              !is.na(t$num1[j + 1]) & t$num1[j + 1] < 0]
    if (length(pair) < 2) next
    t$type[pair] <- "meanSD"
    t$text[pair] <- paste0(t$text[pair], t$text[pair + 1])
    t$num2[pair] <- -t$num1[pair + 1]
    t$dec2[pair] <- t$dec1[pair + 1]
    t$x1[pair]   <- t$x1[pair + 1]
    t$mid[pair]  <- (t$x0[pair] + t$x1[pair]) / 2
    tokensByLine[[i]] <- t[-(pair + 1), , drop = FALSE]
  }
  # ---- Manuscript-genre repairs (2026-08-20) -------------------------------
  # Both patterns below were found on the A&A submitted-manuscript corpus;
  # journal typography rarely produces either. See test-manuscript-layouts.R.
  #
  # (1) "Race, %   <0.001": a category header whose only numeric content is
  #     p-value-shaped. Classified as data, it never becomes the category
  #     header, so every child row beneath it is skipped as a bare number.
  #     Reclassify it as a label line; its p-value goes with it.
  for (i in which(kind == "data")) {
    toks <- tokensByLine[[i]]
    lbl  <- .ppSquish(substr(paste(lines[[i]]$text, collapse = " "),
                             1, min(toks$start) - 1))
    if (nchar(lbl) >= 4 &&
        all(toks$type == "plain" & !is.na(toks$num1) & abs(toks$num1) < 1)) {
      kind[i] <- "label"
      lineTexts[i] <- lbl                 # the header text, minus its p-value
      tokensByLine[[i]] <- toks[0, , drop = FALSE]
    }
  }
  # (2) A numeric line ABOVE the (n = ...) header row - quintile bounds, a
  #     year range, dose levels - is not data. Left as data, its tokens seed
  #     the column clustering with x positions that belong to no arm, and
  #     the arm count explodes. Reclassified as a label, its words can still
  #     contribute to the arm names.
  headerAt <- which(kind == "header")
  if (length(headerAt) > 0 && headerAt[1] - capIdx <= 8) {
    early <- which(kind == "data")
    early <- early[early < headerAt[1]]
    if (length(early) <= 3)
      for (i in early) {
        kind[i] <- "label"
        tokensByLine[[i]] <- tokensByLine[[i]][0, , drop = FALSE]
      }
  }

  dataIdx <- which(kind == "data")
  if (length(dataIdx) == 0) return(NULL)
  firstData <- dataIdx[1]
  lastData  <- dataIdx[length(dataIdx)]

  # ---- Column clustering --------------------------------------------------
  allToks <- do.call(rbind, tokensByLine[dataIdx])
  cols    <- .ppClusterColumns(allToks$mid)

  # ---- Header: arm names and arm N ----------------------------------------
  headerIdx <- which(kind %in% c("header", "label"))
  headerIdx <- headerIdx[headerIdx > capIdx & headerIdx < firstData]
  # When a real header line exists, the label lines before it are the
  # caption's legend sentences, not arm names - manuscripts print those
  # between the caption and the table (2026-08-20).
  headerAt <- which(kind == "header")
  if (length(headerAt) > 0)
    headerIdx <- headerIdx[headerIdx >= headerAt[1]]
  armN    <- rep(NA_integer_, cols$n)
  armName <- rep(NA_character_, cols$n)

  # Arm N first, by character position: "(n= 19)" splits into two words, so
  # per-column word bucketing can lose the digits. Matching the joined line
  # and mapping the match back to word x positions - the tokenizer's own
  # technique - is robust to how poppler split the cell (2026-08-20).
  for (i in intersect(headerAt, seq(capIdx + 1, length(lines)))) {
    d <- lines[[i]]
    joined    <- paste(d$text, collapse = " ")
    wordStart <- cumsum(c(1, nchar(d$text) + 1))[seq_len(nrow(d))]
    wordEnd   <- wordStart + nchar(d$text) - 1
    m <- gregexpr("(?i)n\\s*=\\s*\\d[\\d,]*", joined, perl = TRUE)[[1]]
    if (m[1] == -1) next
    for (k in seq_along(m)) {
      s <- m[k]; e <- s + attr(m, "match.length")[k] - 1
      wFirst <- which(wordEnd >= s)[1]
      wLast  <- rev(which(wordStart <= e))[1]
      xmid   <- (d$x[wFirst] + d$x[wLast] + d$width[wLast]) / 2
      colk   <- cols$assign(xmid)
      nval   <- suppressWarnings(as.integer(gsub("\\D", "", substr(joined, s, e))))
      if (!is.na(nval) && is.na(armN[colk])) armN[colk] <- nval
    }
  }
  for (i in headerIdx) {
    d    <- lines[[i]]
    wMid <- d$x + d$width / 2
    wCol <- cols$assign(wMid)
    # Only words near a column centre belong to it, so a row-label header
    # such as "Characteristic" is not swept into the first arm.
    near <- abs(cols$centers[wCol] - wMid) <
              (if (cols$n > 1) min(diff(sort(cols$centers))) * 0.75 else 100)
    for (k in seq_len(cols$n)) {
      wtxt <- paste(d$text[near & wCol == k], collapse = " ")
      if (nchar(wtxt) == 0) next
      nMatch <- regmatches(wtxt, regexpr("(?i)n\\s*=\\s*(\\d+)", wtxt, perl = TRUE))
      if (length(nMatch) > 0 && is.na(armN[k]))
        armN[k] <- as.integer(sub("\\D+", "", nMatch))
      nameTxt <- .ppSquish(gsub("(?i)\\(?\\s*n\\s*=\\s*\\d+\\s*\\)?", "", wtxt, perl = TRUE))
      if (nchar(nameTxt) > 0)
        armName[k] <- .ppSquish(paste(ifelse(is.na(armName[k]), "", armName[k]), nameTxt))
    }
  }

  nRowPattern <- paste0("(?i)^(no\\.?\\s+of\\s+(patients|subjects|cases)|n",
                        "|number\\s+of\\s+(patients|subjects))$")

  # ---- Drop a p-value column ----------------------------------------------
  # A column header of a bare "P" is ambiguous: it is the usual heading of a
  # p-value column, but it is also how trials abbreviate a placebo arm. Only
  # "P value" (spelled out) is taken as conclusive on the header alone;
  # a bare "P" must also have cells that look like p-values, or a real
  # treatment arm gets discarded - which corrupts the neighbouring row label
  # as well, since the label is everything left of the first surviving cell.
  pCol <- integer(0)
  if (cols$n >= 2) {
    for (k in seq_len(cols$n)) {
      hdr  <- armName[k]
      toks <- allToks[cols$assign(allToks$mid) == k, ]
      pExplicit <- !is.na(hdr) && grepl("(?i)p[-\u2013 ]?values?|significance",
                                        hdr, perl = TRUE)
      pMaybe    <- !is.na(hdr) && grepl("^[Pp][.:]?$", .ppSquish(hdr))
      cellsLikeP <- nrow(toks) > 0 &&
        mean(toks$type %in% c("plain", "pctOnly") & !is.na(toks$num1) &
               toks$num1 < 1) > 0.5 &&
        is.na(armN[k])
      if (pExplicit || (pMaybe && cellsLikeP) || cellsLikeP)
        pCol <- c(pCol, k)
    }
  }
  arms  <- setdiff(seq_len(cols$n), pCol)
  nArms <- length(arms)
  if (nArms == 0) return(NULL)

  # ---- Recover missing arm N (2026-08-21) ----------------------------------
  # The single largest deficit found by comparing this engine against an
  # AI-only run of the 654-submission corpus: 583 skipped rows were blocked
  # ONLY on an unknown arm N. Two deterministic sources, table first:
  #
  # (a) The arm's own printed n (%) cells. "13 (68.4%)" pins the arm size
  #     to the integers consistent with the printed rounding - usually
  #     exactly one. Traceable entirely to cells on the page.
  # Recovery only runs when NO data-bearing cluster has an N yet. Measured
  # on the 654-submission corpus, every genuine recovery was of a table
  # with no printed arm sizes anywhere; when the real arms already carried
  # header Ns, recovery only decorated stray clusters (a "%" subcolumn, an
  # escaped p column, an "All" column) with phantom arm sizes.
  armNSource <- rep(NA_character_, cols$n)
  dataArms   <- intersect(arms, unique(cols$assign(allToks$mid)))
  # Eligibility is decided ONCE, before either source runs: a table whose
  # header printed no arm size at all. The n (%) derivation may then fill
  # some arms and the text the rest.
  recoveryEligible <- length(dataArms) > 0 && all(is.na(armN[dataArms]))
  if (recoveryEligible) {
    for (k in dataArms) {
      kt <- allToks[cols$assign(allToks$mid) == k & allToks$type == "nPct", ,
                    drop = FALSE]
      if (nrow(kt) == 0) next
      n <- .ppDeriveArmN(kt$num1, kt$num2, kt$dec2)
      if (!is.na(n)) {
        armN[k] <- n
        armNSource[k] <- sprintf(
          "derived from %d printed n (%%) cell(s) of this arm", nrow(kt))
        say("  arm ", k, ": N = ", n, " ", armNSource[k], ".")
      }
    }
  }
  # (b) The document text - the randomization sentence of the Methods, the
  #     abstract, or a CONSORT flow label with a text layer. Candidates and
  #     stated totals are extracted once per document by the caller; the
  #     assignment ladder (name match, elimination, position confirmed by
  #     the stated total) and its safeguards live in armNRecovery.R. Every
  #     N taken this way carries its source sentence, and reviewFlags()
  #     tells the reviewer to verify it against the CONSORT diagram.
  #     The same no-known-N gate applies, for the same measured reason.
  if (recoveryEligible && any(is.na(armN[dataArms])) &&
      !is.null(textCands) && nrow(textCands) > 0) {
    fill <- .ppFillArmNFromText(armN[dataArms], armName[dataArms], textCands,
                                if (is.null(textTotals)) integer(0)
                                else textTotals)
    newly <- is.na(armN[dataArms]) & !is.na(fill$N)
    armN[dataArms] <- fill$N
    armNSource[dataArms][newly] <- fill$source[newly]
    for (k in which(newly))
      say("  arm ", dataArms[k], ": N = ", fill$N[k], " from ",
          fill$source[k])
  }

  # ---- How to read "a (b)" cells ------------------------------------------
  footTxt <- paste(footnoteInfo, collapse = " ")
  footSaysMeanSD <- grepl("(?i)mean\\s*[(\u00b1]\\s*(sd|standard deviation)",
                          footTxt, perl = TRUE)
  # A footnote reading "Data are numbers (%)" settles the question that
  # "20 (66.7)" otherwise leaves open. Without this the cell defaulted to
  # mean-and-SD, turning a count and a percentage into a baseline statistic -
  # 20 patients, 66.7% of them, became a mean of 20 with an SD of 66.7.
  footSaysPercent <- grepl(
    paste0("(?i)(numbers?|counts?|figures?)\\s*\\(\\s*%",
           "|\\bn\\s*\\(\\s*%\\s*\\)",
           "|number\\s*\\(\\s*per\\s*cent"),
    footTxt, perl = TRUE)

  # Is the dispersion a standard deviation or a standard error? Papers print
  # one or the other and say which, usually in a footnote and sometimes in the
  # row label. Getting this wrong is not a rounding-level error: at n = 15 an
  # SE is roughly a quarter of the SD, so filing one as the other is out by a
  # factor of four. When the table says nothing the value goes in SD, which is
  # the overwhelming convention, and the assumption is recorded rather than
  # hidden - see the `dispersion` element of the returned object.
  seWord <- paste0("(?i)\\bs\\.?e\\.?m\\.?\\b|\\bs\\.?e\\.?\\b",
                   "|standard\\s+error")
  footSaysSE <- grepl(seWord, footTxt, perl = TRUE)
  footSaysSD <- grepl("(?i)\\bs\\.?d\\.?\\b|standard\\s+deviation", footTxt,
                      perl = TRUE)
  dispersionBasis <- if (footSaysSE && !footSaysSD) "se (stated)"
                     else if (footSaysSD && !footSaysSE) "sd (stated)"
                     else if (footSaysSE && footSaysSD) "mixed (per row)"
                     else "sd (assumed - table does not say)"
  tableHasPlusMinus <- any(allToks$type == "meanSD")
  continuousKeyword <- paste0(
    "(?i)age|weight|height|bmi|body\\s+mass|duration|time|pressure|rate|",
    "score|hemoglobin|haemoglobin|creatinine|glucose|albumin|dose|volume|",
    "length|circumference|temperature|count|level")

  # ---- Walk the data lines and build output rows --------------------------
  outRows      <- list()
  skipped      <- list()
  catHeader    <- NA_character_
  catHeaderPct <- FALSE        # did the category header announce percentages?
  catColumns   <- character(0)
  usedRowNames <- character(0)
  pctDerived   <- character(0) # rows whose counts were derived from percents
  pctApproxRows <- character(0) # rows using the opt-in approximation
  derivedCells <- list()       # (ROW, COL, KIND, NOTE) for the app grid
  addDerived <- function(rowName, colName, kind, note)
    derivedCells[[length(derivedCells) + 1]] <<-
      data.frame(ROW = rowName, COL = colName, KIND = kind,
                 NOTE = note, stringsAsFactors = FALSE)

  addSkip <- function(label, reason, txt)
    skipped[[length(skipped) + 1]] <<-
      data.frame(label = label, reason = reason, text = txt,
                 stringsAsFactors = FALSE)

  for (i in seq(firstData, lastData)) {
    if (kind[i] == "label") {
      lbl <- .ppCleanLabel(lineTexts[i])
      if (nchar(lbl) > 0 && nrow(lines[[i]]) <= 6) {
        catHeader <- lbl
        # "Race, %" / "ASA status, %": the children below are percentages
        catHeaderPct <- grepl("%", lineTexts[i], fixed = TRUE) ||
          grepl("(?i)\\bpercent", lineTexts[i], perl = TRUE)
      }
      next
    }
    if (kind[i] != "data") next
    toks <- tokensByLine[[i]]
    toks$col <- cols$assign(toks$mid)
    toks <- toks[toks$col %in% arms, , drop = FALSE]
    if (nrow(toks) == 0) next
    joined   <- paste(lines[[i]]$text, collapse = " ")
    rawLabel <- substr(joined, 1, min(toks$start) - 1)
    label    <- .ppCleanLabel(rawLabel)
    txt      <- lineTexts[i]

    armTok <- lapply(arms, function(k) {
      t <- toks[toks$col == k, , drop = FALSE]
      if (nrow(t) > 0) t[1, ] else NULL
    })
    types <- vapply(armTok, function(t) if (is.null(t)) NA_character_ else t$type,
                    character(1))
    mainType <- names(sort(table(types), decreasing = TRUE))[1]

    if (grepl(nRowPattern, label, perl = TRUE) && identical(mainType, "plain")) {
      for (j in seq_len(nArms))
        if (!is.null(armTok[[j]])) armN[arms[j]] <- as.integer(armTok[[j]]$num1)
      next
    }
    if (is.na(mainType)) next

    if (mainType == "medianRng") {
      addSkip(label, "median [range/IQR] - integrity analysis needs mean and SD", txt)
      next
    }
    # ---- Percent-block conversion (2026-08-21) ----------------------------
    # Three genres tabulate categorical data as bare percentages, found by
    # the AI comparison over the 654-submission corpus (~800 rows skipped):
    #   "Male 55%"                  - pctOnly cells
    #   "Race, %" then "Caucasian 47" - plain children under a % header
    #   "Gender (Male), % 47 44"    - a plain row whose own label says %
    # With a known arm N the printed percentage pins the count to the
    # integers consistent with its rounding (.ppCountFromPct); a cell is
    # converted ONLY when that bracket holds exactly one integer, so "47%"
    # of n = 40 becomes 19 while "47%" of n = 702 stays unconverted - a
    # fraud screen must not analyze approximated counts as printed ones.
    # Every converted row is recorded and reported by reviewFlags().
    rowSaysPct <- grepl("%", rawLabel, fixed = TRUE) ||
      grepl("(?i)\\bpercent", rawLabel, perl = TRUE)
    pctGenre <- mainType == "pctOnly" ||
      (mainType == "plain" &&
         ((!is.na(catHeader) && catHeaderPct) || rowSaysPct))
    pendingDerive <- NULL
    if (pctGenre) {
      present <- !vapply(armTok, is.null, logical(1))
      cnts   <- rep(NA_integer_, nArms)
      approx <- rep(FALSE, nArms)
      notes  <- rep(NA_character_, nArms)
      for (j in which(present)) {
        t <- armTok[[j]]
        if (!t$type %in% c("pctOnly", "plain")) next
        N <- armN[arms[j]]
        cnts[j] <- .ppCountFromPct(t$num1, t$dec1, N)
        if (!is.na(cnts[j])) {
          notes[j] <- sprintf("%s%% of N=%d -> %d (uniquely pinned)",
                              format(t$num1), N, cnts[j])
        } else if (isTRUE(pctApprox) && !is.na(N) &&
                   !is.na(t$num1) && t$num1 >= 0 && t$num1 <= 100) {
          # Opt-in APPROXIMATION (2026-08-21, Steve's request): the
          # bracket did not pin a unique count, so round(N x pct / 100)
          # is used - within half a printed unit of N/100 of the truth.
          # Recorded as approximate, painted green in the app grid, and
          # reported by reviewFlags(); never on by default.
          cnts[j]   <- max(0L, min(as.integer(N),
                                   as.integer(round(N * t$num1 / 100))))
          approx[j] <- TRUE
          notes[j]  <- sprintf(
            "APPROXIMATE: %s%% of N=%d -> round() = %d (not uniquely pinned)",
            format(t$num1), N, cnts[j])
        }
      }
      if (any(present) && !any(is.na(cnts[present]))) {
        for (j in which(present)) {
          armTok[[j]]$num1 <- cnts[j]
          armTok[[j]]$type <- "plain"
        }
        # The "%" in the label described the notation just converted
        # away; drop it so the category column is named "Diabetes", not
        # "Diabetes, %".
        label <- .ppCleanLabel(sub(
          "(?i)\\s*[,;]?\\s*\\(?\\s*(%|percent(age)?s?)\\s*\\)?\\s*$",
          "", label, perl = TRUE))
        shown <- if (nchar(label) > 0) label else catHeader
        if (any(approx[present])) pctApproxRows <- c(pctApproxRows, shown)
        else                      pctDerived    <- c(pctDerived, shown)
        # Remembered until the branch below knows the column names it
        # created; consumed there into $derivedCells for the app grid.
        pendingDerive <- list(
          kind = if (any(approx[present])) "approximate" else "unique",
          note = paste(notes[present][!is.na(notes[present])],
                       collapse = "; "))
        # Children of a category header accumulate into its row as counts;
        # a standalone percent row is a binary category with a complement,
        # exactly like a printed "n (%)" cell.
        mainType <- if (!is.na(catHeader)) "plain" else "nPct"
      } else {
        addSkip(if (nchar(label) > 0) label else txt,
                paste("percent only - needs the arm N, and the printed",
                      "percent must pin a unique count (or pctApprox =",
                      "TRUE); enter by hand"), txt)
        next
      }
    }

    if (mainType == "numParen") {
      labelSaysPct    <- grepl("(?i)\\(%\\)|percent", rawLabel, perl = TRUE)
      labelContinuous <- grepl(continuousKeyword, label, perl = TRUE)
      decision <-
        if (parenIsSD == "sd") "sd"
        else if (parenIsSD == "percent") "percent"
        else if (labelSaysPct) "percent"
        else if (labelContinuous || footSaysMeanSD) "sd"
        else if (tableHasPlusMinus) "percent"
        else if (footSaysPercent) "percent"
        else "sd"
      mainType <- if (decision == "sd") "meanSD" else "nPct"
      if (parenIsSD == "auto" && !labelSaysPct && !labelContinuous)
        say("  \"", label, "\": read \"a (b)\" as ",
            if (decision == "sd") "mean (SD)" else "n (%)",
            " - check, or set parenIsSD.")
    }

    if (mainType == "meanSD") {
      rowName <- .ppUniqueName(if (nchar(label) > 0) label else "Unnamed",
                               usedRowNames)
      usedRowNames <- c(usedRowNames, rowName)
      catHeader <- NA_character_
      catHeaderPct <- FALSE
      # A row label may override the table-level footnote: "Age, mean (SEM)"
      rowSaysSE <- grepl(seWord, rawLabel, perl = TRUE)
      rowSaysSD <- grepl("(?i)\\bs\\.?d\\.?\\b|standard\\s+deviation",
                         rawLabel, perl = TRUE)
      isSE <- if (rowSaysSE && !rowSaysSD) TRUE
              else if (rowSaysSD) FALSE
              else footSaysSE && !footSaysSD

      perArm <- lapply(seq_len(nArms), function(j) {
        t <- armTok[[j]]
        if (is.null(t) || !t$type %in% c("meanSD", "numParen")) return(NULL)
        list(N = armN[arms[j]], MEAN = t$num1,
             SD = if (isSE) NA_real_ else t$num2,
             SE = if (isSE) t$num2 else NA_real_,
             ROUND_MEAN = t$dec1,
             ROUND_DISPERSION = t$dec2,
             ROUND_OBSERVATION = t$dec1 + roundObsDelta)
      })
      outRows[[length(outRows) + 1]] <-
        list(row = rowName, type = "continuous", perArm = perArm)

    } else if (mainType == "fraction") {
      catHeader <- NA_character_
      catHeaderPct <- FALSE
      nParts <- max(vapply(armTok, function(t)
        if (is.null(t)) 0L else length(strsplit(t$text, "/")[[1]]), integer(1)))
      partNames <- NULL
      m <- regmatches(rawLabel, regexpr("\\(([^()]*/[^()]*)\\)", rawLabel))
      if (length(m) > 0) {
        partNames <- strsplit(gsub("[()]", "", m), "\\s*/\\s*")[[1]]
        label <- .ppCleanLabel(sub("\\(([^()]*/[^()]*)\\)", "", rawLabel))
      } else {
        m2 <- regmatches(rawLabel,
                         regexpr("[A-Za-z]+(\\s*/\\s*[A-Za-z]+)+\\s*[,:]?\\s*$",
                                 rawLabel))
        if (length(m2) > 0) {
          partNames <- strsplit(.ppSquish(m2), "\\s*/\\s*")[[1]]
          label <- .ppCleanLabel(sub("[A-Za-z]+(\\s*/\\s*[A-Za-z]+)+\\s*[,:]?\\s*$",
                                     "", rawLabel))
        }
      }
      if (is.null(partNames) || length(partNames) != nParts) {
        if (nParts == 2 && grepl("(?i)sex|gender", label, perl = TRUE))
          partNames <- c("Male", "Female")
        else
          partNames <- paste(label, seq_len(nParts))
      }
      if (grepl("(?i)sex|gender", label, perl = TRUE) && length(partNames) == 2) {
        partNames[toupper(partNames) %in% c("M", "MALE")]   <- "Male"
        partNames[toupper(partNames) %in% c("F", "FEMALE")] <- "Female"
      }
      partNames <- ifelse(nchar(partNames) <= 3 & !partNames %in% c("Male", "Female"),
                          paste(label, partNames), partNames)
      partNames <- vapply(partNames, .ppUniqueName, character(1),
                          existing = setdiff(catColumns, partNames))
      catColumns <- unique(c(catColumns, partNames))
      rowName <- .ppUniqueName(if (nchar(label) > 0) label else "Category",
                               usedRowNames)
      usedRowNames <- c(usedRowNames, rowName)
      perArm <- lapply(seq_len(nArms), function(j) {
        t <- armTok[[j]]
        if (is.null(t) || t$type != "fraction") return(NULL)
        counts <- as.integer(strsplit(gsub("\\s", "", t$text), "/")[[1]])
        stats::setNames(as.list(counts), partNames[seq_along(counts)])
      })
      outRows[[length(outRows) + 1]] <-
        list(row = rowName, type = "category", perArm = perArm)

    } else if (mainType == "nPct") {
      catHeader <- NA_character_
      catHeaderPct <- FALSE
      catName <- .ppUniqueName(if (nchar(label) > 0) label else "Category",
                               catColumns)
      complementName <- .ppUniqueName(paste("Not", catName),
                                      c(catColumns, catName))
      haveN <- all(!is.na(armN[arms]))
      catColumns <- unique(c(catColumns, catName, if (haveN) complementName))
      rowName <- .ppUniqueName(catName, usedRowNames)
      usedRowNames <- c(usedRowNames, rowName)
      if (haveN)
        say("  \"", label, "\": binary n (%) row - complement column \"",
            complementName, "\" computed as arm N minus the count.")
      else
        addSkip(label, paste("n (%) with unknown arm N - complement category",
                             "cannot be computed; edit by hand"), txt)
      perArm <- lapply(seq_len(nArms), function(j) {
        t <- armTok[[j]]
        if (is.null(t)) return(NULL)
        cnt <- as.integer(t$num1)
        out <- stats::setNames(list(cnt), catName)
        if (haveN) out[[complementName]] <- armN[arms[j]] - cnt
        out
      })
      if (!is.null(pendingDerive)) {
        addDerived(rowName, catName, pendingDerive$kind, pendingDerive$note)
        if (haveN)
          addDerived(rowName, complementName, pendingDerive$kind,
                     "complement: arm N minus the derived count")
      }
      outRows[[length(outRows) + 1]] <-
        list(row = rowName, type = "category", perArm = perArm)

    } else if (mainType == "plain") {
      if (!is.na(catHeader)) {
        catName <- .ppUniqueName(if (nchar(label) > 0) label else "Category",
                                 catColumns)
        catColumns <- unique(c(catColumns, catName))
        key <- paste0("__cat__", catHeader)
        existing <- which(vapply(outRows, function(r) identical(r$key, key),
                                 logical(1)))
        counts <- lapply(seq_len(nArms), function(j) {
          t <- armTok[[j]]
          if (is.null(t)) NULL else stats::setNames(list(as.integer(t$num1)), catName)
        })
        if (length(existing) == 0) {
          rowName <- .ppUniqueName(catHeader, usedRowNames)
          usedRowNames <- c(usedRowNames, rowName)
          outRows[[length(outRows) + 1]] <-
            list(row = rowName, type = "category", perArm = counts, key = key)
        } else {
          e <- existing[1]
          rowName <- outRows[[e]]$row
          for (j in seq_len(nArms))
            if (!is.null(counts[[j]]))
              outRows[[e]]$perArm[[j]] <- c(outRows[[e]]$perArm[[j]], counts[[j]])
        }
        if (!is.null(pendingDerive))
          addDerived(rowName, catName, pendingDerive$kind, pendingDerive$note)
      } else {
        addSkip(label, "bare number with no category header and no SD - not usable",
                txt)
      }
    }
  }

  if (length(outRows) == 0) return(NULL)

  # ---- Assemble the template-format data frame ----------------------------
  allCols <- c(.ppBaseColumns(), catColumns)
  rows <- list()
  for (r in outRows) {
    for (j in seq_len(nArms)) {
      v <- r$perArm[[j]]
      if (is.null(v)) next
      line <- stats::setNames(as.list(rep(NA, length(allCols))), allCols)
      line$TRIAL <- trial
      line$ROW   <- r$row
      if (r$type == "continuous") {
        line$N <- v$N; line$MEAN <- v$MEAN
        line$SD <- v$SD; line$SE <- v$SE
        line$ROUND_MEAN <- v$ROUND_MEAN
        line$ROUND_DISPERSION <- v$ROUND_DISPERSION
        line$ROUND_OBSERVATION <- v$ROUND_OBSERVATION
      } else {
        for (nm in names(v)) line[[nm]] <- v[[nm]]
      }
      rows[[length(rows) + 1]] <- line
    }
  }
  DATA <- do.call(rbind, lapply(rows, function(l)
    as.data.frame(l, check.names = FALSE, stringsAsFactors = FALSE)))

  skippedDf <- if (length(skipped) > 0) do.call(rbind, skipped) else
    data.frame(label = character(0), reason = character(0), text = character(0))

  # An "arm" that received neither an N nor a single data cell is not an
  # arm - it is a label-column word swept into the clustering (wide
  # manuscript tables put header text far left of the first value column).
  # Drop it from the report; no data line ever referenced it. The raw
  # cluster count is kept for the parse score, where an implausible number
  # of clusters is evidence of a mangled reading (2026-08-20).
  used <- vapply(seq_len(nArms), function(j)
    any(vapply(outRows, function(r)
      length(r$perArm) >= j && !is.null(r$perArm[[j]]), logical(1))),
    logical(1))
  keep <- used | !is.na(armN[arms])
  if (!any(keep)) keep <- rep(TRUE, nArms)

  list(data       = DATA,
       arms       = data.frame(arm = armName[arms][keep], N = armN[arms][keep],
                               stringsAsFactors = FALSE),
       armNSource = armNSource[arms][keep],
       derivedCounts = unique(pctDerived),
       approxCounts  = unique(pctApproxRows),
       derivedCells  = if (length(derivedCells)) do.call(rbind, derivedCells)
                       else NULL,
       clusters   = nArms,
       skipped    = skippedDf,
       dispersion = dispersionBasis)
}

#' Parse a baseline table with the deterministic engine only
#'
#' Reads the baseline characteristics table out of `pdfFile` using word
#' coordinates and regular expressions. No AI service is contacted, so the
#' result is reproducible and every value can be traced to a printed cell.
#' [parseBaselineTable()] wraps this function and adds the optional AI
#' fallback; call this one directly when you need a purely deterministic
#' answer.
#'
#' How it works, in order:
#'
#' 1. `pdftools::pdf_data()` gives every word on every page with its x/y
#'    position (points, origin top-left).
#' 2. Each page is split into its typographic columns by finding the vertical
#'    gutters that few text lines write into. This matters: journals are set
#'    in two columns, and a table usually sits in one of them with body prose
#'    beside it, so lines have to be built inside a column rather than across
#'    the page.
#' 3. Every captioned table in the document is located by finding the word
#'    "Table" (or "TABLE", or "Tab.") followed by a numeral, Arabic or Roman.
#'    Each caption is scored for how much it sounds like a baseline table.
#' 4. The most promising candidates are parsed and the best result is kept.
#'    Within a candidate: words are clustered into lines by y, numeric cells
#'    are recognized by regular expression (mean +/- SD, mean (SD), n (%),
#'    n/m fractions such as sex 15/10, median \[IQR\], plain counts), cell
#'    x-midpoints are clustered into treatment-arm columns, a p-value column
#'    is detected and dropped, arm names and N are read from the header lines,
#'    and rows are expanded to one output line per arm.
#'
#' Anything the parser could not use is reported in `$skipped` rather than
#' silently dropped.
#'
#' @param pdfFile Path to the article or submission PDF. The PDF must have a
#'   text layer; a scanned image must be run through OCR first (for example
#'   `pdftools::pdf_ocr_text()`).
#' @param trial Value for the TRIAL column. Defaults to the PDF file name.
#' @param pages Integer vector of pages to search. `NULL` (default) searches
#'   the whole document.
#' @param layout `"auto"` (default) tries both a column-segmented and a
#'   full-width reading of each candidate page and keeps whichever parses
#'   better; `"columns"` forces column segmentation; `"single"` forces
#'   full-width, which is right for a table that spans the page.
#' @param parenIsSD How to read "a (b)" cells that carry no percent sign:
#'   `"auto"` decides from the footnotes ("mean (SD)"), the row label
#'   ("n (%)"), and continuous-variable keywords (age, weight, ...);
#'   `"sd"` always reads mean (SD); `"percent"` always reads n (%).
#' @param roundObsDelta ROUND_OBSERVATION is set to ROUND_MEAN +
#'   `roundObsDelta`. Observations are often recorded with one more digit than
#'   the reported mean, hence the default of 1. Set to 0 to make them equal.
#' @param maxCandidates How many captioned tables to attempt before giving up.
#'   Candidates are tried in order of caption score.
#' @param ocr Read the pages with OCR instead of the text layer, for scanned
#'   articles that have no text layer at all. Needs the `tesseract` package.
#'   Everything downstream is unchanged — OCR word boxes are converted to the
#'   same coordinates `pdftools::pdf_data()` reports — but the characters
#'   themselves are now fallible, so treat the result with more suspicion than
#'   a text-layer parse.
#' @param ocrDpi Rendering resolution for OCR. Higher is slower and not
#'   necessarily better; 300 is a reasonable default for journal scans.
#' @param pctApprox Opt in to APPROXIMATE percent conversion. Percent-block
#'   cells are normally converted to counts only when the printed percentage
#'   and the arm N pin exactly one integer; with `pctApprox = TRUE`, cells
#'   the bracket cannot pin fall back to `round(N x pct / 100)`. Every such
#'   value is recorded in `$approxCounts` and `$derivedCells` and reported
#'   by [reviewFlags()] - it is a computed approximation (off by up to half
#'   a printed unit of N/100), not a printed datum. Default `FALSE`.
#' @param quiet Suppress the progress and summary messages.
#'
#' @return An object of class `ParsePDFTable`: a list with
#'   \describe{
#'     \item{data}{data frame in Integrity-Analysis template layout - TRIAL,
#'       ROW, N, MEAN, SD, ROUND_MEAN, ROUND_OBSERVATION, then one column per
#'       category.}
#'     \item{arms}{data frame of arm names and arm N read from the header.}
#'     \item{skipped}{data frame of table lines that could not be used, with
#'       the reason. Review these by hand.}
#'     \item{provenance}{one row per output line recording which engine
#'       produced it - `"heuristic"` throughout, for this function.}
#'     \item{pages}{the page the table was found on.}
#'     \item{caption}{the table caption line, as read.}
#'     \item{engine}{`"heuristic"`.}
#'   }
#'
#' @seealso [parseBaselineTable()] for the hybrid entry point,
#'   [writeIntegrityTemplate()] to write the result to a spreadsheet.
#' @export
parseBaselineTableHeuristics <- function(pdfFile,
                                         trial         = tools::file_path_sans_ext(basename(pdfFile)),
                                         pages         = NULL,
                                         layout        = c("auto", "columns", "single"),
                                         parenIsSD     = c("auto", "sd", "percent"),
                                         roundObsDelta = 1,
                                         maxCandidates = 6,
                                         ocr           = FALSE,
                                         ocrDpi        = 300,
                                         pctApprox     = FALSE,
                                         quiet         = FALSE)
{
  layout    <- match.arg(layout)
  parenIsSD <- match.arg(parenIsSD)
  # A Word manuscript takes its own route (issue 19): the parameter keeps
  # its historical name `pdfFile` for API stability, and dispatch happens
  # HERE - inside the exported function - so parseOne.R and every other
  # caller need no change. `pages`, `layout`, and `ocr` have no meaning
  # for a .docx and are ignored.
  if (grepl("[.]docx$", pdfFile, ignore.case = TRUE))
    return(parseBaselineTableDocx(pdfFile, trial = trial,
                                  parenIsSD = parenIsSD,
                                  roundObsDelta = roundObsDelta,
                                  maxCandidates = maxCandidates,
                                  pctApprox = pctApprox, quiet = quiet))
  if (!requireNamespace("pdftools", quietly = TRUE))
    stop("Package 'pdftools' is required: install.packages('pdftools')")
  say <- function(...) if (!quiet) message(...)

  allPages <- if (isTRUE(ocr)) .ppOcrData(pdfFile, dpi = ocrDpi)
              else .ppPdfData(pdfFile)
  # Submitted manuscripts number every line down the left margin; strip the
  # rail before anything downstream sees it (2026-08-20, see pageLayout.R).
  allPages <- lapply(allPages, .ppStripLineNumberRail)
  # Arm-N recovery candidates are document-level constants: the "(n = 24)"
  # mentions with allocation-flavoured context, and the stated randomized
  # totals that confirm a positional assignment. Extracted once here, used
  # by every candidate parse (2026-08-21, see armNRecovery.R).
  fullText   <- if (isTRUE(ocr)) character(0) else
    tryCatch(.ppPdfText(pdfFile), error = function(e) character(0))
  textCands  <- .ppArmNCandidatesFromText(fullText)
  textTotals <- .ppRandomizedTotals(fullText)
  nWords   <- sum(vapply(allPages, nrow, integer(1)))
  if (length(allPages) == 0 || nWords == 0)
    stop("No text layer found in ", pdfFile,
         if (isTRUE(ocr)) " (OCR produced no words either)."
         else " - it is a scanned image. Re-run with ocr = TRUE.")
  pageIdx <- if (is.null(pages)) seq_along(allPages) else
    intersect(pages, seq_along(allPages))
  if (length(pageIdx) == 0)
    stop("No such page in ", pdfFile, ".")

  # ---- Enumerate candidate tables ----------------------------------------
  modes <- switch(layout,
                  auto    = c("columns", "single"),
                  columns = "columns",
                  single  = "single")

  cand <- list()
  # Look-ahead bookkeeping (2026-08-20): a caption with no data beneath it -
  # at the foot of a page, or on a caption-list page, both customary in
  # submitted manuscripts - announces a table that lives on the NEXT page
  # with no caption of its own. Such pages are recorded here and turned into
  # full-width candidates below, each carrying its caption's score.
  lookScore   <- list()
  lookCaption <- list()
  for (p in pageIdx) {
    w <- allPages[[p]]
    if (is.null(w) || nrow(w) == 0) next
    for (mode in modes) {
      bands <- if (mode == "columns") .ppPageBands(w)
               else data.frame(x0 = -Inf, x1 = Inf)
      if (mode == "columns" && nrow(bands) == 1 && "single" %in% modes) next
      for (b in seq_len(nrow(bands))) {
        bw <- .ppWordsInBand(w, bands[b, ])
        # The 4-word floor only serves the look-ahead: a caption-list page
        # can be a handful of words, and its anchors must still be seen.
        # Parseable candidates keep the original 10-word / 3-line floor.
        if (nrow(bw) < 4) next
        lines <- .ppBuildLines(bw)
        if (length(lines) < 1) next
        lineTexts <- vapply(lines, .ppLineText, character(1))
        anchors <- .ppCaptionAnchors(bw)
        if (nrow(anchors) == 0) next
        for (a in seq_len(nrow(anchors))) {
          # Which line holds this caption?
          li <- which.min(vapply(lines,
                                 function(L) min(abs(L$y - anchors$y[a])),
                                 numeric(1)))
          # A "Table N" inside a sentence is worth trying only as a last
          # resort, so it is penalised rather than dropped.
          cs <- .ppCaptionScore(lineTexts[li]) -
            if (isTRUE(anchors$startsBlock[a])) 0 else 5
          if (nrow(bw) >= 10 && length(lines) >= 3)
            cand[[length(cand) + 1]] <- list(
              page = p, mode = mode, band = b, lines = lines,
              lineTexts = lineTexts, capIdx = li,
              caption = lineTexts[li], capScore = cs)
          if (isTRUE(anchors$startsBlock[a]) && p < length(allPages)) {
            # Fewer than two data-looking lines (two or more printed
            # numbers) below the caption: the table is not on this page.
            below <- if (li < length(lineTexts))
              lineTexts[seq(li + 1, length(lineTexts))] else character(0)
            dataish <- sum(vapply(below, function(t)
              sum(gregexpr("[0-9]+", t)[[1]] > 0) >= 2, logical(1)))
            key <- as.character(p + 1)
            prev <- if (is.null(lookScore[[key]])) -Inf else lookScore[[key]]
            if (dataish < 2 && cs > prev) {
              lookScore[[key]]   <- cs
              lookCaption[[key]] <- lineTexts[li]
            }
          }
        }
      }
    }
  }

  # Materialise the look-ahead candidates: the page after a data-less
  # caption, read full width from its top (capIdx = 0 starts .ppParseBlock()
  # at the first line).
  for (key in names(lookScore)) {
    p2 <- as.integer(key)
    w2 <- allPages[[p2]]
    if (is.null(w2) || nrow(w2) < 10) next
    lines2 <- .ppBuildLines(w2)
    if (length(lines2) < 2) next
    cand[[length(cand) + 1]] <- list(
      page = p2, mode = "single", band = 1, lines = lines2,
      lineTexts = vapply(lines2, .ppLineText, character(1)),
      capIdx = 0L, caption = lookCaption[[key]], capScore = lookScore[[key]])
  }

  # No caption anywhere: fall back to the old behaviour of scoring pages by
  # vocabulary and reading from the top of the best one.
  if (length(cand) == 0) {
    scores <- vapply(allPages[pageIdx], .ppScorePage, numeric(1))
    p      <- pageIdx[which.max(scores)]
    say("No table caption found; falling back to page ", p,
        " (highest baseline-vocabulary score).")
    for (mode in modes) {
      bands <- if (mode == "columns") .ppPageBands(allPages[[p]])
               else data.frame(x0 = -Inf, x1 = Inf)
      for (b in seq_len(nrow(bands))) {
        bw <- .ppWordsInBand(allPages[[p]], bands[b, ])
        if (nrow(bw) < 10) next
        lines <- .ppBuildLines(bw)
        if (length(lines) < 3) next
        cand[[length(cand) + 1]] <- list(
          page = p, mode = mode, band = b, lines = lines,
          lineTexts = vapply(lines, .ppLineText, character(1)),
          capIdx = 1L, caption = NA_character_, capScore = 0)
      }
    }
  }
  if (length(cand) == 0)
    stop("No table caption and no parseable page were found in ", pdfFile,
         " (", nWords, " words of text). Try the `pages` argument.")

  # What the caption says outranks how big the table is. A results table can
  # be much larger than the baseline table and would otherwise win on parse
  # score alone, which is how "Table 3 Pain scores" got returned in place of
  # "Table 1 Patient characteristics". So: if any caption clearly announces a
  # baseline table, only those candidates are considered, and the parse score
  # merely breaks ties among them.
  capScores <- vapply(cand, function(x) x$capScore, numeric(1))
  pageOf    <- vapply(cand, function(x) x$page, numeric(1))
  isStrong  <- capScores >= 3
  # Strong captions are *preferred*, not exclusive: many tables carry a bland
  # caption, and several strong-looking ones turn out to be unparseable, so
  # the weaker candidates still have to be tried rather than abandoned.
  ord  <- c(order(-capScores, pageOf)[isStrong[order(-capScores, pageOf)]],
            order(-capScores, pageOf)[!isStrong[order(-capScores, pageOf)]])
  cand <- cand[ord]
  strongOrdered <- isStrong[ord]

  tried <- 0L
  best <- NULL; bestScore <- -Inf; bestCand <- NULL; bestStrong <- FALSE
  for (i in seq_along(cand)) {
    cc <- cand[[i]]
    if (tried >= maxCandidates && bestScore > -Inf) break
    tried <- tried + 1L
    res <- tryCatch(
      .ppParseBlock(cc$lines, cc$lineTexts, cc$capIdx, trial, parenIsSD,
                    roundObsDelta, function(...) invisible(NULL),
                    textCands = textCands, textTotals = textTotals,
                    pctApprox = pctApprox),
      error = function(e) NULL)
    sc <- .ppParseScore(res)
    if (!is.finite(sc)) next
    sc <- sc + 2 * cc$capScore
    # A table whose caption announces baseline data beats any table whose
    # caption does not, however large the latter is. Only within one class
    # does the parse score decide.
    better <- if (strongOrdered[i] != bestStrong) strongOrdered[i] else
      sc > bestScore
    if (better) {
      bestScore  <- sc; best <- res; bestCand <- cc
      bestStrong <- strongOrdered[i]
    }
  }

  if (is.null(best))
    stop("No usable baseline table could be parsed from ", pdfFile,
         ". Try the `pages` or `layout` argument, or ai = \"always\".")

  # ---- Continuation onto following pages (2026-08-20) ----------------------
  # Manuscript tables regularly run over the page break, with no repeated
  # caption on the continuation page; the parse used to end at the bottom of
  # the caption's page and silently lose the rest. A following page is
  # appended when it opens with data-looking lines and no caption of its
  # own, and the extension is kept only if the parse score improves - a page
  # of prose adds skipped lines and lowers it.
  # Only a full-width winner is extended: appending full-width lines to a
  # column band would mix two different readings of the page, and journal
  # two-column tables repeat their caption when they continue anyway.
  bestPages <- bestCand$page
  if (bestCand$mode == "single") {
    extLines <- bestCand$lines
    extTexts <- bestCand$lineTexts
    p2 <- bestCand$page
    while (p2 < length(allPages)) {
      p2 <- p2 + 1
      w2 <- allPages[[p2]]
      if (is.null(w2) || nrow(w2) < 10) break
      lines2 <- .ppBuildLines(w2)
      if (length(lines2) < 2) break
      lt2   <- vapply(lines2, .ppLineText, character(1))
      head6 <- utils::head(lt2, 6)
      nNum  <- vapply(head6, function(t)
        sum(gregexpr("[0-9]+", t)[[1]] > 0), integer(1))
      if (sum(nNum >= 2) < 2) break
      if (any(grepl("(?i)^(table|tab\\.?)\\s+([0-9]{1,2}|[IVXLivxl]{1,4})\\b",
                    utils::head(lt2, 3), perl = TRUE))) break
      resExt <- tryCatch(
        .ppParseBlock(c(extLines, lines2), c(extTexts, lt2), bestCand$capIdx,
                      trial, parenIsSD, roundObsDelta,
                      function(...) invisible(NULL),
                      textCands = textCands, textTotals = textTotals,
                      pctApprox = pctApprox),
        error = function(e) NULL)
      if (.ppParseScore(resExt) <= .ppParseScore(best)) break
      best      <- resExt
      extLines  <- c(extLines, lines2)
      extTexts  <- c(extTexts, lt2)
      bestPages <- c(bestPages, p2)
    }
  }

  say("Table on page ", bestCand$page,
      if (bestCand$mode == "columns")
        paste0(" (column ", bestCand$band, ")") else " (full width)",
      if (!is.na(bestCand$caption))
        paste0(": \"", substr(bestCand$caption, 1, 60), "\"") else "")
  say("Parsed ", length(unique(best$data$ROW)), " variable(s) x ",
      nrow(best$arms), " arm(s) = ", nrow(best$data), " template lines.")
  if (nrow(best$skipped) > 0) {
    say("SKIPPED ", nrow(best$skipped), " line(s) - review these by hand:")
    for (s in seq_len(nrow(best$skipped)))
      say("  - ", best$skipped$label[s], ": ", best$skipped$reason[s])
  }

  structure(
    list(data       = best$data,
         arms       = best$arms,
         skipped    = best$skipped,
         provenance = data.frame(ROW = best$data$ROW,
                                 ENGINE = rep("heuristic", nrow(best$data)),
                                 stringsAsFactors = FALSE),
         pages      = bestPages,
         caption    = bestCand$caption,
         trial      = trial,
         layout     = bestCand$mode,
         dispersion = best$dispersion,
         armNSource = best$armNSource,
         derivedCounts = best$derivedCounts,
         approxCounts  = best$approxCounts,
         derivedCells  = best$derivedCells,
         engine     = "heuristic"),
    class = "ParsePDFTable")
}
