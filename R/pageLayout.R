# pageLayout.R - finding the table page, its lines, and its columns.
#
############################################################################
# Provenance                                                               #
# Ported 2026-08-15 by Claude Code (model: Claude Opus 5, Anthropic) from  #
# parseCovariateTable.R in the Integrity-Analysis repository (drafted      #
# 2026-08-14 by Claude Code, model Claude Fable 5). Logic unchanged; only  #
# the `.pcv` -> `.pp` rename and the file split.                          #
# Deterministic: no AI service is called here.                            #
# Status: run and verified by tests/testthat/test-page-layout.R.          #
############################################################################

# ---------------------------------------------------------------------------
# Page scoring: which page holds the baseline covariate table?
# ---------------------------------------------------------------------------
# Scores are additive vocabulary hits plus a numeric-density term, so a page
# of prose that merely says "baseline" loses to the page that says
# "baseline characteristics" over a block of mean +/- SD cells.
.ppScorePage <- function(pageWords) {
  txt <- paste(pageWords$text, collapse = " ")
  score <- 0
  score <- score + 4 * grepl("(?i)baseline\\s+(patient\\s+|demographic\\s+)?characteristics", txt, perl = TRUE)
  score <- score + 3 * grepl("(?i)patient\\s+characteristics", txt, perl = TRUE)
  score <- score + 2 * grepl("(?i)demographic", txt, perl = TRUE)
  score <- score + 2 * grepl("(?i)\\bbaseline\\b", txt, perl = TRUE)
  score <- score + 2 * grepl("(?i)table\\s+1\\b", txt, perl = TRUE)
  # Numeric density typical of a Table 1
  score <- score + min(4, sum(grepl(.ppPLUSMINUS, pageWords$text, fixed = TRUE)) / 3)
  score <- score + 1 * grepl("(?i)\\bn\\s*=\\s*\\d+", txt, perl = TRUE)
  score
}

# ---------------------------------------------------------------------------
# Page columns: where are the article's text columns?
# ---------------------------------------------------------------------------
# Journal articles are typeset in two (sometimes three) columns, and a table
# usually sits inside one of them with body prose beside it. Clustering words
# into lines by y across the whole page width therefore glues each table row
# onto a sentence of unrelated prose - which is fatal, because the row label
# and the arm columns end up interleaved with words. Splitting the page into
# its typographic columns first is what makes real articles parseable.
#
# A column gutter is a vertical band of x that almost no line writes into.
# "Almost" matters: the running head, the article title, and a full-width
# footnote all cross the gutter, so a strict page-wide emptiness test finds
# nothing on a real page. We therefore measure, for each 1-point x bin, the
# fraction of text lines that cover it, and call a run of low-coverage bins a
# gutter.
#
# Returns a data frame of bands with columns x0, x1 - a single row spanning
# the page when no gutter is convincing.
.ppPageBands <- function(pageWords, minGap = 12, maxCover = 0.08,
                         minBandFrac = 0.2) {
  single <- function(w) data.frame(x0 = -Inf, x1 = Inf)
  if (is.null(pageWords) || nrow(pageWords) < 40) return(single(pageWords))

  left  <- min(pageWords$x)
  right <- max(pageWords$x + pageWords$width)
  W     <- right - left
  if (!is.finite(W) || W <= 0) return(single(pageWords))

  lines <- .ppBuildLines(pageWords)
  if (length(lines) < 8) return(single(pageWords))

  # Coverage: for each x bin, the fraction of lines that have a word over it.
  nBin  <- max(10L, as.integer(ceiling(W)))
  cover <- numeric(nBin)
  binOf <- function(x) pmin(nBin, pmax(1L, as.integer(floor(x - left)) + 1L))
  for (L in lines) {
    hit <- logical(nBin)
    for (i in seq_len(nrow(L)))
      hit[seq.int(binOf(L$x[i]), binOf(L$x[i] + L$width[i]))] <- TRUE
    cover <- cover + hit
  }
  cover <- cover / length(lines)

  # Candidate gutters: long low-coverage runs away from the page margins.
  r     <- rle(cover <= maxCover)
  ends  <- cumsum(r$lengths)
  start <- ends - r$lengths + 1L
  cand  <- which(r$values & r$lengths >= minGap &
                 start > minBandFrac * nBin & ends < (1 - minBandFrac) * nBin)
  if (length(cand) == 0) return(single(pageWords))

  # Keep at most two gutters (a three-column layout); prefer the widest.
  cand <- cand[order(r$lengths[cand], decreasing = TRUE)]
  cand <- utils::head(sort(cand), 2)

  cuts  <- left + (start[cand] + ends[cand]) / 2
  edges <- c(left - 1, cuts, right + 1)
  bands <- data.frame(x0 = utils::head(edges, -1), x1 = utils::tail(edges, -1))
  # A band narrower than minBandFrac of the page is not a text column.
  bands <- bands[(bands$x1 - bands$x0) >= minBandFrac * W, , drop = FALSE]
  if (nrow(bands) < 2) return(single(pageWords))
  bands$x0[1] <- -Inf
  bands$x1[nrow(bands)] <- Inf
  bands
}

# Words whose horizontal centre falls inside one band.
.ppWordsInBand <- function(pageWords, band) {
  mid <- pageWords$x + pageWords$width / 2
  pageWords[mid > band$x0 & mid <= band$x1, , drop = FALSE]
}

# ---------------------------------------------------------------------------
# Table captions: "Table 1", "TABLE I", "Tab. 2"
# ---------------------------------------------------------------------------
# Numbering style varies by journal - Anaesthesia and CJA print Roman
# numerals, most others Arabic - so both are matched. The anchor is found as a
# pair of adjacent words rather than by a regex over the joined line, because
# on a two-column page the joined line may contain prose from the other
# column.
.ppCaptionAnchors <- function(pageWords) {
  empty <- data.frame(x = numeric(0), y = numeric(0), startsBlock = logical(0))
  if (is.null(pageWords) || nrow(pageWords) == 0) return(empty)
  w    <- pageWords[order(pageWords$y, pageWords$x), ]
  isTb <- grepl("^(?i)(table|tab\\.?)$", w$text, perl = TRUE)
  isNo <- grepl("^([0-9]{1,2}|[IVXLivxl]{1,4})[.:)]?$", w$text)
  # "Table" immediately followed by a numeral on the same visual line
  hit  <- which(isTb & c(utils::tail(isNo, -1), FALSE) &
                c(abs(diff(w$y)) <= 3, FALSE))
  if (length(hit) == 0) return(empty)

  # A caption begins a block of text; a cross-reference sits inside a
  # sentence ("as demonstrated in Table 3 B and C, where ..."), with a word
  # right before it. Without this test the prose under such a mention gets
  # parsed as a table.
  #
  # The test is a *gap* to the left, not the absence of anything to the left:
  # a full-width table at the foot of a two-column page has its caption on the
  # same visual line as the other column's prose, and requiring nothing to the
  # left would reject exactly those captions.
  # Rather than discard the cross-references, flag them: they are ranked last
  # so a real caption always wins, but they remain available when a document
  # yields nothing else. Excluding them outright cost real tables whose
  # captions this test misjudged.
  startsBlock <- vapply(hit, function(i) {
    same <- abs(w$y - w$y[i]) <= 3 & w$x < w$x[i]
    if (!any(same)) return(TRUE)
    (w$x[i] - max(w$x[same] + w$width[same])) >= 30
  }, logical(1))
  data.frame(x = w$x[hit], y = w$y[hit], startsBlock = startsBlock)
}

# How much does this caption look like a baseline-characteristics table?
# Used to choose between "Table 1 Patient characteristics" and "Table 2
# Intraoperative drug usage" on the same page.
.ppCaptionScore <- function(txt) {
  s <- 0
  s <- s + 4 * grepl("(?i)baseline", txt, perl = TRUE)
  s <- s + 4 * grepl("(?i)demographic|anthropometric", txt, perl = TRUE)
  # "Characteristics" only means baseline data when it is qualified. Bare
  # "Characteristics of sensory and motor blocks" is a results table, and
  # scoring it as a baseline table made it beat the real one.
  qualChar <- paste0("(?i)(patient|baseline|demographic|clinical|subject",
                     "|participant|study|group)s?[' ]*\\s*characteristic",
                     "|characteristics\\s+of\\s+(the\\s+)?",
                     "(patient|subject|participant|study|group|population)")
  s <- s + 3 * grepl(qualChar, txt, perl = TRUE)
  s <- s + 1 * (grepl("(?i)characteristic", txt, perl = TRUE) &&
                  !grepl(qualChar, txt, perl = TRUE))
  s <- s + 2 * grepl("(?i)patient(s)?\\s+(data|profile|detail)", txt, perl = TRUE)
  s <- s + 1 * grepl("(?i)preoperative|pre-operative|on\\s+admission", txt, perl = TRUE)
  s <- s + 1 * grepl("(?i)\\bpatients?\\b|\\bsubjects?\\b|\\bgroups?\\b", txt, perl = TRUE)
  # Baseline data is nearly always the first table, so its number is evidence
  # in its own right - enough to separate "Table 1 Patient data" from
  # "Table 4 Patient data at 24 h".
  s <- s + 2 * grepl("^\\s*(table|tab\\.?)\\s+(1|I)\\b", txt, perl = TRUE)
  # Tables of results are not baseline tables, even when they tabulate people.
  # But the penalty must not override an explicit announcement: "Table 1
  # Baseline and pre- and intra-operative data" is a baseline table that
  # happens to mention intra-operative variables, and docking it for that
  # pushed a real baseline table below the threshold.
  saysBaseline <- grepl("(?i)baseline|demographic", txt, perl = TRUE) ||
    grepl(qualChar, txt, perl = TRUE)
  if (!saysBaseline)
    s <- s - 3 * grepl(paste0("(?i)outcome|complication|adverse|side.?effect",
                              "|intra-?operative|post-?operative|pain score",
                              "|recovery|haemodynamic|hemodynamic"),
                       txt, perl = TRUE)
  s
}

# Which page carries the most baseline-like table caption?
#
# This is how a page is chosen for the AI engine, and it matters more than it
# looks: sending the model the wrong page is indistinguishable, from its side,
# from an article with no baseline table. Selecting by caption instead of by
# the vocabulary score in .ppScorePage() took the table fallback from 50% to
# 79% of known values over the corpus trials it is meant to rescue, and turned
# seven outright "no table on this page" refusals into answers.
#
# Returns NULL when the document has no caption at all, leaving the caller to
# fall back on .ppScorePage().
.ppBestCaptionPage <- function(allPages, pageIdx = seq_along(allPages)) {
  best <- -Inf
  bestPage <- NULL
  for (p in pageIdx) {
    w <- allPages[[p]]
    if (is.null(w) || nrow(w) == 0) next
    bands <- .ppPageBands(w)
    for (b in seq_len(nrow(bands))) {
      bw <- .ppWordsInBand(w, bands[b, ])
      if (nrow(bw) < 10) next
      anchors <- .ppCaptionAnchors(bw)
      if (nrow(anchors) == 0) next
      lines <- .ppBuildLines(bw)
      lt    <- vapply(lines, .ppLineText, character(1))
      for (i in seq_len(nrow(anchors))) {
        li <- which.min(vapply(lines, function(L) min(abs(L$y - anchors$y[i])),
                               numeric(1)))
        # No penalty for an anchor that does not begin its line, unlike the
        # candidate ranking in parseBaselineTableHeuristics(). There, a
        # penalised candidate is still tried; here exactly one page is chosen
        # and there is no second chance, and the penalty demoted the correct
        # page in two of twenty-one corpus trials - both of which then came
        # back as "no table on this page". A bare cross-reference scores low
        # on caption text anyway, so the penalty buys little here.
        s <- .ppCaptionScore(lt[li])
        if (s > best) { best <- s; bestPage <- p }
      }
    }
  }
  bestPage
}

# ---------------------------------------------------------------------------
# Lines: cluster the words of a page into visual lines by y coordinate
# ---------------------------------------------------------------------------
.ppBuildLines <- function(pageWords, yTol = 3) {
  pageWords <- pageWords[order(pageWords$y, pageWords$x), ]
  # New line whenever the y gap to the previous word exceeds yTol points
  lineId <- cumsum(c(1, diff(pageWords$y) > yTol))
  lines  <- split(pageWords, lineId)
  lines  <- lapply(lines, function(d) d[order(d$x), ])
  # Keep reading order (top to bottom)
  lines[order(vapply(lines, function(d) min(d$y), numeric(1)))]
}

.ppLineText <- function(line) .ppSquish(paste(line$text, collapse = " "))

# ---------------------------------------------------------------------------
# 1-D clustering of token midpoints into table columns
# ---------------------------------------------------------------------------
# Sort the midpoints and cut where the gap between neighbors exceeds
# `gapTol` points.  Column spacing in a journal table is typically well
# over 40 pt while jitter within a column (mean +/- SD vs a lone count)
# stays under ~20 pt.
.ppClusterColumns <- function(mids, gapTol = 25) {
  o <- order(mids)
  s <- mids[o]
  cl <- cumsum(c(1, diff(s) > gapTol))
  centers <- tapply(s, cl, mean)
  list(assign = function(x) {
         # nearest center
         vapply(x, function(v) which.min(abs(centers - v)), integer(1))
       },
       centers = as.numeric(centers),
       n = length(centers))
}
