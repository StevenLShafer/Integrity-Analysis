# armNRecovery.R - recover missing treatment-arm sizes deterministically.
#
############################################################################
# Provenance                                                               #
# Written 2026-08-21 by Claude Code (model: Claude Fable 5) at Steve       #
# Shafer's request, after running all 654 A&A RCT submissions through the  #
# deterministic engine and through the AI engine separately and comparing: #
# the single largest deterministic deficit was the arm N. 583 skipped      #
# n (%) rows across the corpus were blocked ONLY on an unknown arm N, and  #
# the arm-N sets agreed with the AI reading on just 255 of 453 co-parsed   #
# files - while the Ns were nearly always printed somewhere: in the very   #
# n (%) cells being skipped, in the randomization sentence of the Methods, #
# or in the CONSORT flow labels.                                           #
#                                                                          #
# Everything here is deterministic - no AI service is called. Every        #
# recovered N is traceable: either to printed cells of the table itself    #
# (the count and its percentage bracket the arm size), or to a quoted      #
# sentence of the document, which is recorded and surfaced by              #
# reviewFlags() so a human can verify it against the CONSORT diagram.      #
# Status: run and verified by tests/testthat/test-armn-recovery.R and a    #
# full re-run of the 654-submission corpus.                                #
############################################################################

# --------------------------------------------------------------------------
# Source 1: the table's own n (%) cells
# --------------------------------------------------------------------------
# A printed cell "13 (68.4%)" pins the arm size tightly: the true proportion
# lies within half a printed unit of 68.4%, so N = 13/p can only be an
# integer in [13/0.6845, 13/0.6835] = {19}. One cell with a one-decimal
# percentage is usually conclusive; several cells of the same arm intersect
# to a unique N even at zero decimals. This uses nothing but the printed
# table, so it is the most trustworthy recovery and is tried first.

# Feasible arm sizes for one printed "count (pct%)" cell. `dec` is the
# number of printed decimals of the percentage - it decides the bracket
# width, exactly as ROUND_MEAN does for the Monte Carlo.
.ppNFromCountPct <- function(count, pct, dec) {
  if (is.na(count) || is.na(pct) || count <= 0 || pct <= 0 || pct > 100)
    return(integer(0))
  if (is.na(dec)) dec <- 0L
  half <- 0.5 * 10^(-dec)
  lo <- (pct - half) / 100
  hi <- (pct + half) / 100
  if (lo <= 0) return(integer(0))
  nLo <- as.integer(ceiling(count / hi - 1e-9))
  nHi <- as.integer(floor(count / lo + 1e-9))
  if (nHi < nLo) return(integer(0))
  seq.int(max(nLo, count), nHi)
}

# Intersect the feasible sets of several cells belonging to one arm.
# Returns the arm size only when exactly ONE integer survives; anything
# looser stays NA rather than guessed - and a CONTRADICTION (a cell with
# valid inputs but no feasible N at all, or two cells with disjoint sets)
# refuses outright, because it means these are not really n (%) cells and
# nothing about this arm should be inferred from them.
.ppDeriveArmN <- function(counts, pcts, decs) {
  feas <- NULL
  for (i in seq_along(counts)) {
    valid <- !is.na(counts[i]) && !is.na(pcts[i]) &&
             counts[i] > 0 && pcts[i] > 0 && pcts[i] <= 100
    if (!valid) next                     # degenerate cell: no evidence
    s <- .ppNFromCountPct(counts[i], pcts[i], decs[i])
    if (length(s) == 0) return(NA_integer_)   # contradiction: refuse
    if (length(s) > 400) next            # too loose to narrow anything
    feas <- if (is.null(feas)) s else intersect(feas, s)
    if (length(feas) == 0) return(NA_integer_)  # disjoint cells: refuse
  }
  if (!is.null(feas) && length(feas) == 1) feas else NA_integer_
}

# The reverse bracket, for percent-block category tables (2026-08-21): given
# the arm size, which count was printed as `pct`%? The printed rounding
# brackets it - count/N must round to pct at `dec` decimals - and the count
# is accepted only when exactly ONE integer lies in the bracket. "47%" of
# n = 40 pins 19; "47%" of n = 702 spans 327..333 and is refused, because a
# fraud screen must not analyze approximated counts as if they were printed.
.ppCountFromPct <- function(pct, dec, N) {
  if (is.na(pct) || is.na(N) || N <= 0 || pct < 0 || pct > 100)
    return(NA_integer_)
  if (is.na(dec)) dec <- 0L
  half <- 0.5 * 10^(-dec)
  cLo <- as.integer(ceiling(N * (pct - half) / 100 - 1e-9))
  cHi <- as.integer(floor(N * (pct + half) / 100 + 1e-9))
  cLo <- max(cLo, 0L)
  cHi <- min(cHi, as.integer(N))
  if (cHi != cLo) return(NA_integer_)
  cLo
}

# --------------------------------------------------------------------------
# Source 2: the document text (CONSORT labels, randomization sentences)
# --------------------------------------------------------------------------
# Candidate (context, n) pairs from every "n = 137" in the document. The
# context decides whether the mention is an allocation ("allocated to the
# ketamine group (n = 24)") or something else entirely - above all a
# sample-size calculation ("n = 25 per group would provide 80% power"),
# which states a HYPOTHETICAL n that must never be taken for a real one.
.ppArmNCandidatesFromText <- function(txt) {
  empty <- data.frame(n = integer(0), context = character(0),
                      near = character(0), pos = integer(0),
                      stringsAsFactors = FALSE)
  if (!length(txt)) return(empty)
  j <- .ppSquish(paste(txt, collapse = " "))
  m <- gregexpr("(?i)\\bn\\s*=\\s*\\d[\\d,]*", j, perl = TRUE)[[1]]
  if (m[1] == -1) return(empty)
  lens <- attr(m, "match.length")
  out <- lapply(seq_along(m), function(k) {
    hit <- substr(j, m[k], m[k] + lens[k] - 1)
    n   <- suppressWarnings(as.integer(gsub("[^0-9]", "", hit)))
    # The near window must not reach back past the PREVIOUS mention:
    # "...ketamine group (n = 24) or the saline group (n = 26)" packs two
    # mentions into one sentence, and a window that crosses the first one
    # makes the second carry both arm names.
    nearFrom <- max(1, m[k] - 45,
                    if (k > 1) m[k - 1] + lens[k - 1] + 1 else 1)
    data.frame(n = n,
               # wide context: is this an allocation mention at all?
               context = substr(j, max(1, m[k] - 90), m[k] + lens[k] + 12),
               # near context: whose mention is it, and is it a sample-size
               # calculation? The label sits immediately left of "(n = X)".
               near = substr(j, nearFrom, m[k] + lens[k] + 12),
               pos = m[k], stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  out <- out[!is.na(out$n) & out$n > 0, , drop = FALSE]

  alloc <- grepl(paste0("(?i)allocat|assign|randomi[sz]|\\bgroup\\b|",
                        "\\barm\\b|receiv|analy[sz]ed|completed|enrol"),
                 out$context, perl = TRUE)
  # A sample-size calculation states a HYPOTHETICAL n ("n = 25 per group
  # would provide 80% power") that must never be taken for a real arm
  # size. Judged on the NEAR window: the calculation vocabulary sits right
  # beside its n, while a power sentence two lines earlier is no reason to
  # discard a genuine allocation mention.
  power <- grepl(paste0("(?i)power|sample\\s+size|per\\s+group|required|",
                        "detect|dropout|attrition|calculat|hypothes"),
                 out$near, perl = TRUE)
  out[alloc & !power, , drop = FALSE]
}

# A stated randomized/enrolled total, used to confirm a positional
# assignment. Returns every such total found (papers restate it).
.ppRandomizedTotals <- function(txt) {
  j <- .ppSquish(paste(txt, collapse = " "))
  pats <- c(paste0("(?i)(\\d[\\d,]*)\\s+(patients|subjects|participants|",
                   "children|women|men|volunteers|adults)?\\s*(were\\s+)?",
                   "(randomi[sz]ed|enrolled|recruited|included)"),
            "(?i)randomi[sz]ed\\s+(\\d[\\d,]*)",
            "(?i)total\\s+of\\s+(\\d[\\d,]*)")
  tot <- integer(0)
  for (p in pats) {
    m <- gregexpr(p, j, perl = TRUE)[[1]]
    if (m[1] == -1) next
    lens <- attr(m, "match.length")
    for (k in seq_along(m)) {
      hit    <- substr(j, m[k], m[k] + lens[k] - 1)
      digits <- regmatches(hit, gregexpr("[0-9][0-9,]*", hit))[[1]][1]
      v <- suppressWarnings(as.integer(gsub(",", "", digits)))
      if (!is.na(v)) tot <- c(tot, v)
    }
  }
  unique(tot)
}

# Fill the missing entries of `armN` from the text candidates.
#
# Assignment ladder, strictest first:
#   1. NAME MATCH - a candidate whose context contains a distinctive word
#      of the arm's name. All matching candidates must agree on one n.
#   2. ELIMINATION - every arm but one was name-matched and exactly one
#      distinct candidate value is left over.
#   3. POSITION, CONFIRMED BY THE TOTAL - no names matched at all, but the
#      number of allocation candidates equals the number of arms AND their
#      sum equals a stated randomized total. Document order is taken as
#      column order; that assumption is why the total confirmation is
#      mandatory, why any already-known arm N must agree with its
#      positional candidate, and why reviewFlags() reports the sentence.
#
# Returns list(N = the completed vector, source = per-arm character).
.ppFillArmNFromText <- function(armN, armName, cand, totals) {
  source <- rep(NA_character_, length(armN))
  if (nrow(cand) == 0 || !any(is.na(armN)))
    return(list(N = armN, source = source))
  snip <- function(ctx) paste0("\"...", .ppSquish(substr(ctx, 1, 100)), "...\"")

  stop_words <- c("group", "groups", "arm", "arms", "the", "and", "with",
                  "patients", "study", "control")
  armWords <- lapply(armName, function(nm) {
    if (is.na(nm)) return(character(0))
    w <- tolower(unlist(strsplit(gsub("[^A-Za-z ]", " ", nm), "\\s+")))
    setdiff(w[nchar(w) >= 3], stop_words)
  })

  used <- rep(FALSE, nrow(cand))
  # 1. name match - against the NEAR window only, because the arm's label
  # sits immediately left of its "(n = X)", while a 90-character context
  # regularly spans the other arm's mention too ("...the ketamine group
  # (n = 24) or the saline group (n = 26)").
  for (k in which(is.na(armN))) {
    if (length(armWords[[k]]) == 0) next
    hits <- which(vapply(cand$near, function(ctx) {
      lc <- tolower(ctx)
      any(vapply(armWords[[k]], function(w)
        grepl(paste0("\\b", w), lc, perl = TRUE), logical(1)))
    }, logical(1)))
    if (length(hits) == 0) next
    ns <- unique(cand$n[hits])
    if (length(ns) == 1) {
      armN[k]  <- ns
      source[k] <- paste0("document text (arm name matched): ",
                          snip(cand$near[hits[1]]))
      used[hits] <- TRUE
    }
  }
  # 2. elimination - requires the leftover to actually correspond: exactly
  # one open arm AND exactly one unused mention. Two unused mentions of
  # "n = 20" over one open arm means the mentions belong to the two arms
  # already known, not to the leftover cluster (measured failure mode).
  open <- which(is.na(armN))
  if (length(open) == 1 && sum(!used) == 1) {
    armN[open] <- cand$n[!used]
    source[open] <- paste0("document text (only unassigned mention): ",
                           snip(cand$context[which(!used)[1]]))
  }
  # 3. position, confirmed by the stated total
  open <- which(is.na(armN))
  if (length(open) > 0 && !any(!is.na(source))) {
    firstMention <- cand[!duplicated(cand$pos), , drop = FALSE]
    if (nrow(firstMention) == length(armN) &&
        length(totals) > 0 && sum(firstMention$n) %in% totals) {
      agree <- TRUE
      for (k in seq_along(armN))
        if (!is.na(armN[k]) && armN[k] != firstMention$n[k]) agree <- FALSE
      if (agree) {
        for (k in open) {
          armN[k]  <- firstMention$n[k]
          source[k] <- paste0("document text (by position; the ",
                              sum(firstMention$n),
                              " total confirms the set, not the order): ",
                              snip(firstMention$context[k]))
        }
      }
    }
  }
  list(N = armN, source = source)
}
