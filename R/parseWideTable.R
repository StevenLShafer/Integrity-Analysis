# parseWideTable.R - read a journal-style WIDE baseline table as input
# (ISSUES.md issue 17).
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-21,
# at Steve Shafer's request; reviewed against R/baselineTable.R (the
# generator this file inverts) and R/parseBaselineTableHeuristics.R (the
# PDF engine whose cell semantics it mirrors). Verified by the round-trip
# suite in tests/testthat/test-wide-table.R.
#
# The app's long "template" format (one line per variable per arm, columns
# N / MEAN / SD ...) is nobody's native habitat: editors have the
# manuscript's Table 1, and this app itself GENERATES a journal-style
# reconstruction of it (buildBaselineTables(), the Editor's View
# download). Steve's design for this feature: that generated table must
# be valid INPUT - download the Editor's View, feed it back, and the
# analysis reproduces. So the acceptance test is a round trip, pinned in
# test-wide-table.R, and R/baselineTable.R is the format's canonical
# specification. The parser is deliberately more tolerant than the
# generator, though (Steve, 2026-08-21): arbitrary real-world arm headers
# ("Control (n=50)", "Treatment") and untagged labels also parse, using
# the same cell-content rules as the PDF engine.
#
# What this file does NOT do:
#   - It never guesses between an IQR and a min-max range. A bracketed
#     interval is emitted as Q1/Q3 only when the row label SAYS it is an
#     IQR ("median [Q1, Q3]", "median (IQR)"); a label saying "range", or
#     saying nothing, sends the row to $skipped with a reason. Both
#     interval kinds straddle the median, so text is the only evidence -
#     and a wrong guess here would feed a fraud-screening verdict.
#   - Fractions ("15/10") and percent-only cells are skipped with
#     reasons; the full machinery for those lives in the PDF engine and
#     was not duplicated for v1.
#   - Mean and SD in SEPARATE columns is the template format, not this
#     one; such sheets are vetoed by .wideHeaderRow and flow to
#     validateData() unchanged.
#
# Rounding note: a spreadsheet cell that Excel typed as a NUMBER prints
# "12.1" even if the author formatted it "12.10", so printed-precision
# recovery is exact only for TEXT cells. Everything this app generates is
# written as text, so the round trip is unaffected.

# The template's own column names. A sheet whose header row contains two
# or more of these as whole cells is the LONG template format (or the
# results workbook's Test Results sheet) and must NOT be read as a wide
# table - validateData()'s substring grep is the right reader for it.
.wideTemplateNames <- c("TRIAL", "ROW", "N", "MEAN", "SD", "SE")

# Read every sheet of `path` as a matrix of raw cell TEXT, untyped and
# headerless, named by sheet. Raw text matters twice: openxlsx's
# colNames = TRUE mangles header text ("Arm 1 (n = 20)" becomes
# "Arm.1.(n.=.20)"), and the stacked results-workbook layout carries
# "Trial: <id>" marker rows that are data here, not headers.
.wideRawCells <- function(path, ext) {
  toMat <- function(d) {
    if (is.null(d) || nrow(d) == 0 || ncol(d) == 0)
      return(matrix(character(0), nrow = 0, ncol = 0))
    m <- vapply(seq_len(ncol(d)), function(j) {
      x <- d[[j]]
      # keep numbers readable, not "45.299999999999997"
      if (is.numeric(x)) ifelse(is.na(x), "", format(x, trim = TRUE,
                                                     scientific = FALSE))
      else ifelse(is.na(x), "", as.character(x))
    }, character(nrow(d)))
    matrix(m, nrow = nrow(d))
  }
  if (ext == "csv") {
    d <- utils::read.csv(path, header = FALSE, colClasses = "character",
                         check.names = FALSE)
    return(list(toMat(d)))
  }
  if (ext == "xlsx") {
    sheets <- openxlsx::getSheetNames(path)
    out <- lapply(sheets, function(s)
      toMat(tryCatch(openxlsx::read.xlsx(path, sheet = s, colNames = FALSE,
                                         skipEmptyRows = FALSE,
                                         skipEmptyCols = FALSE),
                     error = function(e) NULL)))
    names(out) <- sheets
    return(out)
  }
  # .xls via readxl
  sheets <- readxl::excel_sheets(path)
  out <- lapply(sheets, function(s)
    toMat(as.data.frame(readxl::read_excel(path, sheet = s,
                                           col_names = FALSE,
                                           col_types = "text"))))
  names(out) <- sheets
  out
}

# Find the wide table's header row in `cells` (a character matrix), or NA.
# Conservative on purpose: anything not confidently wide falls through to
# the app's existing spreadsheet path, whose failure mode (raw grid plus
# "Missing column labeled ..." comments) is the long-standing behavior.
#
# A row is the header when it has at least two non-empty cells past the
# label column AND either
#   (a) one of them carries an arm size "(n = 15)"  - the app's own
#       format always does - or
#   (b) the label cell is blank or a label word ("Variable",
#       "Characteristic", ...), every body cell is non-numeric (arm NAMES,
#       not data), and at least two of the following rows look like data
#       (a labelled row whose cells hold "mean (SD)" / "a ± b" /
#       "median [a, b]" values).
# The template veto above trumps everything.
.wideHeaderRow <- function(cells) {
  if (nrow(cells) == 0 || ncol(cells) < 3) return(NA_integer_)
  valuePat <- paste0("^[<>]?-?\\d[\\d.,·]*\\s*",
                     "(\\(\\s*-?\\d[\\d.,·]*\\s*%?\\s*\\)",
                     "|±\\s*\\d[\\d.,·]*",
                     "|\\[[^]]+\\])")
  for (r in seq_len(min(10L, nrow(cells)))) {
    row  <- cells[r, ]
    up   <- toupper(trimws(row))
    if (sum(up %in% .wideTemplateNames) >= 2) return(NA_integer_)  # veto
    body <- trimws(row[-1])
    body <- body[nzchar(body)]
    if (length(body) < 2) next
    if (any(grepl("(?i)\\(\\s*n\\s*=\\s*\\d", body, perl = TRUE)))
      return(r)
    labelish <- !nzchar(trimws(row[1])) ||
      grepl("(?i)^(variable|characteristic|parameter|outcome|item)s?$",
            trimws(row[1]), perl = TRUE)
    if (labelish && !any(grepl("^[<>]?-?[\\d.,·]+$", body))) {
      below <- seq(r + 1L, length.out = min(15L, nrow(cells) - r))
      evidence <- sum(vapply(below, function(rr)
        nzchar(trimws(cells[rr, 1])) &&
          any(grepl(valuePat, trimws(cells[rr, -1]), perl = TRUE)),
        logical(1)))
      if (evidence >= 2) return(r)
    }
  }
  NA_integer_
}

# --- label tags: the generator writes the row's statistical type into its
# label ("Age, mean (SD)"); real-world tables often do the same. Each tag
# regex both DETECTS the type and is stripped to recover the ROW label.
# Labels are otherwise kept verbatim (no .ppCleanLabel): unlike a PDF text
# line, a spreadsheet label is deliberate, and stripping units ("(kg)")
# would break the round trip against the validated frame it came from.
.wideTagMeanSD  <- "(?i)[,;]?\\s*mean\\s*(\\(\\s*sd\\s*\\)|±\\s*sd)\\s*$"
.wideTagMedIQR  <- paste0("(?i)[,;]?\\s*median\\s*[\\[(]\\s*",
                          "(q1\\s*[,;]?\\s*q3|iqr|interquartile[^\\])]*",
                          "|25th[^\\])]*)\\s*[\\])]\\s*$")
.wideTagMedRng  <- "(?i)[,;]?\\s*median\\s*[\\[(][^\\])]*range[^\\])]*[\\])]\\s*$"
.wideTagCat     <- "(?i)[,;]\\s*(no\\.?|n)(\\s*\\(\\s*%\\s*\\))?\\s*$"

# One number, as the engine prints it (comma/middle-dot decimals,
# thousands separators, stray < >) - kept in sync with tokenize.R's .ppNUM.
.wideNUM <- "[<>]?-?\\d+(?:[.,·]\\d+)*"

# "median [Q1, Q3]" as a whole cell: three numbers, the outer two
# bracketed and separated by a comma/semicolon/dash/"to". The engine's
# medianRng token cannot see the comma-separated form (its separators are
# dashes and "to"), which is why this file carries its own pattern.
.wideMedianPat <- paste0("^(", .wideNUM, ")\\s*[\\[(]\\s*(", .wideNUM,
                         ")\\s*(?:[,;]|–|—|−|-|to)\\s*(",
                         .wideNUM, ")\\s*[\\])]$")

# Parse one trial block. `cells` is the block's matrix (header row
# included), `hdr` the header row index within it, `trial` the trial id
# (NA when the caller does not know - the server substitutes the file
# stem). Returns list(trial, data, arms, skipped), or NULL when no line
# parsed.
.wideParseBlock <- function(cells, hdr, trial) {
  header  <- cells[hdr, ]
  armCols <- which(vapply(seq_len(ncol(cells))[-1], function(j)
    nzchar(trimws(header[j])) ||
      any(nzchar(trimws(cells[-seq_len(hdr), j]))), logical(1))) + 1L
  if (length(armCols) == 0) return(NULL)
  nArms <- length(armCols)

  # Arm N from "(n = 15)" in the header; the remainder is the arm's name.
  # Bare names ("Treatment") leave N as NA - validation paints the gap.
  armN    <- rep(NA_real_, nArms)
  armName <- character(nArms)
  for (k in seq_len(nArms)) {
    h <- trimws(header[armCols[k]])
    m <- regmatches(h, regexec("(?i)n\\s*=\\s*(\\d[\\d,]*)", h,
                               perl = TRUE))[[1]]
    if (length(m) == 2) {
      armN[k] <- .ppAsNumeric(m[2])
      h <- trimws(sub("(?i)[,;]?\\s*\\(?\\s*n\\s*=\\s*\\d[\\d,]*\\s*\\)?",
                      "", h, perl = TRUE))
    }
    armName[k] <- if (nzchar(h)) h else paste("Arm", k)
  }

  outRows      <- list()   # same shape as the engine's: row / type / perArm
  skipped      <- list()
  usedRowNames <- character(0)
  catColumns   <- character(0)
  anyMedian    <- FALSE

  addSkip <- function(label, reason, txt)
    skipped[[length(skipped) + 1]] <<-
      data.frame(label = label, reason = reason, text = txt,
                 stringsAsFactors = FALSE)

  # A category variable accumulates over consecutive count rows (the
  # generator writes a "Sex, n" header then one indented row per
  # category); it flushes when anything else appears.
  catHeader <- NA_character_
  catAccum  <- NULL   # list(row = <ROW label>, perArm = list of named lists)
  flushCat <- function() {
    if (!is.null(catAccum)) outRows[[length(outRows) + 1]] <<- catAccum
    catAccum  <<- NULL
    catHeader <<- NA_character_
  }
  addCount <- function(colName, counts) {
    # counts: integer vector over arms, NA where the cell was empty
    if (is.null(catAccum)) {
      rowName <- .ppUniqueName(catHeader, usedRowNames)
      usedRowNames <<- c(usedRowNames, rowName)
      catAccum <<- list(row = rowName, type = "category",
                        perArm = vector("list", nArms))
    }
    for (j in seq_len(nArms))
      if (!is.na(counts[j]))
        catAccum$perArm[[j]] <<- c(catAccum$perArm[[j]],
                                   stats::setNames(list(counts[j]), colName))
  }

  # Classify one cell (already stripped of its "; n = X" suffix).
  # medianTriple is checked first - the engine's tokenizer cannot match
  # the comma-separated form; everything else defers to .ppTokenizeLine
  # so the cell rules stay identical to the PDF engine's.
  classify <- function(txt) {
    m <- regmatches(txt, regexec(.wideMedianPat, txt, perl = TRUE))[[1]]
    if (length(m) == 4)
      return(list(type = "medianTriple",
                  num1 = .ppAsNumeric(m[2]), num2 = .ppAsNumeric(m[3]),
                  num3 = .ppAsNumeric(m[4]), dec1 = .ppDecimals(m[2])))
    t <- .ppTokenizeLine(data.frame(text = txt, x = 0, width = nchar(txt),
                                    stringsAsFactors = FALSE))
    if (nrow(t) == 0) return(NULL)
    as.list(t[1, ])
  }

  dataRows <- seq(hdr + 1L, length.out = nrow(cells) - hdr)
  for (r in dataRows) {
    rawLabel <- cells[r, 1]
    bodyTxt  <- cells[r, armCols]
    if (!nzchar(trimws(rawLabel)) && !any(nzchar(trimws(bodyTxt)))) {
      flushCat()
      next
    }
    indent <- grepl("^\\s{2,}", rawLabel)
    label  <- .ppSquish(rawLabel)

    # -- label tags ----------------------------------------------------
    tag <- if (grepl(.wideTagMeanSD, label, perl = TRUE))      "meanSD"
           else if (grepl(.wideTagMedIQR, label, perl = TRUE)) "medIQR"
           else if (grepl(.wideTagMedRng, label, perl = TRUE)) "medRng"
           else if (grepl(.wideTagCat, label, perl = TRUE))    "cat"
           else NA_character_
    if (!is.na(tag))
      label <- .ppSquish(sub(switch(tag, meanSD = .wideTagMeanSD,
                                    medIQR = .wideTagMedIQR,
                                    medRng = .wideTagMedRng,
                                    cat = .wideTagCat),
                             "", label, perl = TRUE))

    # -- header line of a category variable ("Sex, n" - no cells) ------
    if (!any(nzchar(trimws(bodyTxt)))) {
      flushCat()
      catHeader <- if (nzchar(label)) label else "Category"
      next
    }

    # -- per-cell N override "45.3 (12.1); n = 14" ---------------------
    lineN <- rep(NA_real_, nArms)
    cellTxt <- trimws(bodyTxt)
    for (j in seq_len(nArms)) {
      m <- regmatches(cellTxt[j],
                      regexec("[,;]\\s*n\\s*=\\s*(\\d[\\d,]*)\\s*$",
                              cellTxt[j], perl = TRUE))[[1]]
      if (length(m) == 2) {
        lineN[j] <- .ppAsNumeric(m[2])
        cellTxt[j] <- trimws(sub("[,;]\\s*n\\s*=\\s*\\d[\\d,]*\\s*$", "",
                                 cellTxt[j], perl = TRUE))
      }
    }

    toks <- lapply(cellTxt, function(x)
      if (nzchar(x)) classify(x) else NULL)
    types <- vapply(toks, function(t)
      if (is.null(t)) NA_character_ else t$type, character(1))
    present <- !is.na(types)
    if (!any(present)) {
      addSkip(if (nzchar(label)) label else paste(cellTxt, collapse = " "),
              "cells not in a recognized format - enter by hand",
              paste(cells[r, ], collapse = " | "))
      next
    }
    mainType <- names(sort(table(types), decreasing = TRUE))[1]

    # -- category count rows -------------------------------------------
    # The generator indents them under the header; real-world sheets
    # often do not, so a run of bare integers under an active category
    # header also counts.
    if (mainType == "plain" && (indent || !is.na(catHeader))) {
      if (is.na(catHeader)) catHeader <- "Category"
      counts <- vapply(seq_len(nArms), function(j) {
        t <- toks[[j]]
        if (is.null(t) || t$type != "plain") NA_integer_
        else as.integer(t$num1)
      }, integer(1))
      colName <- .ppUniqueName(if (nzchar(label)) label else "Category",
                               catColumns)
      catColumns <- unique(c(catColumns, colName))
      addCount(colName, counts)
      next
    }
    flushCat()

    # -- an "N of patients" row supplies missing arm Ns ----------------
    if (mainType == "plain" &&
        grepl("(?i)^(no\\.?|n|number)\\b.*(patient|subject|participant|randomi)|^n$",
              label, perl = TRUE)) {
      for (j in seq_len(nArms))
        if (!is.null(toks[[j]]) && toks[[j]]$type == "plain")
          armN[j] <- as.numeric(toks[[j]]$num1)
      next
    }

    txt <- paste(cells[r, ], collapse = " | ")

    # -- median rows ----------------------------------------------------
    if (mainType == "medianTriple" || tag %in% c("medIQR", "medRng")) {
      # The interval's meaning comes from the LABEL, never from the
      # numbers: an IQR and a range both straddle the median. Same
      # conservative gate as the engine (issue 18): explicit IQR -> emit
      # Q1/Q3; explicit range -> skip; unlabeled -> skip. Feeding a range
      # into the metalog null would be a correctness bug in a
      # fraud-screening verdict, so ambiguity always loses.
      saysIQR <- identical(tag, "medIQR") ||
        grepl("(?i)iqr|interquartile|quartile|\\bq1\\b|25th", rawLabel,
              perl = TRUE)
      saysRng <- identical(tag, "medRng") ||
        grepl("(?i)\\brange\\b|min\\s*[-–]?\\s*max|minimum", rawLabel,
              perl = TRUE)
      if (saysRng) {
        addSkip(label, paste("median [range] - the analysis needs",
                             "quartiles (Q1/Q3), not the range"), txt)
        next
      }
      if (!saysIQR) {
        addSkip(label, paste("median with an unlabeled interval - if it",
                             "is an IQR, enter median/Q1/Q3 by hand"), txt)
        next
      }
      bad <- vapply(toks, function(t)
        !is.null(t) && t$type == "medianTriple" &&
          (t$num2 > t$num1 || t$num3 < t$num1), logical(1))
      if (any(bad)) {
        addSkip(label, "median outside its own [Q1, Q3] - check the cells",
                txt)
        next
      }
      rowName <- .ppUniqueName(if (nzchar(label)) label else "Unnamed",
                               usedRowNames)
      usedRowNames <- c(usedRowNames, rowName)
      anyMedian <- TRUE
      perArm <- lapply(seq_len(nArms), function(j) {
        t <- toks[[j]]
        if (is.null(t) || t$type != "medianTriple") return(NULL)
        list(N = if (!is.na(lineN[j])) lineN[j] else armN[j],
             MEAN = t$num1, Q1 = t$num2, Q3 = t$num3,
             SD = NA_real_, SE = NA_real_,
             ROUND_MEAN = t$dec1, ROUND_DISPERSION = NA_integer_,
             ROUND_OBSERVATION = t$dec1)
      })
      outRows[[length(outRows) + 1]] <-
        list(row = rowName, type = "median", perArm = perArm)
      next
    }

    # -- "a (b)": SD or percent? ---------------------------------------
    # The engine's decision ladder (parseBaselineTableHeuristics.R,
    # "numParen"), minus the table-level footnote evidence a lone
    # spreadsheet does not carry: an explicit tag or a continuous-sounding
    # label reads SD; a percent-sounding label reads n (%); the default is
    # SD, the overwhelming convention.
    if (mainType == "numParen") {
      labelSaysPct <- grepl("(?i)\\(%\\)|percent", rawLabel, perl = TRUE)
      mainType <- if (identical(tag, "meanSD")) "meanSD"
                  else if (labelSaysPct || identical(tag, "cat")) "nPct"
                  else "meanSD"
    }

    if (mainType == "meanSD") {
      rowName <- .ppUniqueName(if (nzchar(label)) label else "Unnamed",
                               usedRowNames)
      usedRowNames <- c(usedRowNames, rowName)
      # "Age, mean (SEM)" in the label files the value as SE, mirroring
      # the engine's row-level override; there is no footnote here to
      # consult, so the label is the only evidence.
      isSE <- grepl("(?i)\\bs\\.?e\\.?m?\\.?\\b|standard\\s+error",
                    rawLabel, perl = TRUE) &&
              !grepl("(?i)\\bs\\.?d\\.?\\b|standard\\s+deviation",
                     rawLabel, perl = TRUE)
      perArm <- lapply(seq_len(nArms), function(j) {
        t <- toks[[j]]
        if (is.null(t) || !t$type %in% c("meanSD", "numParen")) return(NULL)
        list(N = if (!is.na(lineN[j])) lineN[j] else armN[j],
             MEAN = t$num1,
             SD = if (isSE) NA_real_ else t$num2,
             SE = if (isSE) t$num2 else NA_real_,
             ROUND_MEAN = t$dec1,
             ROUND_DISPERSION = t$dec2,
             # ROUND_OBSERVATION is not printed in a wide table, and
             # validateData() defaults it to ROUND_MEAN - matching that
             # default (NOT the PDF engine's ROUND_MEAN + 1) is what lets
             # the round trip close exactly.
             ROUND_OBSERVATION = t$dec1)
      })
      outRows[[length(outRows) + 1]] <-
        list(row = rowName, type = "continuous", perArm = perArm)
      next
    }

    if (mainType == "nPct") {
      # Binary n (%) row: count column plus its complement when the arm N
      # is known - mirroring the engine's nPct branch, including the skip
      # when no N makes the complement uncomputable.
      catName <- .ppUniqueName(if (nzchar(label)) label else "Category",
                               catColumns)
      complementName <- .ppUniqueName(paste("Not", catName),
                                      c(catColumns, catName))
      effN <- ifelse(is.na(lineN), armN, lineN)
      haveN <- all(!is.na(effN[!vapply(toks, is.null, logical(1))]))
      catColumns <- unique(c(catColumns, catName,
                             if (haveN) complementName))
      if (!haveN) {
        addSkip(label, paste("n (%) with unknown arm N - complement",
                             "category cannot be computed; edit by hand"),
                txt)
        next
      }
      rowName <- .ppUniqueName(catName, usedRowNames)
      usedRowNames <- c(usedRowNames, rowName)
      perArm <- lapply(seq_len(nArms), function(j) {
        t <- toks[[j]]
        if (is.null(t) || !t$type %in% c("nPct", "numParen")) return(NULL)
        cnt <- as.integer(t$num1)
        stats::setNames(list(cnt, as.integer(effN[j] - cnt)),
                        c(catName, complementName))
      })
      outRows[[length(outRows) + 1]] <-
        list(row = rowName, type = "category", perArm = perArm)
      next
    }

    if (mainType == "fraction") {
      addSkip(label, paste("fraction cell (a/b) - enter the counts as",
                           "category columns by hand"), txt)
      next
    }
    if (mainType == "pctOnly") {
      addSkip(label, "percent only, no count - enter by hand", txt)
      next
    }
    addSkip(if (nzchar(label)) label else "Unnamed",
            "bare numbers - cannot tell counts from means; enter by hand",
            txt)
  }
  flushCat()

  if (length(outRows) == 0) return(NULL)

  # ---- Assemble the template-format frame (engine layout) ------------
  # One line per variable per arm; arm identity is positional, so an
  # interior empty cell becomes an all-NA line (holding the position, and
  # painting yellow in the grid) while TRAILING empties - the generator's
  # padding for a variable with fewer arms - produce no line at all.
  allCols <- c(.ppBaseColumns(), if (anyMedian) c("Q1", "Q3"), catColumns)
  rows <- list()
  for (rr in outRows) {
    filled <- which(!vapply(rr$perArm, is.null, logical(1)))
    if (length(filled) == 0) next
    for (j in seq_len(max(filled))) {
      v <- rr$perArm[[j]]
      line <- stats::setNames(as.list(rep(NA, length(allCols))), allCols)
      line$TRIAL <- trial
      line$ROW   <- rr$row
      if (!is.null(v)) {
        if (rr$type %in% c("continuous", "median")) {
          for (nm in intersect(names(v), allCols)) line[[nm]] <- v[[nm]]
        } else {
          for (nm in names(v)) line[[nm]] <- v[[nm]]
        }
      }
      rows[[length(rows) + 1]] <- line
    }
  }
  DATA <- do.call(rbind, lapply(rows, function(l)
    as.data.frame(l, check.names = FALSE, stringsAsFactors = FALSE)))

  list(trial   = trial,
       data    = DATA,
       arms    = data.frame(arm = armName, N = armN,
                            stringsAsFactors = FALSE),
       skipped = if (length(skipped) > 0) do.call(rbind, skipped) else
         data.frame(label = character(0), reason = character(0),
                    text = character(0)))
}

#' Read a journal-style wide baseline table as input
#'
#' Detects and parses spreadsheets laid out the way journals print a
#' baseline table - variables as rows, arms as columns, cells like
#' "45.3 (12.1)" - including the Editor's View workbook this app itself
#' generates (both its shapes: one sheet per trial, and the results
#' workbook's stacked "Trial: ..." blocks). Returns NULL when nothing in
#' the file looks like a wide table, so the caller can fall back to the
#' ordinary template reader.
#'
#' @param path the spreadsheet file.
#' @param ext lower-case extension ("csv", "xlsx", "xls").
#' @return NULL, or a list of blocks, each
#'   `list(trial, data, arms, skipped)`: `trial` may be NA (caller names
#'   it), `data` is a template-format frame, `skipped` names each
#'   unusable row and why (the app turns these into red grid rows).
#' @noRd
parseWideTable <- function(path, ext) {
  sheetList <- tryCatch(.wideRawCells(path, ext), error = function(e) NULL)
  if (is.null(sheetList)) return(NULL)
  blocks <- list()
  for (s in seq_along(sheetList)) {
    cells <- sheetList[[s]]
    if (nrow(cells) == 0) next
    sheetName <- names(sheetList)[s]
    markers <- which(grepl("^Trial:\\s*\\S", cells[, 1]))
    if (length(markers) > 0) {
      # the stacked results-workbook shape: each "Trial: <id>" row opens
      # a block; ids here are exact (no sheet-name truncation), which is
      # why this shape round-trips long trial names faithfully
      ends <- c(markers[-1] - 1L, nrow(cells))
      for (b in seq_along(markers)) {
        if (ends[b] <= markers[b]) next   # marker with nothing under it
        sub <- cells[seq(markers[b] + 1L, ends[b]), , drop = FALSE]
        hdr <- .wideHeaderRow(sub)
        if (is.na(hdr)) next
        blk <- .wideParseBlock(sub, hdr,
                               sub("^Trial:\\s*", "", cells[markers[b], 1]))
        if (!is.null(blk)) blocks[[length(blocks) + 1]] <- blk
      }
    } else {
      hdr <- .wideHeaderRow(cells)
      if (is.na(hdr)) next
      # sheet-per-trial shape: the sheet name IS the trial id (as
      # writeBaselineTablesXlsx names them), except Excel's meaningless
      # defaults ("Sheet1"), which the caller replaces with the file stem
      trial <- if (is.null(sheetName) || !nzchar(sheetName) ||
                   grepl("(?i)^sheet ?\\d*$", sheetName))
        NA_character_ else sheetName
      blk <- .wideParseBlock(cells, hdr, trial)
      if (!is.null(blk)) blocks[[length(blocks) + 1]] <- blk
    }
  }
  if (length(blocks) == 0) NULL else blocks
}
