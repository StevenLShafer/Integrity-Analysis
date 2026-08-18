# baselineTable.R - the journal-style reconstructed baseline table
# (ISSUES.md issue 15).
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-19,
# at Steve Shafer's request. The cell-per-line grid looks nothing like a
# manuscript's Table 1, so an editor cannot eyeball what IntegrityAnalysis
# thought the baseline data were. These functions rebuild, from the
# VALIDATED data, a per-trial table shaped the way journals print one -
# variables as rows, arms as columns, cells "mean (SD)",
# "median [Q1, Q3]", counts for categories - which is the artifact an
# editor compares against the manuscript page. Formatting uses the same
# rounding columns the analysis uses (ROUND_MEAN for the mean/median and
# quartiles, ROUND_DISPERSION for the SD when present), so the
# reconstruction shows the PRINTED precision the analysis assumed - if
# the reconstruction disagrees with the page, so did the analysis.
# Candidate for the API response too (docs/api-spec.md, open decision).

#' Format one number at the printed precision
#'
#' @param x numeric value (may be NA).
#' @param digits decimal places (NA is treated as 0).
#' @return character; "" for NA input.
#' @noRd
.fmtAt <- function(x, digits) {
  if (is.na(x)) return("")
  if (is.na(digits)) digits <- 0
  sprintf("%.*f", as.integer(digits), x)
}

#' Reconstruct journal-style baseline tables from validated data
#'
#' One data frame per trial. Within a trial, the lines sharing a ROW value
#' are that variable's arms, in the order they appear (the validated data
#' preserves within-variable line order); arm k of every variable is the
#' same study arm, which is how the template has always been organized.
#'
#' Layout choices, made for the editor's eyeball comparison:
#' - Column headers carry the arm N ("Arm 1 (n = 15)") taken as the most
#'   common N among that arm's continuous lines; a line whose N differs
#'   (dropouts, missing data) gets "; n = X" appended in its own cell.
#' - A continuous variable prints as "mean (SD)"; a median/IQR variable
#'   (Q1/Q3 filled) prints as "median [Q1, Q3]"; the label says which.
#' - A category variable becomes a header line ("Sex, n") followed by one
#'   indented line per category column, with counts.
#'
#' @param DATA the VALIDATED data frame (validateData()$DATA).
#' @param CategoryNames character vector of category column names
#'   (validateData()$CategoryNames), or NULL.
#' @return named list of data.frames, one per trial, in trial order.
#' @noRd
buildBaselineTables <- function(DATA, CategoryNames = NULL) {
  out <- list()
  for (trial in unique(DATA$TRIAL)) {
    d <- DATA[DATA$TRIAL == trial, , drop = FALSE]
    vars <- unique(d$ROW)
    groups <- lapply(vars, function(v) which(d$ROW == v))
    names(groups) <- vars
    maxArms <- max(vapply(groups, length, integer(1)))

    # Arm-level N for the headers: the most common N among the continuous
    # lines sitting at position k across variables. Ties break toward the
    # largest count first seen; no continuous line at k leaves the header
    # bare ("Arm k").
    headerN <- rep(NA_real_, maxArms)
    for (k in seq_len(maxArms)) {
      ns <- unlist(lapply(groups, function(g)
        if (length(g) >= k && !is.na(d$MEAN[g[k]])) d$N[g[k]] else NULL))
      ns <- ns[!is.na(ns)]
      if (length(ns) > 0) {
        tab <- table(ns)
        headerN[k] <- as.numeric(names(tab)[which.max(tab)])
      }
    }

    hasQ <- all(c("Q1", "Q3") %in% names(d))
    rows <- list()
    addRow <- function(label, cells) {
      length(cells) <- maxArms          # pad with NA
      cells[is.na(cells)] <- ""
      rows[[length(rows) + 1]] <<- c(label, cells)
    }

    for (v in vars) {
      g <- groups[[v]]
      isCat <- !is.null(CategoryNames) &&
        all(is.na(d$MEAN[g])) &&
        any(!is.na(d[g, CategoryNames, drop = FALSE]))
      if (isCat) {
        # header line, then one indented line per category column that
        # holds a count anywhere in this variable's arms
        addRow(paste0(v, ", n"), character(0))
        for (cn in CategoryNames) {
          counts <- d[[cn]][g]
          if (all(is.na(counts))) next
          addRow(paste0("    ", cn),
                 vapply(counts, .fmtAt, character(1), digits = 0))
        }
      } else {
        medVar <- hasQ && any(!is.na(d$Q1[g]) | !is.na(d$Q3[g]))
        label <- paste0(v, if (medVar) ", median [Q1, Q3]"
                           else ", mean (SD)")
        cells <- character(length(g))
        for (j in seq_along(g)) {
          i <- g[j]
          rm <- d$ROUND_MEAN[i]
          if (medVar) {
            cells[j] <- paste0(.fmtAt(d$MEAN[i], rm), " [",
                               .fmtAt(d$Q1[i], rm), ", ",
                               .fmtAt(d$Q3[i], rm), "]")
          } else {
            rd <- if ("ROUND_DISPERSION" %in% names(d) &&
                      !is.na(d$ROUND_DISPERSION[i])) d$ROUND_DISPERSION[i]
                  else rm
            cells[j] <- paste0(.fmtAt(d$MEAN[i], rm), " (",
                               .fmtAt(d$SD[i], rd), ")")
          }
          # a line whose N differs from the arm header's N says so
          if (!is.na(d$N[i]) && !is.na(headerN[j]) &&
              d$N[i] != headerN[j])
            cells[j] <- paste0(cells[j], "; n = ", .fmtAt(d$N[i], 0))
        }
        addRow(label, cells)
      }
    }

    tab <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    names(tab) <- c("Variable", vapply(seq_len(maxArms), function(k)
      paste0("Arm ", k,
             if (!is.na(headerN[k]))
               paste0(" (n = ", .fmtAt(headerN[k], 0), ")") else ""),
      character(1)))
    out[[as.character(trial)]] <- tab
  }
  out
}

#' Write the reconstructed baseline tables to an xlsx, one sheet per trial
#'
#' Sheet names come from the trial identifiers, sanitized for Excel's
#' rules (31-character limit, no []:*?/\\) and de-duplicated.
#'
#' @param tables the list returned by [buildBaselineTables()].
#' @param file path of the xlsx file to write.
#' @return invisibly, the sheet names used.
#' @noRd
writeBaselineTablesXlsx <- function(tables, file) {
  wb <- openxlsx::createWorkbook()
  headStyle <- openxlsx::createStyle(textDecoration = "bold",
                                     border = "bottom")
  used <- character(0)
  for (trial in names(tables)) {
    # class lists ] first and [ last so "[:" never appears (TRE would
    # read it as a POSIX class opener)
    nm <- gsub("[]:*?/\\\\[]", " ", trial)
    nm <- substr(trimws(nm), 1, 31)
    if (nm == "") nm <- "Trial"
    base <- substr(nm, 1, 28); k <- 1
    while (nm %in% used) { k <- k + 1; nm <- paste0(base, " ", k) }
    used <- c(used, nm)
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, tables[[trial]], headerStyle = headStyle)
    openxlsx::setColWidths(wb, nm,
                           cols = seq_len(ncol(tables[[trial]])),
                           widths = "auto")
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(used)
}
