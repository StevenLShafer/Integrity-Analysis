# writeIntegrityTemplate.R - write a parsed table in the layout the
# Integrity-Analysis Shiny app reads.
#
############################################################################
# Provenance                                                               #
# Written 2026-08-15 by Claude Code (model: Claude Opus 5, Anthropic) at   #
# Steve Shafer's request. The column contract it targets comes from        #
# Template.xlsx / Example.xlsx and server.R in the Integrity-Analysis      #
# repository; the spreadsheet-writing call was factored out of             #
# parseCovariateTable() during the port.                                   #
# Status: run and verified by tests/testthat/test-write-template.R,        #
# which round-trips the file with openxlsx and checks the column contract. #
############################################################################

#' Write a parsed table as an Integrity-Analysis input spreadsheet
#'
#' Writes `x$data` to the first worksheet in the layout the IntegrityAnalysis
#' app expects (the full `.ppBaseColumns()` contract): `TRIAL`, `ROW`, `N`,
#' `MEAN`, `SD`, `SE`, `ROUND_MEAN`, `ROUND_DISPERSION`,
#' `ROUND_OBSERVATION`, then one integer column per category. `SD` and `SE`
#' are separate columns and are never converted into each other; a row
#' carries whichever the paper printed, and `ROUND_DISPERSION` records that
#' value's printed precision. Continuous rows carry N / MEAN / SD-or-SE and
#' the rounding columns; categorical rows leave those blank and carry counts
#' in the category columns, which is what the app's own validation requires.
#'
#' Two further worksheets are added by default. They sit after the data, so
#' the app - which reads the first worksheet - is unaffected, but the
#' provenance travels with the file:
#'
#' * **Provenance** - one line per data row, recording whether it came from
#'   the deterministic engine or from the model.
#' * **Skipped** - the table lines the parser could not use, and why.
#'
#' @param x A `ParsePDFTable` from [parseBaselineTable()].
#' @param file Path to the `.xlsx` file to write.
#' @param extraSheets Write the Provenance and Skipped worksheets. Set to
#'   `FALSE` for a single-worksheet file.
#' @param overwrite Overwrite `file` if it already exists.
#'
#' @return `file`, invisibly.
#' @seealso [parseBaselineTable()]
#' @export
writeIntegrityTemplate <- function(x, file, extraSheets = TRUE,
                                   overwrite = TRUE) {
  stopifnot(inherits(x, "ParsePDFTable"))
  if (!requireNamespace("openxlsx", quietly = TRUE))
    stop("Package 'openxlsx' is required: install.packages('openxlsx')")

  missingCols <- setdiff(.ppBaseColumns(), names(x$data))
  if (length(missingCols) > 0)
    stop("The parsed table is missing required column(s): ",
         paste(missingCols, collapse = ", "))

  sheets <- list(Template = x$data)
  if (extraSheets) {
    prov <- x$provenance
    # Whether the dispersion column holds a standard deviation or a standard
    # error - and whether the table actually said so - travels with the file.
    # It belongs here rather than in a data column: server.R treats any
    # unrecognised numeric column with an NA in it as a category.
    if (!is.null(x$dispersion) && nrow(prov))
      prov$DISPERSION <- x$dispersion
    sheets$Provenance <- prov
    sheets$Skipped    <- x$skipped
  }
  openxlsx::write.xlsx(sheets, file = file, keepNA = FALSE,
                       overwrite = overwrite)
  invisible(file)
}
