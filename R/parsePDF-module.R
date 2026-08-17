# parsePDF-module.R — overview documentation for the PDF-parsing half of
# this package.
#
# PROVENANCE: this file and the eight parser files (aiFallback.R,
# pageLayout.R, parseBaselineTable.R, parseBaselineTableFiles.R,
# parseBaselineTableHeuristics.R, tokenize.R, utils.R,
# writeIntegrityTemplate.R) were folded in verbatim from the ParsePDF
# package (github.com/StevenLShafer/ParsePDF, its temporary home) on
# 2026-08-17 - ISSUES.md issue 9. Every internal function is prefixed
# .pp, so nothing collides with the app's own names. The test suite came
# with it (tests/testthat/). For the LLM-optimization loop over the
# parser - the corpus, the master outcome sheet, and how to re-attack the
# problem - see AGENTS.md section "The parser optimization loop".

#' parsePDF module: baseline demographic tables out of trial PDFs
#'
#' The package reads the baseline characteristics table ("Table 1") of a
#' randomized controlled trial out of the article PDF and returns it as one
#' line per baseline variable per treatment arm, in the input format of the
#' Integrity-Analysis Shiny app.
#'
#' Two engines produce that table:
#'
#' * The **deterministic engine** ([parseBaselineTableHeuristics()]) works from
#'   the word coordinates in the PDF text layer. It clusters words into visual
#'   lines, clusters numeric cells into treatment-arm columns, and recognizes
#'   cells with regular expressions. Nothing is sent anywhere; the same PDF
#'   always yields the same answer, and every number can be traced to a printed
#'   cell. This engine runs first, always.
#' * The **AI fallback** ([parseBaselineTableAI()]) sends the text of the table
#'   page to the Claude API and asks for the table back as structured JSON. It
#'   runs only when the deterministic engine leaves something unread, and every
#'   value it contributes is recorded in the result's `provenance` table so a
#'   reader can tell the two apart.
#'
#' [parseBaselineTable()] is the entry point that combines them.
#'
#' Because this package is used to prepare data for research-integrity
#' analysis, treat both engines as a first pass: **check the parsed table
#' against the printed table before drawing any conclusion from it.**
#'
#' @name parsePDF-module
#' @keywords internal
NULL
