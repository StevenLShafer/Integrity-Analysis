# app_run.R — the exported entry point.
#
# PROVENANCE: new in phase 1 of the package restructure (Claude Code, model
# Claude Fable 5, 2026-08-16 — see docs/package-restructure-plan.md), adapted
# from stanpumpR's R/app_run.R. The library() calls are the ones that opened
# the old global.R, moved here verbatim: the app's code calls these packages
# unqualified (dashboardPage, read.xlsx, %do%, s.u, dqrnorm, ...), so they
# must be attached to the search path at launch, exactly as Shiny's
# auto-sourcing of global.R used to do. Qualifying every call with :: is
# phase 2+ work, if ever - stanpumpR ships this same pattern.

#' Launch the Integrity Analysis Shiny app
#'
#' @param testNote optional single string shown as a prominent banner under
#'   the header, naming the PR under test and what to look at (e.g.
#'   `"PR #6: function extraction - exercise upload, validate, analyze"`).
#'   PR test deployments (`IntegrityAnalysis_PR_<n>`) deploy an `app.R` that
#'   passes this; production passes nothing and shows no banner.
#'
#' @export
run_app <- function(testNote = NULL) {
  suppressWarnings(suppressPackageStartupMessages({
    library(shiny)
    library(openxlsx)       # read.xlsx / write.xlsx (xlsx upload + results download)
    library(readxl)         # read_excel (legacy .xls upload)
    library(Rfast)          # rowmeans / rowsums on the Monte Carlo matrix
    library(shinyjs)
    library(shinyWidgets)   # actionBttn
    library(foreach)        # %do% loop over rows in P_Calc
    library(MBESS)          # s.u: unbiased SD correction for small N
    library(dqrng)          # dqrnorm: fast RNG for the simulated means
    library(bslib)          # input_task_button
    library(shinydashboard)
  }))

  # A packaged app has no auto-served www/ directory; serve inst/www under
  # the "www" prefix that app_ui()'s asset references use.
  shiny::addResourcePath(
    "www", system.file("www", package = "IntegrityAnalysis"))

  # Shiny's default upload cap is 5 MB - too small for journal article
  # PDFs, which the app parses since the PDF-upload feature (2026-08-17).
  options(shiny.maxRequestSize = 50 * 1024^2)

  shiny::shinyApp(app_ui(testNote), app_server)
}
