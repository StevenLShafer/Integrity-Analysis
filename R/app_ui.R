# Testing Baseline RCT Values for Fraud / Error
# August  2025

###############################
# UI                          #
###############################
#
# PROVENANCE: was ui.R at the repository root until the package restructure
# (phase 1, Claude Code model Claude Fable 5, 2026-08-16 - see
# docs/package-restructure-plan.md). Two changes only:
#   - the top-level `ui <- ...` object became the function app_ui(), so the
#     page is built when run_app() asks for it rather than at package load
#     (the stanpumpR pattern; a load-time object would also fail R CMD check,
#     since building it calls shinydashboard before packages are attached).
#   - the three static assets are referenced through the "www/" resource
#     prefix that run_app() registers with addResourcePath(), because a
#     packaged app has no auto-served www/ directory. The files themselves
#     moved unchanged to inst/www/.
# Phase 2 (same date) added the testNote banner: Steve wants a PR test
# deployment to SAY, in the app itself, which PR it is and what to test,
# so triage never requires opening GitHub. When testNote is NULL (the
# production app.R) the page is built exactly as before.

app_ui <- function(testNote = NULL)
  dashboardPage(
    title = "RCT Integrity Analysis",
    dashboardHeader(
      title = 
        div(
          h3(
            "Evaluation of Baseline Data Integrity", 
            style="margin: 0;"
            ), 
          h4(
            "Carlisle Shafer 'Monte Carlo' approach", 
            style="margin: 0;"
            )
          ),
      titleWidth = "100%"
      ),
    dashboardSidebar(
      collapsed = FALSE,
      title = "Instructions",
      tags$style(".skin-blue .sidebar .shiny-download-link { color: #444; }"),
      tags$style(".sidebar { height: 10px; }"),
      p(),
      downloadButton("documentation", "Download Documentation"),
      p(),
      downloadButton("template", "Download Template"),
      p(),
      downloadButton("example", "Download Example"),
      br(),
      br(),
      br(),
      h6(
        "Developed from John Carlisle's analysis of fraudulent research studies ",
        "(references 2012, 2015, and 2017, see documentation) using the Monte Carlo approach",
        "developed by John Carlisle and Steve Shafer."
        ),
      br(),
      HTML(
        '<p>
        <h6>Please direct questions and feedback to Steve Shafer at
        <a href="mailto:steven.shafer@stanford.edu">steven.shafer@stanford.edu</a>
        .
        </h6>
        </p>'
        )
      ),
    dashboardBody(
      # Visible only on PR test deployments (run_app(testNote = ...)):
      # a banner naming the PR and what to test, so the tester never has
      # to inspect the PR itself to know what to look for.
      if (!is.null(testNote))
        fluidRow(
          div(
            strong("TEST DEPLOYMENT - "), testNote,
            style = paste0(
              "background-color: #f39c12; color: #000; padding: 8px 5%; ",
              "font-size: 15px; border-bottom: 2px solid #c87f0a;")
          )
        ),
      shinyjs::useShinyjs(),
      tags$script(src = "www/app.js"),
      tags$head(tags$link(href = "www/app.css", rel = "stylesheet")),
      style = "max-height: 95vh; overflow-y: auto;" ,
      tags$head(
        tags$style(
          type="text/css", 
          "#inline label { 
          display: table-cell; 
          text-align: center; 
          vertical-align: middle; 
          } 
        #inline .form-group {
        display: table-row;
        }"
        )
      ),
      fluidRow(
        img(
          src='www/Table.png', align = "right", width = "100%"
        ),
        style = 'border-bottom: 1px solid; padding-left: 5%; padding-right: 5%; padding-bottom: 2%'
      ),  
      fluidRow(
        column(
          12,
          HTML(paste0(
            "<br>Select one or more data entry spreadsheets (csv, xls, ",
            "xlsx) and/or article PDFs - files combine into one table, ",
            "distinguished by trial - or start with an empty table and ",
            "type the data in<br>")),
          fileInput("upload", NULL, multiple = TRUE,
                    accept = c(".csv", ".xls", ".xlsx", ".pdf")),
          actionButton("blank", "Start With an Empty Table"),
          HTML("<br><br>"),
          # The editable pre-analysis grid (Steve's request, 2026-08-17):
          # whatever the upload produced - spreadsheet rows or a PDF
          # extraction - is shown here for inspection and editing BEFORE
          # any statistics run. Edits take effect through the "Apply
          # Edits & Revalidate" button below the grid; for a parsed PDF
          # this is where a missing arm N gets filled in directly.
          rhandsontable::rHandsontableOutput("dataGrid"),
          uiOutput("validateButton"),
          uiOutput("GoButton"),
          # Appears after a PDF parse: the extracted table as a spreadsheet,
          # so a partial extraction is a round trip (fill the gaps, re-upload
          # the spreadsheet) rather than a dead end - the failure contract
          # from ISSUES.md issue 1.
          uiOutput("extractedButton"),
          uiOutput("logContent"),
          uiOutput("downloadButton")
        )
      ),
      uiOutput("stopButton")
    )
  )
