
###############################
# Server                      #
###############################
#
# PROVENANCE: was server.R at the repository root until the package
# restructure (phase 1, Claude Code model Claude Fable 5, 2026-08-16 — see
# docs/package-restructure-plan.md); phase 2 (same date) then moved the
# computation out: P_Calc() to R/P_Calc.R (now taking DATA, CategoryNames
# and m as arguments instead of reading them from this environment),
# is_category() and the upload-validation pipeline to R/validateData.R
# (returning derived state instead of assigning it with <<-). Verified
# bit-identical under fixed seeds against the phase-1 build. What remains
# here is Shiny wiring: reactives, observers, download handlers.
#
# PROVENANCE: Bug-fix pass by Claude Code (model: Claude Fable 5), 2026-08-14,
# on the 2025-09-01 original. Each fix is commented in place with "FIX:".
# All fixes verified by running the app locally against Example.xlsx
# (see PR description for the list and rationale).
#
# Add adjustment to SD for number of subjects
# Add ability to download raw data
# Add line by line integrity checks
# Determine categoricals by Integer only and < 6 types
# Permit comments in file
# Results file should only add P values to original file
# Enforce order: Trial ROW (P value) N MEAN SD
# For Observations decimals, just look for OBS. New name will be Round_Observations
# For Mean Dec, just look for an MEAN that does not equal "MEAN"
# Look for rapid rnorm function
# Cutoff for number of categories (probably 5)


app_server <- function(input, output, session) {
  reactiveData <- reactiveVal()
  reactiveDataValidated <- reactiveVal()
  reactiveResults <- reactiveVal()
  reactiveDone <- reactiveVal(FALSE)
  output$downloadButton <- NULL
  output$logContent <- NULL
  output$GoButton <- NULL

  # FIX: per-session state. These were previously globals assigned with <<-
  # from global.R, which meant every concurrent user session in the same R
  # process shared (and clobbered) one copy of the data mid-analysis.
  # Declaring them here gives each session its own copy; the existing <<-
  # assignments below now bind to these because server() is the nearest
  # enclosing environment.
  OUTPUT <- NULL         # accumulated results across trials, for download
  DATA <- NULL           # validated data table for the current upload
  TRIALS <- NULL         # unique trial identifiers in DATA
  ColumnNames <- NULL    # cleaned-up column names of DATA
  CategoryNames <- NULL  # columns holding categorical (count) data
  pdfResult <- NULL      # ParsePDFTable from a PDF upload, for the
                         # "Download Extracted Table" round trip

  # FIX: removed stopImplicitCluster() and the commented-out doParallel
  # cluster setup. The row loop in P_Calc runs sequentially (%do%), so no
  # parallel backend is in play; the doParallel calls only created worker
  # processes that were never used. Restore a single parallel framework
  # (future/doFuture OR doParallel, not both) if/when the loop is
  # parallelized.

  output$stopButton <-
    renderUI({
      fluidRow(
        column(
          12,
          br(),
          actionBttn("stop", HTML("&nbsp; &nbsp; EXIT &nbsp; &nbsp;"), style = "gradient", size = "xs", color = "warning"),
          br()
        )
      )
    })


  # Write out logs to the log section
  initLogMsg <- "Comments Log"
  commentsLog <- reactiveVal(NULL)
  output$logContent <- renderUI({
    invalidateLater(1000)
    HTML(commentsLog())
  })
  # Register the comments log with this user's session, to use outside the server
  session$userData$commentsLog <- commentsLog

  ###########################################################
  # The editable pre-analysis grid                          #
  ###########################################################
  # Steve's request (2026-08-17): every input path lands its table in an
  # editable grid (rhandsontable, the same machinery stanpumpR uses) so
  # the data can be inspected and corrected BEFORE any statistics run.
  # Validation still runs automatically on upload - a clean file goes
  # straight to the Analyze button, exactly as before - but the user can
  # now fix what validation flags (a missing N, a mistyped SD) directly
  # in the grid and revalidate, instead of editing the file and
  # re-uploading. Revalidation is EXPLICIT (a button), not per-keystroke:
  # each validation pass writes line-by-line messages to the comments
  # log, and firing it on every cell edit would bury the user in output.

  output$dataGrid <- rhandsontable::renderRHandsontable({
    d <- reactiveData()
    if (is.null(d)) return(NULL)
    rhandsontable::rhandsontable(
      d,
      # cap the widget height; rhandsontable scrolls and virtualizes rows
      height = min(400, 60 + 24 * nrow(d)),
      rowHeaders = TRUE) |>
      rhandsontable::hot_table(highlightRow = TRUE, highlightCol = TRUE)
  })

  output$validateButton <- renderUI({
    if (is.null(reactiveData())) return(NULL)
    tagList(
      actionButton("applyEdits", "Apply Edits & Revalidate"),
      HTML("<br><br>"))
  })

  observeEvent(input$applyEdits, {
    if (is.null(input$dataGrid)) return()
    # Test seam: a real client always sends the handsontable payload (a
    # list) which hot_to_r() decodes; shiny::testServer can instead
    # inject a plain data.frame directly, keeping this flow headlessly
    # testable without faking the widget's wire format.
    edited <- if (is.data.frame(input$dataGrid)) input$dataGrid
              else rhandsontable::hot_to_r(input$dataGrid)
    # A fresh validation pass gets a fresh log, and any previous results
    # are discarded - the edited table is now the data of record.
    commentsLog(NULL)
    OUTPUT <<- NULL
    reactiveDone(FALSE)
    output$downloadButton <- NULL
    reactiveDataValidated(NULL)
    output$GoButton <- NULL
    reactiveData(edited)
  })

  ###########################################################
  # Processing Loop                                         #
  ###########################################################

  observeEvent(
    {
      input$go
    },
    {
      output$stopButton <- NULL
      progress <- shiny::Progress$new(session, style = "notification")
      on.exit(progress$close())
      DATA <<- reactiveDataValidated()
      # FIX: results from any previous run are discarded here. OUTPUT was
      # never reset, so analyzing a second file (or re-analyzing) in the same
      # session appended new results to the old ones in the downloaded
      # spreadsheet.
      OUTPUT <<- NULL
      start_time <- Sys.time()
      # (Progress message wording below taken from the 2025-09-01 local copy
      # on g:, which post-dated the GitHub upload.)
      progress$set(message = "Processed Trial ", value = 0)
      # FIX: removed "cores <- detectCores() - 1; registerDoParallel(cores)".
      # The P_Calc loop runs sequentially (%do%), so this registered a
      # parallel backend that was never used.
      LengthTrials <- length(TRIALS)
      for (i in 1:LengthTrials)
      {
        TRIAL <- TRIALS[i]
        OUTPUT <<- rbind(
          OUTPUT,
          P_Calc(TRIAL, DATA, CategoryNames, m)
        )
        progress$set(
          value = i / LengthTrials,
          detail = paste0(TRIAL, ", P = ",OUTPUT$P[nrow(OUTPUT)-1]))
      }
      # FIX: removed 'with(registerDoFuture(), local = TRUE)' (the line the
      # original marked "Not sure which is correct"). with() has no 'local'
      # argument, so this always threw "argument is missing, with no
      # default", aborting the observer here - which is why the EXIT button
      # was never restored, the execution time never logged, and
      # reactiveDone(TRUE) never ran, so the Download Results button never
      # appeared.
      output$stopButton <-
        renderUI({
          fluidRow(
            column(
              12,
              br(),
              actionBttn("stop", HTML("&nbsp; &nbsp; EXIT &nbsp; &nbsp;"), style = "gradient", size = "xs", color = "warning"),
              br()
            )
          )
        })
    outputComments(paste("Execution time", round(Sys.time() - start_time, 2)))
    reactiveDone(TRUE)
    }
  )


  ###########################################################
  # Upload Data Routines                                    #
  ###########################################################

  observeEvent(
    {
      input$upload
    },
    {
      reactiveResults(NULL)
      reactiveDone(FALSE)
      commentsLog(NULL)
      # FIX: also clear the results and buttons from any previous file, so a
      # failed or fresh upload can't be analyzed/downloaded against stale data
      OUTPUT <<- NULL
      reactiveDataValidated(NULL)
      output$GoButton <- NULL
      output$downloadButton <- NULL
      pdfResult <<- NULL
      output$extractedButton <- NULL

      Filename <- input$upload$datapath
      ext <- tolower(tools::file_ext(Filename))

      # Switch statement started to fail parsing... ????
      if (!ext %in% c("csv", "xlsx", "xls", "pdf"))
      {
        outputComments(
          paste0(".", ext, " is not a supported file type.")  # FIX: spacing
        )
        return()
      }

      # PDF upload (Steve's request, 2026-08-17): parse the article's
      # baseline table and feed it into the SAME validation pipeline the
      # spreadsheets use. Design constraints, all deliberate:
      #   - Deterministic engine only (ai = "never"): manuscripts under
      #     review are confidential, so nothing may leave the server, and
      #     the same PDF must always yield the same verdict.
      #   - Parsed in a SUBPROCESS with an OS timeout, never in-process:
      #     ~2% of real journal PDFs hang poppler indefinitely, R cannot
      #     interrupt it, and an in-process hang would take this worker
      #     down for every connected user.
      #   - A partial extraction is a round trip, not a dead end: the
      #     extracted table is offered as a download in the app's own
      #     input layout, so the user fills the gaps the printed table
      #     did not provide (most often arm N) and re-uploads the
      #     spreadsheet. Expectation from the corpus (see
      #     corpus/ParseOutcomes.csv): roughly a third of PDFs yield a
      #     fully analysable trial; the spreadsheet path is the reliable
      #     one and this is the convenient one.
      if (ext == "pdf")
      {
        progress <- shiny::Progress$new(session, style = "notification")
        progress$set(message = "Parsing PDF ",
                     detail = "deterministic engine, up to 60 s")
        res <- parseBaselineTableFiles(Filename, ai = "never",
                                       timeout = 60, quiet = TRUE)
        progress$close()
        r <- res$result[[1]]
        if (is.null(r) || nrow(r$data) == 0)
        {
          # The parser's error text is written for the R console; strip
          # the advice that means nothing inside the app (`pages=`,
          # `layout=`, ai = "always", ocr = TRUE), and replace the
          # uploaded temp-file path with the name the user actually chose.
          msg <- res$error[1]
          msg <- gsub(Filename, input$upload$name, msg, fixed = TRUE)
          msg <- sub(" Try the `pages` or `layout` argument, or ai = \"always\"\\.",
                     "", msg)
          msg <- sub(" Re-run with ocr = TRUE\\.",
                     " (a scanned image with no text layer - the parser reads text, not pictures)",
                     msg)
          outputComments(paste0(
            "Could not extract a baseline table from ", input$upload$name,
            ": ", msg))
          outputComments(paste(
            "This is expected for roughly a quarter of published PDFs -",
            "layouts vary more than any parser can. You can still analyze",
            "this trial by entering its table into the Template",
            "spreadsheet (sidebar) and uploading that."))
          return()
        }

        # The parse narrative, then every unusable line and why - the
        # user should never have to guess what the parser did.
        outputComments(paste0(
          "Extracted the baseline table from ", input$upload$name,
          ": table page ", res$page[1], ", ", res$arms[1], " arm(s) (",
          res$armsWithN[1], " with N), ", res$variables[1],
          " variable(s), ", res$continuous[1], " with mean and SD."))
        if (nrow(r$skipped) > 0)
        {
          outputComments(paste0(
            nrow(r$skipped), " table line(s) could not be used:"))
          for (k in seq_len(nrow(r$skipped)))
            outputComments(paste0("- ", r$skipped$label[k], ": ",
                                  r$skipped$reason[k]))
        }

        # The uploaded temp file has an opaque name; label the trial with
        # the real file name the user chose.
        r$data$TRIAL <- tools::file_path_sans_ext(input$upload$name)
        pdfResult <<- r
        output$extractedButton <- renderUI({
          downloadButton("extracted", "Download Extracted Table")
        })

        DATA <<- r$data
        reactiveData(DATA)
        return()
      }

      if (ext == "csv")
      {
        DATA <<- read.csv(Filename)
        reactiveData(DATA)
        return()
      }
      if (ext == "xlsx")
      {
        DATA <<- read.xlsx(Filename)
        reactiveData(DATA)
        return()
      }
      if (ext == "xls")
      {
        # FIX: was read.xl(), which does not exist in any loaded package -
        # every .xls upload crashed the session. readxl::read_excel() is the
        # correct reader; as.data.frame() converts its tibble return value,
        # whose different [,col] subsetting semantics would otherwise break
        # the column handling downstream.
        DATA <<- as.data.frame(read_excel(Filename))
        reactiveData(DATA)
        return()
      }
    }
  )

  observeEvent(
    {
      reactiveData()
    },
    {
      DATA <- reactiveData()
      if (is.null(DATA))
      {
        return()
      }

      # Phase 2: the validation pipeline lives in validateData()
      # (R/validateData.R). It reports problems itself through
      # outputComments() and returns the derived state; assignment to the
      # per-session variables stays here, where the session is.
      v <- validateData(DATA)
      if (v$FAIL)
      {
        return()
      }

      # Assign globally
      DATA <<- v$DATA
      TRIALS <<- v$TRIALS
      ColumnNames <<- v$ColumnNames
      CategoryNames <<- v$CategoryNames

      LengthTrials <- length(v$TRIALS)
      # FIX: the Analyze button now renders into output$GoButton - the slot
      # ui.R provides for it - instead of output$downloadButton. Sharing the
      # download slot made the Analyze button disappear as soon as results
      # were ready, so a trial could not be re-analyzed without re-uploading.
      # Also removed the style/size/color arguments: those belong to
      # shinyWidgets::actionBttn, not bslib::input_task_button, which was
      # silently emitting them as meaningless HTML attributes (including
      # style="gradient", which is invalid CSS). input_task_button is kept
      # for its automatic busy state during the long simulation.
      # (Also removed a leftover debugging cat() and a dead HTML("<br>")
      # whose value was discarded.)
      if (LengthTrials == 1)
      {
        output$GoButton <- renderUI({
          input_task_button(
            "go", HTML("&nbsp; &nbsp; Analyze Trial &nbsp; &nbsp;"))
        })
      } else {
        output$GoButton <- renderUI({
          input_task_button(
            "go", HTML(
              paste(
                "&nbsp; &nbsp; Analyze", LengthTrials, "Trials &nbsp; &nbsp;")))
        })
      }
      # Set reactive value
      reactiveDataValidated(v$DATA)

    }
  )



  observeEvent(
    {
      reactiveDone()
    },
    {
      DONE <- reactiveDone()
      if (!DONE)
      {
        output$downloadButton <- NULL
      } else {
        output$downloadButton <- renderUI({
          downloadButton("download", "Download Results")
          })
      }
    }
  )

  output$download <- downloadHandler(
    filename = function() {
      paste0("Integrity Analysis.",format(Sys.time(), format = "%y%m%d-%H%M%S"), ".xlsx")
    },
    content = function(file) {
      x <- OUTPUT
      # One-sided toward homogeneity (issue 6): a single P column. Small
      # P = baseline data more homogeneous than random sampling explains.
      names(x) <- c("TRIAL", "ROW", "P (one-sided toward homogeneity)")
      write.xlsx(x, file)
    })

  # The extracted-table round trip: writeIntegrityTemplate() emits the
  # app's own input layout (plus Provenance and Skipped sheets after the
  # data; the app reads only the first sheet on re-upload).
  output$extracted <- downloadHandler(
    filename = function() {
      paste0("Extracted ",
             tools::file_path_sans_ext(input$upload$name), ".xlsx")
    },
    content = function(file) {
      writeIntegrityTemplate(pdfResult, file)
    })

  output$documentation <- downloadHandler(
    filename = function() {
      "IntegrityAnalysis.pdf"
    },
    content = function(file) {
      file.copy(system.file("extdata", "IntegrityAnalysis.pdf",
                            package = "IntegrityAnalysis"), file)
    })

  output$template <- downloadHandler(
    filename = function() {
      "Template for Integrity Analysis.xlsx"
    },
    content = function(file) {
      write.xlsx(read.xlsx(system.file("extdata", "Template.xlsx",
                                       package = "IntegrityAnalysis")), file)
    })


    output$example <- downloadHandler(
    filename = function() {
      "Example for Integrity Analysis.xlsx"
    },
    content = function(file) {
      write.xlsx(read.xlsx(system.file("extdata", "Example.xlsx",
                                       package = "IntegrityAnalysis")), file)
    })

  observeEvent(input$stop, {
    stopApp(returnValue = invisible())
  })
}
