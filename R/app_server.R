
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
  skipValidation <- FALSE  # one-shot: the blank-table starter sets this so
                           # eight empty rows are not validated (and flagged
                           # line by line) before the user has typed anything
  uploadedPaths <- character(0)  # every file this session uploaded, for
                                 # the purge-on-exit guarantee below

  # THE PURGE GUARANTEE (Steve's requirement, 2026-08-17): when the
  # session ends, no record of the analysis survives. Uploaded files
  # (manuscript PDFs and spreadsheets) are deleted from disk along with
  # the per-upload temp directories Shiny created for them; the in-memory
  # state (data, results, log) dies with the session environment. Nothing
  # in this app writes analysis content anywhere else: downloads are
  # generated straight into the response, outputComments() keeps no file
  # log, and bookmarking is not enabled. Manuscripts under review are
  # confidential - this is a promise to the people uploading them, and
  # any future code path that touches an uploaded file must preserve it.
  session$onSessionEnded(function() {
    for (p in uploadedPaths) {
      try(unlink(p, force = TRUE), silent = TRUE)
      # Shiny stages each upload in its own temp subdirectory; remove it
      # too so not even the file NAME survives.
      try(unlink(dirname(p), recursive = TRUE, force = TRUE),
          silent = TRUE)
    }
    OUTPUT <<- NULL
    DATA <<- NULL
  })

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
    w <- rhandsontable::rhandsontable(
      d,
      # cap the widget height; rhandsontable scrolls and virtualizes rows
      height = min(400, 60 + 24 * nrow(d)),
      rowHeaders = TRUE) |>
      rhandsontable::hot_table(highlightRow = TRUE, highlightCol = TRUE) |>
      # Right-click menu for inserting and deleting ROWS (Steve's request,
      # 2026-08-17 - essential for the blank-entry mode). Column editing
      # stays off: handsontable's added columns cannot be NAMED from the
      # grid, and column names are the data model here (category columns
      # are recognized by being extra named integer columns).
      rhandsontable::hot_context_menu(allowRowEdit = TRUE,
                                      allowColEdit = FALSE)
    # Column display formats (Steve, 2026-08-17): rhandsontable's numeric
    # default shows two decimals, which made counts and the rounding
    # columns read as "25.00". Whole-number columns display as integers;
    # measurement columns (MEAN/SD/SE) keep their decimals as typed (up
    # to five, trailing zeros dropped). MEAN/SD/SE are never
    # integer-formatted even when their values happen to be whole,
    # because numbro's "0" format would DISPLAY a later-typed 63.5 as 64
    # while storing 63.5 - a lie on screen.
    measureCols <- intersect(c("MEAN", "SD", "SE"), names(d))
    for (nm in names(d)) {
      v <- d[[nm]]
      if (!is.numeric(v)) next
      if (nm %in% measureCols) {
        w <- rhandsontable::hot_col(w, nm, format = "0.[00000]")
      } else if (all(is.na(v) | v %% 1 == 0)) {
        # N, ROUND_MEAN, ROUND_DISPERSION, ROUND_OBSERVATION, category
        # counts - anything whole-numbered
        w <- rhandsontable::hot_col(w, nm, format = "0")
      } else {
        w <- rhandsontable::hot_col(w, nm, format = "0.[00000]")
      }
    }
    w
  })

  output$validateButton <- renderUI({
    if (is.null(reactiveData())) return(NULL)
    tagList(
      actionButton("applyEdits", "Apply Edits & Revalidate"),
      actionButton("addRows", "Add 5 Rows"),
      div(style = "display: inline-block; vertical-align: top;",
          textInput("newColName", NULL, placeholder = "new column name",
                    width = "180px")),
      actionButton("addCol", "Add Column"),
      HTML("<br><br>"))
  })

  # Explicit structural controls (Steve, 2026-08-17: the right-click menu
  # proved undiscoverable/unreliable in deployment, and it can never NAME
  # a new column - and column names are the data model). Both controls
  # preserve any edits currently sitting in the grid (hot_to_r on the live
  # widget), and skip the validation pass: adding empty structure is not
  # a data change worth a fresh error log.
  currentGrid <- function() {
    if (!is.null(input$dataGrid)) {
      if (is.data.frame(input$dataGrid)) input$dataGrid
      else rhandsontable::hot_to_r(input$dataGrid)
    } else reactiveData()
  }

  observeEvent(input$addRows, {
    d <- currentGrid()
    if (is.null(d)) return()
    blank <- d[0, ]
    blank[1:5, ] <- NA
    skipValidation <<- TRUE
    reactiveData(rbind(d, blank))
  })

  observeEvent(input$addCol, {
    d <- currentGrid()
    if (is.null(d)) return()
    nm <- trimws(input$newColName)
    if (!nzchar(nm)) {
      outputComments("Type a name for the new column first.")
      return()
    }
    if (toupper(nm) %in% toupper(names(d))) {
      outputComments(paste0("A column named ", nm, " already exists."))
      return()
    }
    d[[nm]] <- NA_real_   # numeric: new columns are category counts
    skipValidation <<- TRUE
    reactiveData(d)
    updateTextInput(session, "newColName", value = "")
  })

  observeEvent(input$applyEdits, {
    if (is.null(input$dataGrid)) return()
    # Test seam: a real client always sends the handsontable payload (a
    # list) which hot_to_r() decodes; shiny::testServer can instead
    # inject a plain data.frame directly, keeping this flow headlessly
    # testable without faking the widget's wire format.
    edited <- if (is.data.frame(input$dataGrid)) input$dataGrid
              else rhandsontable::hot_to_r(input$dataGrid)
    # Rows with no content at all are dropped silently - the blank-entry
    # starter provides eight empty rows, and unused ones are not data
    # entry errors.
    keep <- apply(edited, 1, function(r)
      any(!is.na(r) & trimws(as.character(r)) != ""))
    edited <- edited[keep, , drop = FALSE]
    if (nrow(edited) == 0) {
      outputComments("The table has no data yet.")
      return()
    }
    # A TRIAL column left entirely blank means a single trial - the same
    # convention as a spreadsheet with no TRIAL column at all.
    if ("TRIAL" %in% names(edited) &&
        all(is.na(edited$TRIAL) |
            trimws(as.character(edited$TRIAL)) == ""))
      edited$TRIAL <- 1
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

  # Blank-table entry (Steve's request, 2026-08-17): start from nothing
  # and type everything in the grid. Eight empty rows in the canonical
  # column layout, plus three placeholder category columns (leave unused
  # ones blank - fully empty rows and all-NA columns are harmless).
  # Validation is skipped for this initial empty frame (skipValidation);
  # it runs when the user clicks Apply Edits & Revalidate.
  observeEvent(input$blank, {
    reactiveResults(NULL)
    reactiveDone(FALSE)
    commentsLog(NULL)
    OUTPUT <<- NULL
    reactiveDataValidated(NULL)
    output$GoButton <- NULL
    output$downloadButton <- NULL
    blank <- data.frame(
      TRIAL = rep(NA_character_, 8), ROW = NA_character_,
      N = NA_real_, MEAN = NA_real_, SD = NA_real_, SE = NA_real_,
      ROUND_MEAN = NA_real_, ROUND_DISPERSION = NA_real_,
      ROUND_OBSERVATION = NA_real_,
      CAT1 = NA_real_, CAT2 = NA_real_, CAT3 = NA_real_,
      stringsAsFactors = FALSE)
    skipValidation <<- TRUE
    DATA <<- blank
    reactiveData(blank)
    outputComments(paste(
      "Empty table ready. Type your data into the grid (right-click to",
      "add or delete rows); CAT1-CAT3 are placeholders for categorical",
      "count columns - leave unused ones blank. When done, click Apply",
      "Edits & Revalidate."))
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

      # Multi-file upload (Steve's request, 2026-08-17): any mix of
      # csv/xls/xlsx/PDF in one selection. Every file becomes a data
      # frame; the frames concatenate into ONE table distinguished by the
      # TRIAL column, which is what P_Calc analyzes trial by trial with
      # no cross-talk.
      #
      # TRIAL bookkeeping across files:
      #   - a file without a TRIAL column gets its own file name (sans
      #     extension) as the trial - the single-file "TRIAL <- 1" rule
      #     does not survive two files;
      #   - if the SAME trial value appears in more than one file (two
      #     spreadsheets both numbered 1, 2, ...), every trial in the
      #     later file is prefixed "filename: " so nothing silently
      #     merges into one trial.
      #
      # PDFs go through parseBaselineTableFiles() in ONE call - a
      # subprocess per file with an OS timeout (~2% of real PDFs hang
      # poppler; an in-process hang would take this worker down for every
      # user), deterministic engine only (ai = "never": manuscripts are
      # confidential, verdicts must be reproducible). A failed parse is
      # reported per file and the rest continue.
      files <- input$upload
      # record for the purge-on-exit guarantee (see session$onSessionEnded)
      uploadedPaths <<- unique(c(uploadedPaths, files$datapath))
      files$ext <- tolower(tools::file_ext(files$name))
      files$stem <- tools::file_path_sans_ext(files$name)

      bad <- !files$ext %in% c("csv", "xlsx", "xls", "pdf")
      for (nm in files$name[bad])
        outputComments(paste0(nm, " is not a supported file type."))
      files <- files[!bad, , drop = FALSE]
      if (nrow(files) == 0) return()

      frames <- list()

      readSheet <- function(path, ext) {
        if (ext == "csv")  return(read.csv(path))
        if (ext == "xlsx") return(read.xlsx(path))
        # FIX (from the single-file code): read.xl() never existed;
        # readxl::read_excel() is the reader, as.data.frame() because a
        # tibble's [,col] semantics break the column handling downstream.
        as.data.frame(read_excel(path))
      }
      for (i in which(files$ext != "pdf")) {
        d <- tryCatch(readSheet(files$datapath[i], files$ext[i]),
                      error = function(e) NULL)
        if (is.null(d) || nrow(d) == 0) {
          outputComments(paste0("Could not read ", files$name[i], "."))
          next
        }
        outputComments(paste0("Read ", files$name[i], ": ", nrow(d),
                              " row(s)."))
        frames[[length(frames) + 1]] <-
          list(stem = files$stem[i], data = d)
      }

      pdfIdx <- which(files$ext == "pdf")
      if (length(pdfIdx) > 0) {
        progress <- shiny::Progress$new(session, style = "notification")
        progress$set(message = "Parsing PDF(s) ",
                     detail = paste0(length(pdfIdx),
                                     " file(s), up to 60 s each"))
        res <- parseBaselineTableFiles(files$datapath[pdfIdx],
                                       ai = "never", timeout = 60,
                                       quiet = TRUE)
        progress$close()
        for (k in seq_along(pdfIdx)) {
          i <- pdfIdx[k]
          r <- res$result[[k]]
          if (is.null(r) || nrow(r$data) == 0) {
            # Console advice (`pages=`, ai = "always", ocr = TRUE) means
            # nothing inside the app; translate, and show the user's own
            # file name rather than the upload temp path.
            msg <- res$error[k]
            msg <- gsub(files$datapath[i], files$name[i], msg, fixed = TRUE)
            msg <- sub(" Try the `pages` or `layout` argument, or ai = \"always\"\\.",
                       "", msg)
            msg <- sub(" Re-run with ocr = TRUE\\.",
                       " (a scanned image with no text layer - the parser reads text, not pictures)",
                       msg)
            outputComments(paste0(
              "Could not extract a baseline table from ", files$name[i],
              ": ", msg))
            next
          }
          outputComments(paste0(
            "Extracted the baseline table from ", files$name[i],
            ": table page ", res$page[k], ", ", res$arms[k], " arm(s) (",
            res$armsWithN[k], " with N), ", res$variables[k],
            " variable(s), ", res$continuous[k], " with mean and SD."))
          if (nrow(r$skipped) > 0) {
            outputComments(paste0(
              nrow(r$skipped), " table line(s) could not be used:"))
            for (s in seq_len(nrow(r$skipped)))
              outputComments(paste0("- ", r$skipped$label[s], ": ",
                                    r$skipped$reason[s]))
          }
          d <- r$data
          d$TRIAL <- files$stem[i]   # opaque temp name -> the user's name
          frames[[length(frames) + 1]] <-
            list(stem = files$stem[i], data = d)
        }
      }

      if (length(frames) == 0) {
        outputComments(paste(
          "No file produced a usable table. You can enter the data by",
          "hand: use Start With an Empty Table, or fill in the Template",
          "spreadsheet (sidebar) and upload it."))
        return()
      }

      # TRIAL assignment and cross-file disambiguation.
      seen <- character(0)
      for (j in seq_along(frames)) {
        d <- frames[[j]]$data
        tcol <- grep("TRIAL", toupper(trimws(names(d))))
        if (length(tcol) == 0) {
          d$TRIAL <- frames[[j]]$stem
        } else {
          names(d)[tcol[1]] <- "TRIAL"
          if (all(is.na(d$TRIAL))) d$TRIAL <- frames[[j]]$stem
        }
        if (any(as.character(unique(d$TRIAL)) %in% seen)) {
          d$TRIAL <- paste0(frames[[j]]$stem, ": ", d$TRIAL)
          outputComments(paste0(
            "Trial identifiers in ", frames[[j]]$stem,
            " duplicate an earlier file; prefixed with the file name."))
        }
        seen <- c(seen, as.character(unique(d$TRIAL)))
        frames[[j]]$data <- d
      }

      # Concatenate on the union of columns (different files carry
      # different category columns; absent columns fill with NA, which is
      # exactly what the category rules expect).
      allCols <- unique(unlist(lapply(frames, function(f) names(f$data))))
      DATA <<- do.call(rbind, lapply(frames, function(f) {
        d <- f$data
        for (nm in setdiff(allCols, names(d))) d[[nm]] <- NA
        d[, allCols, drop = FALSE]
      }))
      if (length(frames) > 1)
        outputComments(paste0("Combined ", length(frames), " file(s): ",
                              nrow(DATA), " rows, ",
                              length(unique(DATA$TRIAL)), " trial(s)."))
      reactiveData(DATA)
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

      # The blank-table starter shows an empty grid to type into;
      # validating eight empty rows would flag every one of them.
      # One-shot skip: validation resumes on Apply Edits & Revalidate.
      if (skipValidation)
      {
        skipValidation <<- FALSE
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

  # Download the current table (generalized 2026-08-17 from the single-PDF
  # "Download Extracted Table": with multiple files and blank-entry there
  # is one combined table, and THAT is what the user wants to save - the
  # round trip for a partial PDF extraction, a checkpoint for hand-typed
  # data. Reflects the table as of the last upload / Apply Edits; the
  # file is valid input for a later upload.
  output$extractedButton <- renderUI({
    if (is.null(reactiveData())) return(NULL)
    tagList(downloadButton("extracted", "Download Table"),
            HTML("<br><br>"))
  })
  output$extracted <- downloadHandler(
    filename = function() {
      paste0("Integrity Data.",
             format(Sys.time(), format = "%y%m%d-%H%M%S"), ".xlsx")
    },
    content = function(file) {
      write.xlsx(reactiveData(), file, keepNA = FALSE)
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
