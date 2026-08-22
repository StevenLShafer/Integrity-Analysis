
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
  # one anonymous count per session (see R/usageCount.R; no-op unless
  # run_app enabled it - production only)
  countUsage("session")

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
  graphsData <- NULL     # per-row Monte Carlo draws for the issue-16
                         # graphs; refilled by every Analyze run
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
    # SAFETY GUARD (2026-08-19): only delete under tempdir(). A real
    # client's uploads are ALWAYS staged there (each in its own
    # subdirectory), so the guarantee is unchanged in production - but a
    # test driving this server with a real file path must not have that
    # file's parent DIRECTORY recursively deleted. (Learned the hard
    # way: a testServer run that uploaded corpus PDFs by their real
    # paths wiped the local corpus folder; it was rebuilt from sources.)
    tmp <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
    for (p in uploadedPaths) {
      pn <- try(normalizePath(p, winslash = "/", mustWork = FALSE),
                silent = TRUE)
      if (inherits(pn, "try-error") ||
          !startsWith(pn, paste0(tmp, "/"))) next
      try(unlink(pn, force = TRUE), silent = TRUE)
      # Shiny stages each upload in its own temp subdirectory; remove it
      # too so not even the file NAME survives - but never tempdir()
      # itself (a file placed at the temp root keeps the root).
      dp <- dirname(pn)
      if (startsWith(dp, paste0(tmp, "/")))
        try(unlink(dp, recursive = TRUE, force = TRUE), silent = TRUE)
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

  # Issue 13 (Steve's design): validation problems paint their CELLS -
  # yellow = missing, red = unreadable, blue = incongruent - so review
  # means looking at the table, not reading a log. rIssues holds the
  # (row, col, code) map from the last validateData pass.
  rIssues <- reactiveVal(NULL)

  # Table lines the PDF parser saw but could not use (r$skipped) become
  # GRID ROWS - label in ROW, everything else empty - so a parse loss is
  # a conspicuous colored row to fill in or delete, never a silent gap
  # (the PMID 14984519 lesson: missed variables diluted a real alarm).
  # This registry holds (TRIAL, ROW, reason); the renderer matches it
  # against the displayed frame by TRIAL + ROW (indices survive edits)
  # and paints the ROW cell red with the parser's reason as the hover
  # text. A row stops matching - and stops being red - the moment the
  # user fills any data into it, which is exactly right.
  parseSkips <- reactiveVal(NULL)

  # Values the parser DERIVED rather than read off the page - counts
  # converted from printed percentages (exact or, with the checkbox on,
  # approximate) and recovered arm Ns. They paint GREEN in the grid:
  # "OK to use, but best to check before it runs" (Steve, 2026-08-21).
  # Registry rows: (TRIAL, ROW, COL, note); ROW = "*" means every row of
  # the trial (used for the N column when arm sizes were recovered).
  parseDerived <- reactiveVal(NULL)

  # The legend IS the error report (Steve's direction, 2026-08-19): no
  # explanatory text prints below the table, so each color carries its
  # explanation here, and every colored cell explains itself on hover.
  output$issueLegend <- renderUI({
    hasDerived <- !is.null(parseDerived()) && nrow(parseDerived()) > 0
    if (is.null(rIssues()) && !hasDerived) return(NULL)
    entry <- function(color, label, text) div(
      style = "margin: 2px 0;",
      span(style = paste0("display:inline-block; width:14px; height:14px;",
                          "background:", color, "; border:1px solid #999;",
                          "vertical-align:middle; margin-right:6px;")),
      tags$b(label), paste0(" - ", text))
    div(style = "margin: 4px 0 8px 0; font-size: 13px;",
        if (hasDerived) entry("#d7f0d7", "derived", paste(
          "the parser computed this value - a percentage converted to a",
          "count, or an arm N recovered from the document. OK to use,",
          "but best to check before it runs; hover the cell to see how",
          "it was derived.")),
        entry("#fff3b0", "missing", paste(
          "a required value is empty. Enter it, or delete the row.",
          "Rows with a label but no data at all are left out of the",
          "analysis.")),
        entry("#f4b6b6", "unreadable", paste(
          "could not be read: text where a number belongs, or a table",
          "line the PDF reader could not use. Hover the cell for the",
          "reason.")),
        entry("#b8d0f0", "incongruent", paste(
          "the value conflicts with the row's type - for example an SD",
          "on a median/IQR row, or continuous entries on a category",
          "row.")),
        div(style = "margin-top: 4px;", paste(
          "Fix or delete the colored cells, then click Apply Edits &",
          "Revalidate. Hover any colored cell for details.")))
  })

  output$dataGrid <- rhandsontable::renderRHandsontable({
    d <- reactiveData()
    if (is.null(d)) return(NULL)
    # cell-issue payload for the renderer: keys "row|col", 0-based.
    # cellNotes carries per-cell hover text where a specific reason is
    # known (the parser's skip reasons); the renderer falls back to a
    # generic per-color explanation otherwise.
    issPayload <- NULL
    notePayload <- NULL
    # GREEN derived cells first, so that issue and skip colors - which
    # demand action rather than a glance - overwrite them on conflict.
    dv <- parseDerived()
    if (!is.null(dv) && nrow(dv) > 0 && all(c("TRIAL", "ROW") %in% names(d))) {
      issPayload <- list(); notePayload <- list()
      for (g in seq_len(nrow(dv))) {
        ci <- match(dv$COL[g], names(d))
        if (is.na(ci)) next
        hits <- if (dv$ROW[g] == "*")
          which(as.character(d$TRIAL) == dv$TRIAL[g])
        else
          which(as.character(d$TRIAL) == dv$TRIAL[g] &
                as.character(d$ROW) == dv$ROW[g])
        # paint only cells that carry a value - a green empty cell would
        # read as "this blank is fine", which is the opposite of true
        hits <- hits[!is.na(d[hits, ci])]
        for (r in hits) {
          key <- paste0(r - 1, "|", ci - 1)
          issPayload[[key]] <- "derived"
          notePayload[[key]] <- dv$note[g]
        }
      }
      if (length(issPayload) == 0) { issPayload <- NULL; notePayload <- NULL }
    }
    iss <- rIssues()
    if (!is.null(iss)) {
      ci <- match(iss$col, names(d))
      ok <- !is.na(ci) & iss$row <= nrow(d)
      if (any(ok)) {
        if (is.null(issPayload)) issPayload <- list()
        addI <- as.list(iss$code[ok])
        names(addI) <- paste0(iss$row[ok] - 1, "|", ci[ok] - 1)
        issPayload[names(addI)] <- addI
        # cell-specific hover text where validateData supplied one
        # (e.g. the single-line-categorical explanation)
        if ("note" %in% names(iss)) {
          noted <- ok & !is.na(iss$note)
          if (any(noted)) {
            if (is.null(notePayload)) notePayload <- list()
            addN <- as.list(iss$note[noted])
            names(addN) <- paste0(iss$row[noted] - 1, "|", ci[noted] - 1)
            notePayload[names(addN)] <- addN
          }
        }
      }
    }
    sk <- parseSkips()
    if (!is.null(sk) && nrow(sk) > 0 &&
        all(c("TRIAL", "ROW") %in% names(d))) {
      rowCol <- match("ROW", names(d))
      dataCols <- intersect(c("N", "MEAN", "SD", "SE", "Q1", "Q3"),
                            names(d))
      for (s in seq_len(nrow(sk))) {
        # match by TRIAL + ROW, but only rows still without data - once
        # the user fills the line in, it is no longer an unread loss
        hits <- which(as.character(d$TRIAL) == sk$TRIAL[s] &
                      as.character(d$ROW) == sk$ROW[s])
        hits <- hits[vapply(hits, function(r)
          all(is.na(d[r, dataCols])), logical(1))]
        for (r in hits) {
          key <- paste0(r - 1, "|", rowCol - 1)
          if (is.null(issPayload)) issPayload <- list()
          if (is.null(notePayload)) notePayload <- list()
          issPayload[[key]] <- "unreadable"
          notePayload[[key]] <- paste0(
            "The PDF reader saw this table line but could not use it: ",
            sk$reason[s])
        }
      }
    }
    # Per-row display precision (Steve, 2026-08-19): which columns are
    # "mean-like" (MEAN holds mean or median; Q1/Q3) vs "dispersion-like"
    # (SD/SE), and where their rounding declarations live. All indices
    # 0-based for the JS renderer.
    # the displayed frame keeps the UPLOADED names (normalization happens
    # inside validateData), so match loosely: case-insensitive, and the
    # rounding columns under both their underscore and space spellings
    un <- toupper(trimws(names(d)))
    ri <- function(...) {
      i <- match(c(...), un)
      i <- i[!is.na(i)]
      if (length(i) == 0) NULL else i[1] - 1
    }
    roles <- list()
    for (nm in c("MEAN", "Q1", "Q3"))
      if (!is.null(ri(nm))) roles[[as.character(ri(nm))]] <- "mean"
    for (nm in c("SD", "SE"))
      if (!is.null(ri(nm))) roles[[as.character(ri(nm))]] <- "disp"
    roundFmt <- list(mean = ri("ROUND_MEAN", "ROUND MEAN"),
                     disp = ri("ROUND_DISPERSION", "ROUND DISPERSION"),
                     roles = roles)
    w <- rhandsontable::rhandsontable(
      d,
      cellIssues = issPayload,
      cellNotes = notePayload,
      roundFmt = roundFmt,
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
    # Issue-13 painting renderer: delegate to the type-appropriate base
    # renderer (numeric keeps its numericFormat pattern), then color the
    # background if this cell is in the issue map. Applied per column so
    # the format specs above stay effective; row/column highlight is
    # class-based and unaffected.
    paintJS <- paste0(
      "function(instance, td, row, col, prop, value, cellProperties) {",
      "  var base = cellProperties.type === 'numeric' ?",
      "    Handsontable.renderers.NumericRenderer :",
      "    Handsontable.renderers.TextRenderer;",
      "  base.apply(this, arguments);",
      # Display precision follows the row's declared rounding (Steve,
      # 2026-08-19): MEAN/Q1/Q3 show ROUND_MEAN decimals; SD/SE show
      # ROUND_DISPERSION's, falling back to ROUND_MEAN. A blank rounding
      # cell leaves the value as typed (the underlying datum is never
      # altered - this is display only, and editing a cell still opens
      # the raw value).
      "  var rf = instance.params ? instance.params.roundFmt : null;",
      "  if (rf && rf.roles && value !== null && value !== '' &&",
      "      isFinite(value)) {",
      "    var role = rf.roles[col];",
      "    if (role) {",
      "      var digits = null;",
      "      var grab = function(ci) {",
      "        if (ci == null) return null;",
      "        var v = instance.getDataAtCell(row, ci);",
      "        return (v === null || v === '' || !isFinite(v)) ? null",
      "                                                        : Number(v);",
      "      };",
      "      if (role === 'mean') digits = grab(rf.mean);",
      "      else { digits = grab(rf.disp);",
      "             if (digits === null) digits = grab(rf.mean); }",
      "      if (digits !== null && digits >= 0 && digits <= 8)",
      "        td.textContent = Number(value).toFixed(digits);",
      "    }",
      "  }",
      "  var iss = instance.params ? instance.params.cellIssues : null;",
      "  if (iss) {",
      "    var key = row + '|' + col;",
      "    var code = iss[key];",
      "    var help = {",
      "      missing: 'A required value is missing. Enter it, or delete ",
                       "the row.',",
      "      unreadable: 'This could not be read as a number.',",
      "      incongruent: 'This value conflicts with the type of the ",
                          "row.',",
      "      derived: 'The parser computed this value (a percent ",
      "               converted to a count, or a recovered arm N). ",
      "               OK to use, but best to check it before the ",
      "               analysis runs.'};",
      "    if (code === 'missing') td.style.background = '#fff3b0';",
      "    else if (code === 'unreadable') td.style.background = '#f4b6b6';",
      "    else if (code === 'incongruent') td.style.background = '#b8d0f0';",
      "    else if (code === 'derived') td.style.background = '#d7f0d7';",
      "    if (code) {",
      "      var notes = instance.params.cellNotes;",
      "      td.title = (notes && notes[key]) ? notes[key] : help[code];",
      "    }",
      "  }",
      "}")
    measureCols <- intersect(c("MEAN", "SD", "SE"), names(d))
    for (nm in names(d)) {
      v <- d[[nm]]
      if (!is.numeric(v)) {
        w <- rhandsontable::hot_col(w, nm, renderer = paintJS)
        next
      }
      if (nm %in% measureCols) {
        w <- rhandsontable::hot_col(w, nm, format = "0.[00000]",
                                    renderer = paintJS)
      } else if (all(is.na(v) | v %% 1 == 0)) {
        # N, ROUND_MEAN, ROUND_DISPERSION, ROUND_OBSERVATION, category
        # counts - anything whole-numbered
        w <- rhandsontable::hot_col(w, nm, format = "0",
                                    renderer = paintJS)
      } else {
        w <- rhandsontable::hot_col(w, nm, format = "0.[00000]",
                                    renderer = paintJS)
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
    rIssues(NULL)   # revalidation will re-derive the cell-issue colors
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
    rIssues(NULL)   # fresh empty table - no issue colors yet
    parseSkips(NULL)
    parseDerived(NULL)
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
      countUsage("analyze")   # anonymous count; no-op outside production
      output$stopButton <- NULL
      progress <- shiny::Progress$new(session, style = "notification")
      on.exit(progress$close())
      DATA <<- reactiveDataValidated()
      # FIX: results from any previous run are discarded here. OUTPUT was
      # never reset, so analyzing a second file (or re-analyzing) in the same
      # session appended new results to the old ones in the downloaded
      # spreadsheet.
      OUTPUT <<- NULL
      # Distribution graphs (issue 16): a fresh collector per run; each
      # simulated row deposits its expected-distribution draws here.
      # Collection always runs (8 KB per row); the PowerPoint itself is
      # built only at download time, and only when the "Graph results"
      # box is checked - so the box can be ticked AFTER the analysis.
      graphsData <<- newGraphCollector()
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
          P_Calc(TRIAL, DATA, CategoryNames, m, graphs = graphsData)
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
      rIssues(NULL)   # new upload - stale issue colors must not carry over
      # parseSkips is NOT cleared here: uploads APPEND (PR #25), so skip
      # rows from earlier uploads remain in the table and must keep their
      # red ROW cell and reason. The blank-table reset clears it.

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

      # Zipped upload (Steve's request, 2026-08-20): an investigator
      # reproducing an analysis like Carlisle's 2012 Fujii review zips
      # every trial file and uploads ONE archive. Each entry becomes an
      # ordinary uploaded file here - same pipeline, same TRIAL-per-file
      # naming. Extraction is defensive (see R/zipUpload.R: junkpaths
      # into fresh directories, traversal names refused, entry and size
      # caps, no nested archives); extracted files land under tempdir(),
      # so the purge guarantee covers them like any direct upload.
      files <- expandZipUploads(files)
      uploadedPaths <<- unique(c(uploadedPaths, files$datapath))

      bad <- !files$ext %in% c("csv", "xlsx", "xls", "pdf", "docx")
      for (nm in files$name[bad])
        outputComments(paste0(nm, " is not a supported file type."))
      files <- files[!bad, , drop = FALSE]
      if (nrow(files) == 0) return()

      frames <- list()

      # Sequential uploads APPEND (Steve, 2026-08-19): the current table
      # - including any edits typed into the grid but not yet
      # revalidated - becomes the first "frame", so a later upload adds
      # rows instead of replacing everything. The existing combining
      # machinery then handles column union and trial disambiguation
      # exactly as it does across files in one selection (the current
      # table's trials are seeded first, so a clashing new file gets the
      # "filename: " prefix, never the other way around). Start With an
      # Empty Table remains the way to start over. All-empty rows (the
      # blank starter's untyped placeholders) are dropped first.
      prior <- currentGrid()
      if (!is.null(prior) && nrow(prior) > 0) {
        keep <- apply(prior, 1, function(r)
          any(!is.na(r) & trimws(as.character(r)) != ""))
        prior <- prior[keep, , drop = FALSE]
        if (nrow(prior) > 0)
          frames[[1]] <- list(stem = "Existing table", data = prior)
      }
      nPrior <- length(frames)

      readSheet <- function(path, ext) {
        if (ext == "csv")  return(read.csv(path))
        if (ext == "xlsx") return(read.xlsx(path))
        # FIX (from the single-file code): read.xl() never existed;
        # readxl::read_excel() is the reader, as.data.frame() because a
        # tibble's [,col] semantics break the column handling downstream.
        as.data.frame(read_excel(path))
      }
      # NOTE this is an EXCLUSION list, not a whitelist: everything that
      # survived the allowlist above and is not a parsed-document type
      # falls into readSheet(). A new document format must be excluded
      # here too, or it lands in the spreadsheet readers (issue 19).
      for (i in which(!files$ext %in% c("pdf", "docx"))) {
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

      # Article PDFs and Word manuscripts share one parsing route: a
      # subprocess per file with an OS timeout (poppler hangs on ~2% of
      # real PDFs; a crafted docx can stall libxml2 - the author of a
      # manuscript under investigation is the threat model), engine
      # dispatch by extension inside parseBaselineTableHeuristics().
      pdfIdx <- which(files$ext %in% c("pdf", "docx"))
      if (length(pdfIdx) > 0) {
        progress <- shiny::Progress$new(session, style = "notification")
        progress$set(message = "Parsing document(s) ",
                     detail = paste0(length(pdfIdx),
                                     " file(s), up to 60 s each"))
        res <- parseBaselineTableFiles(files$datapath[pdfIdx],
                                       ai = "never", timeout = 60,
                                       quiet = TRUE,
                                       pctApprox = isTRUE(input$pctApprox))
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
            # for a .docx, `page` is the table's ordinal in the document
            if (identical(r$layout, "docx"))
              paste0(": table ", res$page[k], " of the document, ")
            else paste0(": table page ", res$page[k], ", "),
            res$arms[k], " arm(s) (",
            res$armsWithN[k], " with N), ", res$variables[k],
            " variable(s), ", res$continuous[k], " with mean and SD."))
          d <- r$data
          d$TRIAL <- files$stem[i]   # opaque temp name -> the user's name
          # Table lines the parser could not use become GRID ROWS - the
          # label in ROW, everything else empty - instead of log text
          # (Steve's direction, 2026-08-19). Their ROW cells paint red
          # with the parser's reason on hover (see parseSkips above),
          # their required cells paint yellow, and validation leaves them
          # out of the analysis until the user fills them in or deletes
          # them.
          if (nrow(r$skipped) > 0) {
            extra <- d[rep(NA_integer_, nrow(r$skipped)), , drop = FALSE]
            extra$TRIAL <- files$stem[i]
            extra$ROW <- r$skipped$label
            rownames(extra) <- NULL
            d <- rbind(d, extra)
          }
          # Green-cell registry input: cells whose values the parser
          # derived (percent conversions per row/column, and the N column
          # when arm sizes were recovered rather than printed).
          derived <- r$derivedCells
          if (!is.null(r$armNSource) && any(!is.na(r$armNSource))) {
            nNote <- paste0("arm N recovered by the parser: ",
                            paste(unique(r$armNSource[!is.na(r$armNSource)]),
                                  collapse = " | "))
            derived <- rbind(derived,
                             data.frame(ROW = "*", COL = "N", KIND = "recovered",
                                        NOTE = nNote, stringsAsFactors = FALSE))
          }
          frames[[length(frames) + 1]] <-
            list(stem = files$stem[i], data = d,
                 skips = if (nrow(r$skipped) > 0) r$skipped else NULL,
                 derived = derived)
        }
      }

      if (length(frames) == nPrior) {
        # no NEW file produced a table; leave the existing table alone
        outputComments(if (nPrior > 0) paste(
          "No uploaded file produced a usable table; the existing table",
          "is unchanged.")
        else paste(
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
      if (nPrior > 0) {
        outputComments(paste0(
          "Appended ", length(frames) - nPrior, " file(s) to the ",
          "existing table: now ", nrow(DATA), " rows, ",
          length(unique(DATA$TRIAL)), " trial(s)."))
      } else if (length(frames) > 1) {
        outputComments(paste0("Combined ", length(frames), " file(s): ",
                              nrow(DATA), " rows, ",
                              length(unique(DATA$TRIAL)), " trial(s)."))
      }
      # Register the parser's skipped lines under each frame's FINAL
      # trial value (disambiguation above may have prefixed it) so the
      # grid renderer can find and paint their rows. With appending
      # uploads (PR #25) the registry ACCUMULATES: skip rows from
      # earlier uploads are still in the table, so their entries must
      # survive this upload (an entry whose row was since fixed or
      # deleted simply stops matching); unique() keeps any
      # re-registration harmless.
      skipReg <- do.call(rbind, lapply(frames, function(f) {
        if (is.null(f$skips)) return(NULL)
        data.frame(TRIAL = as.character(f$data$TRIAL[1]),
                   ROW = f$skips$label, reason = f$skips$reason,
                   stringsAsFactors = FALSE)
      }))
      parseSkips(unique(rbind(parseSkips(), skipReg)))
      deriveReg <- do.call(rbind, lapply(frames, function(f) {
        if (is.null(f$derived) || nrow(f$derived) == 0) return(NULL)
        data.frame(TRIAL = as.character(f$data$TRIAL[1]),
                   ROW = f$derived$ROW, COL = f$derived$COL,
                   note = paste0(
                     ifelse(f$derived$KIND == "approximate",
                            "APPROXIMATE - ", "Derived by the parser - "),
                     f$derived$NOTE,
                     ". OK to use, but best to check against the paper ",
                     "before the analysis runs."),
                   stringsAsFactors = FALSE)
      }))
      parseDerived(unique(rbind(parseDerived(), deriveReg)))
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
      # Issue 13: publish the cell-issue map (colors) from this pass -
      # including the soft warnings a successful pass can carry.
      rIssues(if (!is.null(v$issues) && nrow(v$issues) > 0) v$issues
              else NULL)
      if (v$FAIL)
      {
        # Show the NORMALIZED frame the issues index into (validateData
        # returns it pre-sort). skipValidation prevents this reactive
        # write from re-triggering validation on the same data.
        if (!is.null(v$DATA))
        {
          skipValidation <<- TRUE
          reactiveData(v$DATA)
        }
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
          tagList(
            # Issue 16 (Steve's design, 2026-08-20): PowerPoint graphs
            # are prepared only when asked for. Checked -> Download
            # Results delivers a zip (workbook + Graphs.pptx);
            # unchecked -> the bare xlsx, exactly as before.
            checkboxInput("graphResults",
                          "Graph results (adds a PowerPoint of actual vs expected distributions)",
                          value = FALSE, width = "100%"),
            downloadButton("download", "Download Results")
          )
          })
      }
    }
  )

  output$download <- downloadHandler(
    filename = function() {
      paste0("Integrity Analysis.",
             format(Sys.time(), format = "%y%m%d-%H%M%S"),
             if (isTRUE(input$graphResults)) ".zip" else ".xlsx")
    },
    content = function(file) {
      # Three tabs (Steve's design, 2026-08-19): Test Results (the sheet
      # exactly as before), Baseline Tables (journal-style
      # reconstructions of what was analyzed), Summary (one line per
      # study: name, combined P, Monte Carlo interval). Writer in
      # R/baselineTable.R.
      if (!isTRUE(input$graphResults)) {
        writeResultsWorkbook(OUTPUT, reactiveDataValidated(),
                             CategoryNames, file)
        return(invisible(NULL))
      }
      # Graph results checked (issue 16): the same workbook plus the
      # PowerPoint of actual-vs-expected distributions, zipped. Both are
      # staged under tempdir() (purged with the session); zip::zip with
      # root keeps the archive flat.
      #
      # Steve's review of PR #46: a big analysis builds a big deck (a
      # Fujii-sized run is ~170 slides, tens of seconds), and a silent
      # button invites a second click and a second build. So: the
      # button greys out for the duration, and the same progress
      # notification the app uses for parsing and analysis counts the
      # slides. Both are restored/closed even if the build errors.
      shinyjs::disable("download")
      progress <- shiny::Progress$new(session, style = "notification")
      on.exit({ progress$close(); shinyjs::enable("download") },
              add = TRUE)
      progress$set(message = "Preparing results ", value = 0,
                   detail = "workbook")
      stage <- file.path(tempdir(),
                         paste0("dl", format(Sys.time(), "%H%M%OS3")))
      dir.create(stage)
      on.exit(unlink(stage, recursive = TRUE), add = TRUE)
      xf <- file.path(stage, "Integrity Analysis Results.xlsx")
      pf <- file.path(stage, "Integrity Analysis Graphs.pptx")
      writeResultsWorkbook(OUTPUT, reactiveDataValidated(),
                           CategoryNames, xf)
      writeGraphsPptx(OUTPUT, graphsData, pf,
                      progress = function(done, total)
                        progress$set(value = done / total,
                                     detail = paste0("graph slide ", done,
                                                     " of ", total)))
      progress$set(value = 1, detail = "zipping")
      zip::zip(file, files = basename(c(xf, pf)), root = stage,
               mode = "cherry-pick")
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

  # Journal-style reconstructed baseline table (issue 15, Steve
  # 2026-08-17, implemented 2026-08-19): available whenever validation
  # has succeeded (reactiveDataValidated is cleared by every upload /
  # edit preamble, so the button hides itself when the table on screen
  # is no longer the validated one). One xlsx sheet per trial; the
  # reconstruction logic lives in R/baselineTable.R.
  output$journalButton <- renderUI({
    if (is.null(reactiveDataValidated())) return(NULL)
    tagList(downloadButton("journalTable",
                           "Download Baseline Table (journal view)"),
            HTML("<br><br>"))
  })
  output$journalTable <- downloadHandler(
    filename = function() {
      paste0("Baseline Table.",
             format(Sys.time(), format = "%y%m%d-%H%M%S"), ".xlsx")
    },
    content = function(file) {
      writeBaselineTablesXlsx(
        buildBaselineTables(reactiveDataValidated(), CategoryNames),
        file)
    })

  # No documentation download handler: the sidebar now LINKS to
  # https://integrityanalysis.io/guide.html (Steve's request,
  # 2026-08-19). The page is the very same file - the Markdown master is
  # docs/user-guide.md, pandoc renders it to
  # inst/extdata/IntegrityAnalysis.html (regeneration command in the
  # .md's header comment and AGENTS.md), and .github/workflows/pages.yaml
  # publishes that file as /guide.html.

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
