
###############################
# Server                      #
###############################
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


server <- function(input, output, session) {
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
  
  is_category <- function(x) {
    # Remove NAs first for efficiency, then check if all values are integers

    # FIX: text columns (e.g. a comments column) previously crashed the app:
    # as.integer() on character data yields NA, all() then returns NA, and
    # if (!NA) is a fatal error ("missing value where TRUE/FALSE needed").
    # A non-numeric column can never be a category (categories are counts).
    if (!is.numeric(x))
      return(FALSE)

    # If there are no na values, then it can't be a category
    if (sum(is.na(x)) == 0)
      return(FALSE)

    # If the vector is empty after removing NAs then it is not a category
    x_clean <- x[!is.na(x)]
    if (length(x_clean) == 0) 
      return(FALSE) 

    # Check if all values are equal to their integer representation
    all(x_clean == as.integer(x_clean))
  }

  ###########################################################
  # Primary Statistical Function for Monte Carlo Simulation #
  ###########################################################
  
  P_Calc <- function(TRIAL)
  {
    data <- DATA[DATA$TRIAL == TRIAL,]
    RowIDs <- unique(data$ROW)
    
    x <- foreach(
      j = 1:length(RowIDs), 
      .combine = rbind
      #  .options.future = list(seed = TRUE)
      #.export = c("CategoryNames", "m"),
      #.packages = c("Rfast", "dqrng")
    ) %do%
      {
        Row <- RowIDs[j]
        ROWS <- data[data$ROW == Row,]
        
        # Greater than 1 line?
        if (nrow(ROWS) > 1)
        {
          # Is this categorical?
          if (all(!is.na(ROWS$N)))
          {
            COLS <- nrow(ROWS)
            N <- sum(ROWS$N)
            Meanmean <- sum(ROWS$N*ROWS$MEAN) / N
            # The calculation of Meanvar is OK. SD^2 is an unbiased estimate
            # of variance
            Meanvar <-  sum(ROWS$N*ROWS$SD^2) / N
            
            # However, this next calculatiion is biased. s.u. will correct it
            # If N > 30, then the correction is < 1 %. It blows up if N > 343! 
            if (N < 30)
            {
              Meansd <- s.u(sqrt(Meanvar), N)
            } else {
              Meansd <- sqrt(Meanvar)
            }
            # Protect size of simulation
            if ((m*N) < 1000000000) # One billion
            {
              m1 <- m
            } else {
              # FIX: floor() added. 1e9/N is rarely a whole number, and a
              # fractional replication count silently mis-sizes the matrix
              # dimensions below.
              m1 <- floor(1000000000 / N)
            }
            SEMsample <- Meansd/sqrt(mean(ROWS$N))
            DiffSample <- sum((ROWS$MEAN - Meanmean)^2) # Squared difference of column means
            # Monte Carlo Simulation
            # FIX: was dqrnorm(m, ...). meansim must have exactly m1 entries
            # (one simulated "true" mean per replication row). When N was
            # large enough that m1 < m, the extra entries misaligned the
            # column-major matrix fill below, so within one replication the
            # study arms were simulated from DIFFERENT true means - breaking
            # the null hypothesis the simulation is supposed to represent.
            meansim <- dqrnorm(m1,mean=Meanmean,sd=SEMsample) # Generate a new mean for each simulation
            MonteCarloMean <- matrix(NA, nrow = m1, ncol = COLS) # I want one row for each simulation
            # Need to do each column separately. Couldn't think of an efficient way to do this without
            # a loop. 
            for (i in 1:COLS)
              MonteCarloMean[,i] <-
              round(
                rowmeans(
                  round(
                    # The matrix below will have one row for each replication (m rows),
                    # and one column for each person (N[i] columns)
                    # Cannot use dqrnorm because it won't support the array
                    # of meansim needed for each replication
                    matrix(
                      rnorm(ROWS$N[i] * m1, rep(meansim, ROWS$N[i]), Meansd),
                      nrow = m1, byrow = FALSE
                    ),
                    ROWS$ROUND_OBSERVATION[i]  # observations rounded as recorded
                  )
                ),
                # FIX: was ROWS$ROUND_OBSERVATION[i]. The simulated column
                # means must be rounded to the precision at which the
                # PUBLISHED means were reported (ROUND_MEAN) - that is the
                # value the validation code in the upload observer goes to
                # such lengths to derive, and it was never used. Using the
                # observation precision simulated the Monte Carlo
                # distribution at the wrong granularity whenever means are
                # reported more (or less) precisely than the observations.
                ROWS$ROUND_MEAN[i]
              )
            N <- matrix(ROWS$N, nrow = m1, ncol = COLS, byrow = TRUE)
            # Calculate the weighted mean, and then round
            MeanSamples <- rowsums (MonteCarloMean * N) / sum(ROWS$N)
            DiffSamples <- rowsums((MonteCarloMean - MeanSamples)^2)
            
            PEQ <- sum(DiffSamples == DiffSample) / m1
            PLE <- sum(DiffSamples < DiffSample)/m1 + PEQ
            PGE <- sum(DiffSamples > DiffSample)/m1 + PEQ
          } else {
            # FIX: drop = FALSE added. With a single category column,
            # ROWS[,CategoryNames] dropped to a bare vector and the
            # ROWS[,NAME] <- NULL loop below crashed with "incorrect number
            # of dimensions".
            ROWS <- ROWS[,CategoryNames, drop = FALSE]
            for (NAME in CategoryNames)
            {
              if (all(is.na(ROWS[,NAME])))
                ROWS[,NAME] <- NULL
            }
            # FIX: was chisq.test(ROWS, simulate.p.value = m). A numeric
            # simulate.p.value is simply treated as TRUE and the replicate
            # count stays at chisq.test's DEFAULT of 2000 - the m replicates
            # were never run. The replicate count is the separate B argument.
            PLE <- chisq.test(ROWS, simulate.p.value=TRUE, B=m)$p.value
            PGE <- 1-PLE
          }
          # Need to be sure P != 0 or 1
          if(PLE == 1) PLE <- 0.999
          if(PLE == 0) PLE <- 0.001
          if(PGE == 1) PGE <- 0.999
          if(PGE == 0) PGE <- 0.001
          PLE = as.character(signif(PLE,4))
          PGE = as.character(signif(PGE, 4))
        } else {
          PLE = "Only 1 Row"
          PGE = NA
        }
        
        c(as.character(Row), PLE, PGE)
      }
    # FIX: removed "%seed% TRUE" after the closing brace. %seed% is
    # doFuture's operator for seeding a %dofuture% loop; chained after %do%
    # it was applied to the already-computed result matrix, which is an
    # error. (Note dqrnorm draws from dqrng's own RNG stream; use
    # dqset.seed() if reproducible simulations are ever needed.)

    # This bizarre code is because if there is only 1 row, R creates a data.frame
    # with 3 columns and 1 row. 
    if (length(x) == 3)
    {
      x <- as.data.frame(t(x))
    } else {
    x <- as.data.frame(x)
    }
    
    x <- cbind(NA, x)
    x[1,1] <- TRIAL
    x <- as.data.frame(x)
    names(x) <- c("TRIAL", "ROW", "PLE", "PGE")
    # FIX: was x[match(x$ROW, RowIDs),] - the arguments were reversed, which
    # applies the INVERSE permutation. Harmless today only because %do%
    # returns results already in RowIDs order; it would silently scramble row
    # labels against p-values the day this loop is parallelized. To order x
    # by RowIDs: for each RowID, find its position in x$ROW.
    # (Also removed the leftover cat()/print() debugging output here.)
    x <- x[match(RowIDs, x$ROW),]

    PLEvalues <- as.numeric(x$PLE)
    PGEvalues <- as.numeric(x$PGE)
    
    PLEvalues <- PLEvalues[!is.na(PLEvalues)]
    PGEvalues <- PGEvalues[!is.na(PGEvalues)]

    if (length(PLEvalues) > 1)
    {
      PLE <- signif(sumz(PLEvalues)$p,4)
    } else {
      # FIX: was length(PLEvalues == 1), i.e. the length of a comparison
      # vector, not a comparison of the length. It worked by coincidence
      # (length 1 -> 1 -> truthy; length 0 -> 0 -> falsy) but was a trap.
      if (length(PLEvalues) == 1)
        PLE <- PLEvalues
      if (length(PLEvalues)==0)
        PLE = "No values"
    }

    if (length(PGEvalues) > 1)
    {
      PGE <- signif(sumz(PGEvalues)$p,4)
    } else {
      # FIX: same length(x == 1) -> length(x) == 1 typo as PLE above
      if (length(PGEvalues) == 1)
        PGE <- PGEvalues
      if (length(PGEvalues)==0)
        PGE = "No values"
    }

    lastline <- data.frame(
      TRIAL = c(NA, NA),
      ROW = c("Summary", NA),
      PLE = c(as.character(PLE), NA),
      PGE = c(as.character(PGE), NA)
    )

    x <- rbind(x, lastline)
    outputComments(
      paste0("Trial ", TRIAL,": p = ", PLE, "\n")
    )
    return(x)
  }

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
          P_Calc(TRIAL)
        )
        progress$set(
          value = i / LengthTrials,
          detail = paste0(TRIAL, ", P = ",OUTPUT$PLE[nrow(OUTPUT)-1]))
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

      Filename <- input$upload$datapath
      ext <- tools::file_ext(Filename)

      # Switch statement started to fail parsing... ????
      if (!ext %in% c("csv", "xlsx", "xls"))
      {
        outputComments(
          paste0(".", ext, " is not a supported file type.")  # FIX: spacing
        )
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
      FAIL <- FALSE
      DATA <- reactiveData()
      if (is.null(DATA))
      {
        return()
      }
      
      names(DATA) <- toupper(trimws(names(DATA)))
      ColumnNames <- names(DATA)
      outputComments(paste("Column names:", paste(ColumnNames, collapse = ", ")))
      
      # Add trial number if necessary
      # FIX: the rename is now inside an else branch. It previously ran
      # unconditionally, so with no TRIAL column present it indexed
      # names(DATA) with NA. (Also renamed the local from TRIALS to
      # TrialColumns: it held column indexes, not trial IDs, and shadowed
      # the session-level TRIALS list.)
      TrialColumns <- grep("TRIAL", ColumnNames)
      if (length(TrialColumns) == 0)
      {
        DATA$TRIAL <- 1
      } else {
        names(DATA)[TrialColumns[1]] <- "TRIAL"
      }
      ColumnNames <- names(DATA)
      
      ################################################
      # Adjust names to accept Carlisle 2016 input file
      MEASURES <- grep("MEASURE", ColumnNames)
      if (length(MEASURES) > 0)
      {
        names(DATA)[MEASURES[1]] <- "ROW"
        DATA$GROUP <- NULL
        DATA$DECSD <- NULL
        ColumnNames <- names(DATA)
      }
      DECMS <- grep("DECM", ColumnNames)
      if (length(DECMS) > 0)
      {
        names(DATA)[DECMS[1]] <- "ROUND_MEAN"
        ColumnNames <- names(DATA)
      }
      NUMBERS <-grep("NUMBER", names(DATA))
      if (length(NUMBERS) > 0)
      {
        names(DATA)[NUMBERS[1]] <- "N"
        ColumnNames <- names(DATA)
      }
      if (length(grep("ROW", ColumnNames)) == 0)
      {
        GROUPS <- grep("GROUP", ColumnNames)
        if (length(GROUPS)> 0)
        {
          names(DATA)[GROUPS[1]] <- "ROW"
        }
      }
      
      ColumnNames <- names(DATA)
      
      ##############################################
      
      # Verify that the necessary rows are in place
      RowColumn <- grep("ROW", ColumnNames)
      if (length(RowColumn) == 0)
      {
        outputComments("Missing column labeled ROW")
        FAIL <- TRUE
      } else {
        names(DATA)[RowColumn[1]] <- "ROW"
        ColumnNames <- names(DATA)
      }
      
      if (is.null(DATA$N))
      {
        outputComments("Missing column labeled N")
        FAIL <- TRUE
      }
      
      if (is.null(DATA$MEAN))
      {
        outputComments("Missing column labeled MEAN")
        FAIL <- TRUE
      }
      if (is.null(DATA$SD))
      {
        outputComments("Missing column labeled SD")
        FAIL <- TRUE
      }

      # FIX: force N, MEAN, and SD to numeric. Excel/CSV files with a stray
      # text cell make the whole column character, and character data in the
      # per-line checks below crashed the app (if (NA) errors). Coercion
      # turns non-numeric cells into NA, which those checks then report to
      # the user line by line instead of crashing.
      for (col in c("N", "MEAN", "SD"))
      {
        if (!is.null(DATA[[col]]) && !is.numeric(DATA[[col]]))
        {
          DATA[[col]] <- suppressWarnings(as.numeric(DATA[[col]]))
        }
      }

      # Add rounding column for the mean  
      MeanColumns <- grep("MEAN", ColumnNames)
      RoundMeanColumn <- which(ColumnNames[MeanColumns] != "MEAN")
      if (length(RoundMeanColumn) > 0)
      {
        names(DATA)[MeanColumns[RoundMeanColumn[1]]] <- "ROUND_MEAN"
        ColumnNames <- names(DATA)
      } else {
        if (!is.null(DATA$ROUND))
        {
          names(DATA)[names(DATA) == "ROUND"] <- "ROUND_MEAN"
        } else {
          ObservationColumns <- grep("OBS", ColumnNames)
          if (length(ObservationColumns) > 0)
          {
            names(DATA)[ObservationColumns[1]] <- "ROUND_OBSERVATION"
            DATA$ROUND_MEAN <- DATA$ROUND_OBSERVATION
          }
        }
      }
      # After all of that, if it still doesn't exist, just put in 0
      if (is.null(DATA$ROUND_MEAN))
      {
        DATA$ROUND_MEAN <- 0
      }
      ColumnNames <- names(DATA)
      
      ObservationColumns <- grep("OBS", ColumnNames)
      if (length(ObservationColumns) == 0)
      {
        DATA$ROUND_OBSERVATION <- DATA$ROUND_MEAN
      } else {
        names(DATA)[ObservationColumns[1]] <- "ROUND_OBSERVATION"
      }
      ColumnNames <- names(DATA)
      
      # Validate Categories
      #
      # SE and ROUND_DISPERSION are recognised columns, not categories. Papers
      # print a standard deviation or a standard error, never a variance, so
      # ParsePDF records whichever was printed in its own column and leaves the
      # conversion to us: it needs N, and the sample SD is a biased estimator
      # of sigma (Jensen's inequality), which is what s.u() below corrects.
      # ROUND_DISPERSION is the printed granularity of whichever value was
      # given, and cannot be inferred from ROUND_MEAN - a table may print
      # "39 (4.06)".
      #
      # They MUST be excluded here: is_category() calls any numeric column with
      # an NA and integer values a category, and ROUND_DISPERSION is exactly
      # that, so it would otherwise be analysed as a count column.
      CategoryNames <-
        ColumnNames[!ColumnNames %in% c("TRIAL", "ROW", "MEAN","N", "SD", "SE",
                                        "ROUND_OBSERVATION", "ROUND_MEAN",
                                        "ROUND_DISPERSION")]
      MiscNames <- NULL
      if (length(CategoryNames) == 0)
      {
        CategoryNames <- NULL
      } else {
        for (i in 1:length(CategoryNames))
        {
          if (!is_category(DATA[,CategoryNames[i]])) 
          {
            MiscNames <- c(MiscNames, CategoryNames[i])
            CategoryNames[i] <- "XXXXX"
          }
        }
        CategoryNames <- CategoryNames[CategoryNames != "XXXXX"]
      }
      
      if (length(CategoryNames) == 0)
      {
        CategoryNames <- NULL
      } else {
        outputComments(paste("Category Names", paste(CategoryNames, collapse=", "), "\n"))
      }
      
      # Validate each line
      for (i in 1:nrow(DATA))
      {
        if (any(!is.na(DATA[i, CategoryNames]))) # If there is any category entry, continuous columns are set to NA
        {
          DATA$ROUND_MEAN[i] <- DATA$ROUND_OBSERVATION[i] <- NA
          if (any(!is.na(DATA[i, c("N", "MEAN", "SD")])))
          {
            outputComments(paste("Please look at line", i+1))
            message <- NULL
            if (!is.na(DATA$N[i])) message <- paste(message, "N = ", DATA$N[i])
            if (!is.na(DATA$MEAN[i])) 
            {
              if (!is.null(message)) message <- paste0(message, ", ")
              message <- paste(message, "MEAN = ", DATA$MEAN[i])
            }
            if (!is.na(DATA$SD[i]))
            {
              if (!is.null(message)) message <- paste0(message, ", ")
              message <- paste(message, "SD = ", DATA$SD[i])
            }
            outputComments(paste("This appears to be a category. However, it has entries for continuous variables."))
            outputComments(paste("Specifically: ", message))
            FAIL <- TRUE
          }
        } else {
          if (any(is.na(DATA[i, c("N", "MEAN", "SD")])))
          {
            outputComments(paste("Please look at line", i+1))
            message <- NULL
            if (is.na(DATA$N[i])) message <- paste(message, "N = ", DATA$N[i])
            if (is.na(DATA$MEAN[i])) 
            {
              if (!is.null(message)) message <- paste0(message, ", ")
              message <- paste(message, "MEAN = ", DATA$MEAN[i])
            }
            if (is.na(DATA$SD[i]))
            {
              if (!is.null(message)) message <- paste0(message, ", ")
              message <- paste(message, "SD = ", DATA$SD[i])
            }
            outputComments(paste("This appears to be a continuous variable. However, it has NA entries for required fields."))
            outputComments(paste("Specifically: ", message))
            # A row carrying a standard error instead of a standard deviation
            # is a different problem from a row that is simply blank, and the
            # user can fix it - so say which it is rather than reporting a bare
            # "SD = NA". The conversion is deliberately NOT done here: it needs
            # N, and it is a decision about the analysis, not data entry.
            if ("SE" %in% names(DATA) && !is.na(DATA$SE[i]) && is.na(DATA$SD[i]))
              outputComments(paste0(
                "Line ", i + 1, " reports a standard error (SE = ", DATA$SE[i],
                "), not a standard deviation. The analysis needs an SD. ",
                "Enter the SD, or convert the SE yourself - the conversion ",
                "needs N and is a decision about the analysis, so it is not ",
                "made for you."))
            FAIL <- TRUE
          } else {
            # Fix MEAN digits if Mean has any decimal digits
            # FIX: two changes here.
            # (1) This block is now the else of the NA check above. It
            #     previously ran even for rows just flagged as having a
            #     missing MEAN, and if (NA != ...) is a fatal error - the
            #     user got a crash instead of the validation messages.
            # (2) The decimal test is now MEAN %% 1 != 0 rather than
            #     MEAN != as.integer(MEAN): as.integer() returns NA for
            #     values beyond +/-2^31 (e.g. large counts), which would
            #     also crash the if().
            if (DATA$MEAN[i] %% 1 != 0)
            {
              digits <- nchar(sub("^.*\\.", "", as.character(DATA$MEAN[i])))
              if (DATA$ROUND_MEAN[i] < digits) DATA$ROUND_MEAN[i] <- digits
            }
          }
        }
      }
      
      if (FAIL)
      {
        outputComments("There are one or more errors in the data table. Please review the above messages to address these.")
        return()
      }
      # Carry SE and ROUND_DISPERSION through when the input supplies them.
      # They are optional: a spreadsheet typed by hand, or written before this
      # change, has neither, and must still work.
      OptionalColumns <- intersect(c("SE", "ROUND_DISPERSION"), names(DATA))
      DATA <- DATA[,c("TRIAL", "ROW", "N", "MEAN", "SD",  "ROUND_MEAN", "ROUND_OBSERVATION", OptionalColumns, CategoryNames, MiscNames)]
      DATA <- DATA[order(DATA$TRIAL, DATA$ROW),]
      TRIALS <- unique(DATA$TRIAL)
      
      # Assign globally
      DATA <<- DATA
      TRIALS <<- TRIALS
      ColumnNames <<- ColumnNames
      CategoryNames <<- CategoryNames
      
      LengthTrials <- length(TRIALS)
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
      reactiveDataValidated(DATA)
      
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
      names(x) <- c("TRIAL", "ROW", "Fraction <=", "Fraction >=")
      write.xlsx(x, file)
    })

  output$documentation <- downloadHandler(
    filename = function() {
      "IntegrityAnalysis.pdf"
    },
    content = function(file) {
      file.copy("IntegrityAnalysis.pdf", file)
    })
  
  output$template <- downloadHandler(
    filename = function() {
      "Template for Integrity Analysis.xlsx"
    },
    content = function(file) {
      write.xlsx(read.xlsx("Template.xlsx"), file)
    })

    
    output$example <- downloadHandler(
    filename = function() {
      "Example for Integrity Analysis.xlsx"
    },
    content = function(file) {
      write.xlsx(read.xlsx("Example.xlsx"), file)
    })

  observeEvent(input$stop, {
    stopApp(returnValue = invisible())
  })
} 

     
