
###############################
# Server                      #
###############################
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
  reactiveDone <- reactiveVal()
  output$downloadButton <- NULL
  output$ProgressBar <- NULL
  lastTrial <- NULL

  loopTrials <- ExtendedTask$new(function(TRIAL) {
    future_promise(
      {
        P_Calc(TRIAL)
      },
      seed = TRUE
    )
  })

  # Add text so that download result button appears when available

  postMessage <- function(x)
  {
    priorMessage <<- paste(priorMessage, "<br>", x)
    output$message <- renderUI({
      HTML(priorMessage)
    })
  }

  is_category <- function(x) {
    # Remove NAs first for efficiency, then check if all values are integers
    
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

  P_Calc <- function(TRIAL)
  {
    PValues <- NULL
    data <- DATA[DATA$TRIAL == TRIAL,]
    RowIDs <- unique(data$ROW)
    
    for(Row in RowIDs)
    {
      cat ("Working on row: ", Row, "\n")
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
          SEMsample <- Meansd/sqrt(mean(ROWS$N))
          DiffSample <- sum((ROWS$MEAN - Meanmean)^2) # Squared difference of column means
  
          # Monte Carlo Simulation
          meansim <- dqrnorm(m,mean=Meanmean,sd=SEMsample) # Generate a new mean for each simulation
          MonteCarloMean <- matrix(NA, nrow = m, ncol = COLS) # I want one row for each simulation
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
                    rnorm(ROWS$N[i] * m, rep(meansim, ROWS$N[i]), Meansd),
                    nrow = m, byrow = FALSE
                  ),
                  ROWS$ROUND_OBSERVATION[i]
                )
              ),
              ROWS$ROUND_OBSERVATION[i]
            )
          N <- matrix(ROWS$N, nrow = m, ncol = COLS, byrow = TRUE)
          # Calculate the weighted mean, and then round
          MeanSamples <- rowsums (MonteCarloMean * N) / sum(ROWS$N)
          DiffSamples <- rowsums((MonteCarloMean - MeanSamples)^2)
          P <- sum(DiffSamples < DiffSample, DiffSamples <= DiffSample)/m/2
        } else {
          ROWS <- ROWS[,CategoryNames]
          for (NAME in CategoryNames)
          {
            if (all(is.na(ROWS[,NAME])))
              ROWS[,NAME] <- NULL
          }
          P <- chisq.test(ROWS, simulate.p.value=m)$p.value      
        }
        if(P == 1) P <- 0.999
        if(P == 0) P <- 0.001
        
        DATA$P[DATA$TRIAL == TRIAL & DATA$ROW == Row] <<- signif(P, 4)
        PValues <- c(PValues, P)
      }
    }
      
    if (length(PValues) > 1)
    {
      P <- sumz(PValues)$p
    } else {
      P <- PValues
    }

    if(P > 0.5)
    {
      P <- 1 - P
    }
    P <- P * 2
    DATA$P[DATA$TRIAL == TRIAL & DATA$ROW == "OVERALL P"] <<- signif(P,4)
    
    # Additional assessment of fabricated values
    test.vector <- c(unlist(data$MEAN), unlist(data$SD))
    test.vector <- test.vector[!is.na(test.vector)]
    
    # Benford's law test
    DATA$P[DATA$TRIAL == TRIAL & DATA$ROW == "Benford P"] <<- signif(
      distr.test(test.vector, check = 'first', reference = 'benford')$p.value,
      4)
    
    # Repeated digits test
    DATA$P[DATA$TRIAL == TRIAL & DATA$ROW == "Repeats P"] <<- signif(
      rv.test(test.vector, B = 2000)$p.value,
      4)

    cat(paste0("Trial ", TRIAL,": p = ",round(P, 3), "\n"))
    if (TRIAL == lastTrial) reactiveDone(TRUE)
    return(P)
  }

  observeEvent(
  {
    input$upload
  },
  {
    reactiveResults(NULL)
    priorMessage <<- NULL
    reactiveDone(FALSE)
    lastTrial <<- NULL

    Filename <- input$upload$datapath
    ext <- tools::file_ext(Filename)

    # Switch statement started to fail parsing... ????
    if (!ext %in% c("csv", "xlsx", "xls"))
    {
      postMessage(
        paste0(".",ext,"is not a supported file type."
        )
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
      DATA <<- read.xl(Filename)
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
    postMessage(paste("Column names:", paste(ColumnNames, collapse = ", ")))

    # Add trial number if necessary
    TRIALS <- grep("TRIAL", ColumnNames)
    if (length(TRIALS) == 0)
    {
      DATA$TRIAL <- 1
    }
    names(DATA)[TRIALS[1]] <- "TRIAL"
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
      postMessage("Missing column labeled ROW")
      FAIL <- TRUE
    } else {
      names(DATA)[RowColumn[1]] <- "ROW"
      ColumnNames <- names(DATA)
    }
    
    if (is.null(DATA$N))
    {
      postMessage("Missing column labeled N")
      FAIL <- TRUE
    }
    
    if (is.null(DATA$MEAN))
    {
      postMessage("Missing column labeled MEAN")
      FAIL <- TRUE
    }
    if (is.null(DATA$SD))
    {
      postMessage("Missing column labeled SD")
      FAIL <- TRUE
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
    CategoryNames <-
      ColumnNames[!ColumnNames %in% c("TRIAL", "ROW", "MEAN","N", "SD", "ROUND_OBSERVATION", "ROUND_MEAN")]
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
      postMessage(paste("Category Names", paste(CategoryNames, collapse=", "), "\n"))
    }
    
    # Validate each line
    for (i in 1:nrow(DATA))
    {
      if (any(!is.na(DATA[i, CategoryNames]))) # If there is any category entry, continuous columns are set to NA
      {
        DATA$ROUND_MEAN[i] <- DATA$ROUND_OBSERVATION[i] <- NA
        if (any(!is.na(DATA[i, c("N", "MEAN", "SD")])))
        {
          postMessage(paste("Please look at line", i+1))
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
          postMessage(paste("This appears to be a category. However, it has entries for continuous variables."))
          postMessage(paste("Specifically: ", message))
          FAIL <- TRUE
        }
      } else {
        if (any(is.na(DATA[i, c("N", "MEAN", "SD")])))
        {
          postMessage(paste("Please look at line", i+1))
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
          postMessage(paste("This appears to be a continuous variable. However, it has NA entries for required fields."))
          postMessage(paste("Specifically: ", message))
          FAIL <- TRUE
        }
      # Fix MEAN digits if Mean has any decimal digits
        if (DATA$MEAN[i] != as.integer(DATA$MEAN[i]))
        {
          digits <- nchar(sub("^.*\\.", "", as.character(DATA$MEAN[i])))
          if (DATA$ROUND_MEAN[i] < digits) DATA$ROUND_MEAN[i] <- digits
        }
      }
    }
    if (FAIL)
    {
      postMessage("There are one or more errors in the data table. Please review the above messages to address these.")
      return()
    }
    DATA$P <- NA
    DATA <- DATA[,c("TRIAL", "ROW", "P", "N", "MEAN", "SD",  "ROUND_MEAN", "ROUND_OBSERVATION", CategoryNames, MiscNames)]
    DATA$SEQ <- 1:nrow(DATA)
    TRIALS <- unique(DATA$TRIAL)

    pDATA <- DATA[1:length(TRIALS),]
    pDATA[,] <- NA
    blankDATA <- benfordDATA <- rvDATA <- pDATA
    blankDATA$TRIAL <- pDATA$TRIAL <- benfordDATA$TRIAL <- rvDATA$TRIAL <- TRIALS
    
    pDATA$ROW <- "OVERALL P"
    pDATA$SEQ <- 999997
    
    # Benford's law
    benfordDATA$ROW <- "Benford P"
    benfordDATA$SEQ <- 999998
    
    # Repeated digits
    rvDATA$ROW <- "Repeats P"
    rvDATA$SEQ <- 999999
    
    blankDATA$SEQ <- 1000000
    
    DATA <- rbind(DATA, pDATA, benfordDATA, rvDATA, blankDATA)
    DATA <- DATA[order(DATA$TRIAL, DATA$SEQ),]
    DATA$TRIAL[DATA$SEQ == 1000000] <- ""
    DATA <- DATA[-nrow(DATA),]
    DATA$SEQ <- NULL

    # Assign globally
    TRIALS <<- TRIALS
    ColumnNames <<- ColumnNames
    CategoryNames <<- CategoryNames
    DATA <<- DATA

    # Set reactive value
    reactiveDataValidated(DATA)
    lastTrial <<- DATA$TRIAL[nrow(DATA)]
  }
  )

  observeEvent(
    {
      reactiveDataValidated()
    },
    {
      LengthTrials <- length(TRIALS)
      
      # output$ProgressBar <- renderUI({
      #   progressBar(
      #   id = "pb",
      #   value = 0,
      #   total = LengthTrials,
      #   title = "",
      #   display_pct = FALSE
      # )
      # })
      # 
      
      start_time <- Sys.time()
      DATA <<- reactiveDataValidated()
      # Set up results array

      # Main Processing Loop
      for (i in 1:LengthTrials)
      {
        TRIAL <- TRIALS[i]
        # updateProgressBar(
        #   session = session,
        #   id = "pb",
        #   value = i, total = LengthTrials,
        #   title = paste("Trial:", TRIAL)
        # )
        # Sys.sleep(0.1)
        cat("Working on trial: ", TRIAL, "\n")
        shinycssloaders::showPageSpinner(P_Calc(TRIAL), caption = paste0(i, "/", LengthTrials))
        #loopTrials$invoke(TRIAL)
      }
      end_time <- Sys.time()
      postMessage(paste("Execution time", round(end_time - start_time, 2)))
      reactiveResults(DATA)
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
      write.xlsx(DATA, file)
    })

  output$documentation <- downloadHandler(
    filename = function() {
      "IntegrityAnalysis.docx"
    },
    content = function(file) {
      file.copy("IntegrityAnalysis.docx", file)
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

     

