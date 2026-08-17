# validateData.R — the upload validation pipeline.
#
# PROVENANCE: moved out of app_server() in phase 2 of the package
# restructure (Claude Code, model Claude Fable 5, 2026-08-16). The body of
# the reactiveData() observer became validateData(); is_category() came
# with it. One deliberate change, verified bit-identical under fixed seeds
# (see the phase-2 PR): instead of mutating the server's session state with
# <<-, validateData() RETURNS everything it derives and app_server assigns.
# outputComments() still reports line-by-line problems to the session log -
# it recovers the active session itself, so being called from an ordinary
# function changes nothing. Every FIX comment travels with its code.

#' Is a column a category (count) column?
#'
#' A category column is numeric, integer-valued, and has at least one NA
#' (the NA rows are where the trial's continuous variables live).
#'
#' @param x a column of the uploaded table.
#' @return `TRUE` if the column should be treated as categorical counts.
#' @noRd
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

#' Validate an uploaded baseline-data table
#'
#' Normalizes column names (TRIAL / ROW / N / MEAN / SD, the Carlisle-2016
#' aliases, ROUND_MEAN and ROUND_OBSERVATION), coerces the numeric columns,
#' identifies category columns with [is_category()], and checks every line
#' (continuous rows need N, MEAN and SD; category rows must not carry
#' continuous entries). Problems are reported line by line through
#' [outputComments()], which finds the active Shiny session on its own.
#'
#' @param DATA the raw uploaded data.frame.
#' @return a list: `FAIL` (logical), and on success the validated `DATA`
#'   (columns selected and ordered), `TRIALS`, `ColumnNames`,
#'   `CategoryNames`, `MiscNames`. On failure only `FAIL` is meaningful.
#' @noRd
validateData <- function(DATA) {
  FAIL <- FALSE

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
  # the user line by line instead of crashing. Q1/Q3 and SE included
  # (2026-08-17, median/IQR support).
  for (col in c("N", "MEAN", "SD", "SE", "Q1", "Q3"))
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
  # Q1/Q3 (median/IQR rows, 2026-08-17) join SE and ROUND_DISPERSION on
  # the excluded list for the same reason: integer-valued quartiles with
  # NAs elsewhere would otherwise be swallowed as category columns.
  CategoryNames <-
    ColumnNames[!ColumnNames %in% c("TRIAL", "ROW", "MEAN","N", "SD", "SE",
                                    "Q1", "Q3",
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
      if (any(!is.na(DATA[i, intersect(c("N", "MEAN", "SD", "Q1", "Q3"),
                                       names(DATA))])))
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
    } else if (("Q1" %in% names(DATA) && !is.na(DATA$Q1[i])) ||
               ("Q3" %in% names(DATA) && !is.na(DATA$Q3[i]))) {
      # Median/IQR row (Steve's design, 2026-08-17): quartiles present
      # mean the MEAN column holds the MEDIAN. Both quartiles, N, and the
      # median are required; SD/SE must be EMPTY (a row carrying both an
      # SD and quartiles is ambiguous about what MEAN means); and the
      # median must sit between its quartiles (non-strict - printed
      # rounding can tie them).
      hasQ1 <- "Q1" %in% names(DATA) && !is.na(DATA$Q1[i])
      hasQ3 <- "Q3" %in% names(DATA) && !is.na(DATA$Q3[i])
      if (!hasQ1 || !hasQ3)
      {
        outputComments(paste("Please look at line", i+1))
        outputComments(paste(
          "This row reports quartiles, but only one of Q1/Q3 is filled",
          "in. A median row needs both."))
        FAIL <- TRUE
      } else if (is.na(DATA$N[i]) || is.na(DATA$MEAN[i]))
      {
        outputComments(paste("Please look at line", i+1))
        outputComments(paste(
          "This is a median/IQR row (Q1 and Q3 are filled in), so it",
          "needs N and the median in the MEAN column."))
        FAIL <- TRUE
      } else if (!is.na(DATA$SD[i]) ||
                 ("SE" %in% names(DATA) && !is.na(DATA$SE[i])))
      {
        outputComments(paste("Please look at line", i+1))
        outputComments(paste(
          "This row has quartiles AND an SD or SE. With Q1/Q3 filled in,",
          "MEAN is read as the MEDIAN - remove either the quartiles or",
          "the SD/SE so the row is unambiguous."))
        FAIL <- TRUE
      } else if (DATA$Q1[i] > DATA$MEAN[i] || DATA$MEAN[i] > DATA$Q3[i])
      {
        outputComments(paste("Please look at line", i+1))
        outputComments(paste0(
          "The median must lie between its quartiles: Q1 = ", DATA$Q1[i],
          ", median = ", DATA$MEAN[i], ", Q3 = ", DATA$Q3[i], "."))
        FAIL <- TRUE
      } else {
        # median printed with decimals bumps ROUND_MEAN, same as a mean
        if (DATA$MEAN[i] %% 1 != 0)
        {
          digits <- nchar(sub("^.*\\.", "", as.character(DATA$MEAN[i])))
          if (DATA$ROUND_MEAN[i] < digits) DATA$ROUND_MEAN[i] <- digits
        }
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
    return(list(FAIL = TRUE))
  }
  # Carry SE, Q1/Q3, and ROUND_DISPERSION through when the input supplies
  # them. They are optional: a spreadsheet typed by hand, or written
  # before these changes, has none of them, and must still work.
  OptionalColumns <- intersect(c("SE", "Q1", "Q3", "ROUND_DISPERSION"),
                               names(DATA))
  DATA <- DATA[,c("TRIAL", "ROW", "N", "MEAN", "SD",  "ROUND_MEAN", "ROUND_OBSERVATION", OptionalColumns, CategoryNames, MiscNames)]
  DATA <- DATA[order(DATA$TRIAL, DATA$ROW),]
  TRIALS <- unique(DATA$TRIAL)

  list(FAIL = FALSE, DATA = DATA, TRIALS = TRIALS,
       ColumnNames = ColumnNames, CategoryNames = CategoryNames,
       MiscNames = MiscNames)
}
