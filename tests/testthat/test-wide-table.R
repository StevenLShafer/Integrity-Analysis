# Issue 17: journal-style wide baseline tables as INPUT.
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-21,
# with R/parseWideTable.R. The acceptance test is Steve's design
# (2026-08-21): the Editor's View download this app generates must be
# valid input - generate from a validated frame, parse back, and the
# validated result must match the frame it came from. Fixture and
# comparison helpers live in helper-baselineTable.R.
suppressWarnings(suppressPackageStartupMessages({
  library(shiny); library(openxlsx); library(readxl); library(Rfast)
  library(foreach); library(MBESS); library(dqrng)
}))

stage <- function(src) {
  d <- file.path(tempdir(), paste0("wide", basename(tempfile(""))))
  dir.create(d)
  f <- file.path(d, basename(src))
  file.copy(src, f)
  f
}

# write a character matrix as an xlsx sheet, no headers, all text
writeRawXlsx <- function(m, file) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(wb, "Sheet1",
                      as.data.frame(m, stringsAsFactors = FALSE),
                      colNames = FALSE)
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  file
}

test_that("the Editor's View download round-trips (one sheet per trial)", {
  orig <- vdShared(wideFixtureTwoTrials())
  expect_false(orig$FAIL)
  tabs <- buildBaselineTables(orig$DATA, orig$CategoryNames)
  f <- tempfile(fileext = ".xlsx")
  writeBaselineTablesXlsx(tabs, f)

  blocks <- parseWideTable(f, "xlsx")
  expect_false(is.null(blocks))
  expect_length(blocks, 2)
  expect_identical(vapply(blocks, `[[`, character(1), "trial"),
                   c("A", "B"))
  # nothing the generator printed was unusable
  expect_identical(sum(vapply(blocks, function(b) nrow(b$skipped),
                              integer(1))), 0L)

  combined <- do.call(.ppRbindFill, lapply(blocks, `[[`, "data"))
  back <- vdShared(combined)
  expect_false(back$FAIL)
  expectWideRoundTrip(back$DATA, orig$DATA)
  # the category structure survived too
  expect_setequal(back$CategoryNames, orig$CategoryNames)
})

test_that("the results workbook's stacked Baseline Tables sheet round-trips", {
  orig <- vdShared(wideFixtureTwoTrials())
  dqrng::dqset.seed(42); set.seed(42)
  OUTPUT <- NULL
  for (tr in orig$TRIALS) {
    x <- suppressWarnings(shiny::isolate(
      P_Calc(tr, orig$DATA[orig$DATA$TRIAL == tr, ],
             orig$CategoryNames, 1000)))
    OUTPUT <- rbind(OUTPUT, x)
  }
  f <- tempfile(fileext = ".xlsx")
  writeResultsWorkbook(OUTPUT, orig$DATA, orig$CategoryNames, f)

  # the whole three-tab workbook goes back in: only Baseline Tables
  # parses (Test Results is vetoed by its TRIAL/ROW header, Summary
  # never looks like a wide table), trial ids come from the exact
  # "Trial: <id>" markers
  blocks <- parseWideTable(f, "xlsx")
  expect_false(is.null(blocks))
  expect_length(blocks, 2)
  expect_identical(vapply(blocks, `[[`, character(1), "trial"),
                   c("A", "B"))
  combined <- do.call(.ppRbindFill, lapply(blocks, `[[`, "data"))
  back <- vdShared(combined)
  expect_false(back$FAIL)
  expectWideRoundTrip(back$DATA, orig$DATA)
})

test_that("the app-level upload path accepts the Editor's View workbook", {
  orig <- vdShared(wideFixtureTwoTrials())
  tabs <- buildBaselineTables(orig$DATA, orig$CategoryNames)
  raw <- tempfile(fileext = ".xlsx")
  writeBaselineTablesXlsx(tabs, raw)
  up <- stage(raw)
  origDATA <- orig$DATA
  shiny::testServer(app_server, {
    session$setInputs(upload = data.frame(
      name = "Editors View.xlsx", datapath = up,
      stringsAsFactors = FALSE))
    d <- reactiveData()
    expect_false(is.null(d))
    expect_setequal(unique(d$TRIAL), c("A", "B"))
    # validation ran on upload and succeeded - the grid holds template
    # lines, not raw wide cells
    v <- reactiveDataValidated()
    expect_false(is.null(v))
    expect_identical(sort(unique(v$ROW)), sort(unique(origDATA$ROW)))
  })
})

test_that("arbitrary real-world headers parse; a bare arm name leaves N empty", {
  m <- rbind(
    c("Variable",            "Control (n=50)", "Treatment"),
    c("Age, mean (SD)",      "45.3 (12.1)",    "46.1 (11.8)"),
    c("Weight, mean (SD)",   "70 (10)",        "72 (11)"))
  f <- writeRawXlsx(m, tempfile(fileext = ".xlsx"))
  blocks <- parseWideTable(f, "xlsx")
  expect_false(is.null(blocks))
  d <- blocks[[1]]$data
  expect_identical(nrow(d), 4L)
  expect_identical(d$N, c(50, NA, 50, NA))
  expect_identical(blocks[[1]]$arms$arm, c("Control", "Treatment"))
  expect_identical(d$MEAN, c(45.3, 46.1, 70, 72))
  expect_identical(d$ROUND_MEAN, c(1L, 1L, 0L, 0L))
})

test_that("untagged headers with arm names need data-row evidence, then parse", {
  m <- rbind(
    c("Characteristic",  "Placebo",       "Drug"),
    c("Age",             "45.3 ± 12.1", "46.1 ± 11.8"),
    c("BMI",             "24.2 (3.1)",    "24.8 (3.4)"))
  f <- writeRawXlsx(m, tempfile(fileext = ".xlsx"))
  blocks <- parseWideTable(f, "xlsx")
  expect_false(is.null(blocks))
  d <- blocks[[1]]$data
  # "a +/- b" is mean/SD; untagged "a (b)" under a continuous label too
  expect_identical(d$ROW, c("Age", "Age", "BMI", "BMI"))
  expect_identical(d$SD, c(12.1, 11.8, 3.1, 3.4))
})

test_that("n (%) rows become a count column plus its complement", {
  m <- rbind(
    c("",                  "Arm 1 (n = 20)", "Arm 2 (n = 25)"),
    c("Age, mean (SD)",    "45.3 (12.1)",    "46.1 (11.8)"),
    c("Diabetes",          "5 (25%)",        "10 (40%)"))
  f <- writeRawXlsx(m, tempfile(fileext = ".xlsx"))
  blocks <- parseWideTable(f, "xlsx")
  d <- blocks[[1]]$data
  dia <- d[d$ROW == "Diabetes", ]
  expect_identical(dia$Diabetes, c(5L, 10L))
  expect_identical(dia[["Not Diabetes"]], c(15L, 15L))
  expect_true(all(is.na(dia$MEAN)))
})

test_that("median rows: explicit IQR parses, range and unlabeled refuse", {
  m <- rbind(
    c("Variable",                     "Arm 1 (n = 20)", "Arm 2 (n = 22)"),
    c("Duration, median [Q1, Q3]",    "127 [98, 160]",  "133 [101, 155]"),
    c("Stay, median (range)",         "5 [2, 21]",      "6 [3, 19]"),
    c("Pain",                         "3 [2, 5]",       "4 [2, 6]"))
  f <- writeRawXlsx(m, tempfile(fileext = ".xlsx"))
  blocks <- parseWideTable(f, "xlsx")
  d <- blocks[[1]]$data
  dur <- d[d$ROW == "Duration", ]
  expect_identical(dur$MEAN, c(127, 133))
  expect_identical(dur$Q1, c(98, 101))
  expect_identical(dur$Q3, c(160, 155))
  expect_true(all(is.na(dur$SD)))
  # the two refusals carry their reasons
  sk <- blocks[[1]]$skipped
  expect_identical(nrow(sk), 2L)
  expect_match(sk$reason[sk$label == "Stay"], "range")
  expect_match(sk$reason[sk$label == "Pain"], "unlabeled interval")
})

test_that("trailing empty cells drop the line; interior gaps hold position", {
  m <- rbind(
    c("Variable",           "Arm 1 (n = 20)", "Arm 2 (n = 25)"),
    c("Age, mean (SD)",     "45.3 (12.1)",    "46.1 (11.8)"),
    c("Only 1, mean (SD)",  "50 (9)",         ""),
    c("Only 2, mean (SD)",  "",               "51 (8)"))
  f <- writeRawXlsx(m, tempfile(fileext = ".xlsx"))
  d <- parseWideTable(f, "xlsx")[[1]]$data
  expect_identical(sum(d$ROW == "Only 1"), 1L)       # trailing: no line
  o2 <- d[d$ROW == "Only 2", ]
  expect_identical(nrow(o2), 2L)                     # interior: NA line
  expect_true(is.na(o2$MEAN[1]) && o2$MEAN[2] == 51)
})

test_that("an unreadable row is skipped with its reason, others still parse", {
  m <- rbind(
    c("Variable",          "Arm 1 (n = 20)", "Arm 2 (n = 25)"),
    c("Age, mean (SD)",    "45.3 (12.1)",    "46.1 (11.8)"),
    c("ASA class",         "I-II",           "mostly II"))
  f <- writeRawXlsx(m, tempfile(fileext = ".xlsx"))
  blocks <- parseWideTable(f, "xlsx")
  expect_identical(blocks[[1]]$skipped$label, "ASA class")
  expect_match(blocks[[1]]$skipped$reason, "not in a recognized format")
  expect_identical(unique(blocks[[1]]$data$ROW), "Age")
})

test_that("the template and example spreadsheets are NOT detected as wide", {
  # regression pin: the long template format must keep flowing to
  # validateData() - its header row (TRIAL/ROW/N/MEAN/SD) is the veto
  for (nm in c("Template.xlsx", "Example.xlsx")) {
    f <- system.file("extdata", nm, package = "IntegrityAnalysis")
    skip_if(f == "", paste(nm, "not installed"))
    expect_null(parseWideTable(f, "xlsx"))
  }
})

test_that("a sheet that is no kind of table returns NULL (fallback path)", {
  f <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(head(mtcars), f)
  expect_null(parseWideTable(f, "xlsx"))
})

test_that("the csv flavor parses like the xlsx", {
  lines <- c("Variable,Arm 1 (n = 15),Arm 2 (n = 17)",
             "\"Age, mean (SD)\",45.3 (12.1),46.1 (11.8)",
             "\"Sex, n\",,",
             "    MALE,10,12",
             "    FEMALE,5,5")
  f <- tempfile(fileext = ".csv")
  writeLines(lines, f)
  blocks <- parseWideTable(f, "csv")
  expect_false(is.null(blocks))
  d <- blocks[[1]]$data
  expect_true(is.na(blocks[[1]]$trial))     # csv: caller names the trial
  sex <- d[d$ROW == "Sex", ]
  expect_identical(sex$MALE, c(10L, 12L))
  expect_identical(sex$FEMALE, c(5L, 5L))
  age <- d[d$ROW == "Age", ]
  expect_identical(age$N, c(15, 17))
})

test_that("a differing-N suffix comes back as that line's N", {
  m <- rbind(
    c("Variable",            "Arm 1 (n = 15)", "Arm 2 (n = 17)"),
    c("Age, mean (SD)",      "45.3 (12.1)",    "46.1 (11.8)"),
    c("Height, mean (SD)",   "165 (7); n = 14", "167 (7)"))
  f <- writeRawXlsx(m, tempfile(fileext = ".xlsx"))
  d <- parseWideTable(f, "xlsx")[[1]]$data
  ht <- d[d$ROW == "Height", ]
  expect_identical(ht$N, c(14, 17))
  expect_identical(ht$MEAN, c(165, 167))
})
