# Issue 19: Word .docx manuscripts as input.
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-21,
# with R/parseDocx.R. Fixtures come from helper-syntheticDocx.R - real
# Word tables built with officer, in submission format (prose first,
# tables at the end, caption before the table).
suppressWarnings(suppressPackageStartupMessages({
  library(shiny); library(officer)
}))

test_that("a submission-format manuscript parses: values, Ns, categories", {
  f <- syntheticDocxMeanSD()
  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  expect_s3_class(res, "ParsePDFTable")
  expect_identical(res$engine, "heuristic-docx")
  expect_identical(res$layout, "docx")
  d <- res$data

  age <- d[d$ROW == "Age", ]
  expect_identical(age$MEAN, c(45.3, 46.1))
  expect_identical(age$SD, c(12.1, 11.8))
  expect_identical(age$ROUND_MEAN, c(1L, 1L))
  expect_identical(age$N, c(15L, 17L))

  wt <- d[d$ROW == "Weight", ]
  expect_identical(wt$MEAN, c(63, 68))
  expect_identical(wt$ROUND_MEAN, c(0L, 0L))

  # M/F fraction -> Male/Female columns; multi-row category accumulates
  sex <- d[d$ROW == "Sex", ]
  expect_identical(sex$Male, c(10L, 12L))
  expect_identical(sex$Female, c(5L, 5L))
  surg <- d[d$ROW == "Type of surgery", ]
  expect_identical(surg[["Upper abdominal"]], c(3L, 4L))
  expect_identical(surg[["Urologic"]], c(7L, 7L))

  expect_identical(nrow(res$arms), 2L)
  expect_equal(res$arms$N, c(15, 17), ignore_attr = TRUE)
  expect_match(res$caption, "Baseline patient characteristics")
  # and the parsed frame validates end to end
  v <- shiny::isolate(validateData(d))
  expect_false(v$FAIL)
})

test_that("footnotes disambiguate a (b): mean (SD) vs number (percent)", {
  f1 <- makeTableDocx(
    file.path(tempdir(), "parenSD.docx"),
    caption = "Table 1. Demographic and baseline characteristics",
    headers = c("Characteristic", "Group A (n = 40)", "Group B (n = 42)"),
    rows = rbind(c("Age, yr",         "61.2 (10.4)", "59.8 (11.1)"),
                 c("Body mass index", "27.3 (4.2)",  "26.9 (3.8)"),
                 c("Male sex, n (%)", "24 (60%)",    "25 (60%)")),
    footnote = "Data are presented as mean (SD) or number (percent).")
  res <- parseBaselineTableHeuristics(f1, quiet = TRUE)
  d <- res$data
  expect_identical(d[d$ROW == "Age, yr", ]$SD, c(10.4, 11.1))
  male <- d[d$ROW == "Male sex", ]
  expect_identical(male[["Male sex"]], c(24L, 25L))
  expect_identical(male[["Not Male sex"]], c(16L, 17L))
})

test_that("a p-value column is detected and dropped", {
  f <- makeTableDocx(
    file.path(tempdir(), "pcol.docx"),
    caption = "Table 1. Baseline characteristics of the study groups",
    headers = c("Characteristic", "Group A (n = 40)", "Group B (n = 42)",
                "P value"),
    rows = rbind(c("Age, yr",         "61.2 (10.4)", "59.8 (11.1)", "0.55"),
                 c("Body mass index", "27.3 (4.2)",  "26.9 (3.8)",  "0.67")),
    footnote = "Data are presented as mean (SD).")
  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  expect_identical(nrow(res$arms), 2L)
  age <- res$data[res$data$ROW == "Age, yr", ]
  expect_identical(nrow(age), 2L)
  expect_identical(age$MEAN, c(61.2, 59.8))
})

test_that("a table with no caption at all still parses", {
  f <- makeTableDocx(
    file.path(tempdir(), "nocap.docx"),
    headers = c("Characteristic", "Control (n = 15)", "Treatment (n = 17)"),
    rows = rbind(c("Age (yr)", "45.3 ± 12.1", "46.1 ± 11.8")))
  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  expect_identical(res$data[res$data$ROW == "Age", ]$MEAN, c(45.3, 46.1))
  expect_true(is.na(res$caption) || !nzchar(res$caption))
})

test_that("the baseline table beats a larger results table (caption rules)", {
  decoy <- list(
    caption = "Table 3. Pain scores over time",
    headers = c("Time", "Group A (n = 40)", "Group B (n = 42)"),
    rows = rbind(c("1 h",  "4.2 (1.1)", "4.4 (1.2)"),
                 c("2 h",  "3.9 (1.0)", "4.1 (1.1)"),
                 c("6 h",  "3.1 (0.9)", "3.4 (1.0)"),
                 c("12 h", "2.5 (0.8)", "2.8 (0.9)"),
                 c("24 h", "1.9 (0.7)", "2.1 (0.8)"),
                 c("48 h", "1.2 (0.6)", "1.4 (0.7)")))
  f <- makeTableDocx(
    file.path(tempdir(), "decoy.docx"),
    tablesBefore = list(decoy),
    caption = "Table 1. Baseline and demographic characteristics",
    headers = c("Characteristic", "Group A (n = 40)", "Group B (n = 42)"),
    rows = rbind(c("Age, yr", "61.2 (10.4)", "59.8 (11.1)")),
    footnote = "Data are presented as mean (SD).")
  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  expect_match(res$caption, "Baseline")
  expect_true("Age, yr" %in% res$data$ROW)
  expect_false(any(grepl("^1 h", res$data$ROW)))
})

test_that("arm N is recovered from the Methods text when the table has none", {
  f <- makeTableDocx(
    file.path(tempdir(), "recovery.docx"),
    prose = c(paste("Methods. Patients were randomly allocated to the",
                    "saline group (n = 24) or the drug group (n = 26)."),
              "A total of 50 patients were randomized."),
    caption = "Table 1. Baseline characteristics",
    headers = c("Characteristic", "Saline", "Drug"),
    rows = rbind(c("Age (yr)",    "45.3 ± 12.1", "46.1 ± 11.8"),
                 c("Weight (kg)", "63 ± 13",     "68 ± 12")))
  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  expect_equal(res$arms$N, c(24, 26), ignore_attr = TRUE)
  expect_true(any(grepl("document text", res$armNSource)))
  fl <- reviewFlags(res)
  expect_true(any(grepl("CONSORT", fl)))
})

test_that("ragged and empty cells are tolerated", {
  f <- makeTableDocx(
    file.path(tempdir(), "ragged.docx"),
    caption = "Table 1. Baseline characteristics",
    headers = c("Characteristic", "Control (n = 15)", "Treatment (n = 17)"),
    rows = rbind(c("Age (yr)",    "45.3 ± 12.1", "46.1 ± 11.8"),
                 c("Weight (kg)", "63 ± 13",     ""),
                 c("",            "",            "")))
  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  expect_identical(res$data[res$data$ROW == "Age", ]$MEAN, c(45.3, 46.1))
  wt <- res$data[res$data$ROW == "Weight", ]
  expect_identical(wt$MEAN, 63)
})

test_that("the adapter keeps one cluster per Word column, even wide cells", {
  mat <- rbind(
    c("Characteristic", "Control (n = 15)", "Treatment (n = 17)"),
    c(paste("An implausibly long variable label that keeps going and",
            "going and would overrun a naive fixed pitch"),
      "45.3 ± 12.1", "46.1 ± 11.8"),
    c("Weight (kg)", "63 ± 13", "68 ± 12"))
  adapted <- .ppDocxLines(mat)
  toks <- do.call(rbind, lapply(adapted$lines, .ppTokenizeLine))
  # drop header-line tokens (the "(n = 15)" numbers); keep data tokens
  dataToks <- toks[toks$type == "meanSD", , drop = FALSE]
  cl <- .ppClusterColumns(dataToks$mid)
  expect_identical(cl$n, 2L)
})

test_that("median [Q1, Q3] flows through the docx path once the engine emits it", {
  # graceful pre-merge guard: the medianRng num3 capture ships in the
  # issue-18 PR; this test activates when both PRs are in
  probe <- .ppTokenizeLine(data.frame(text = "1 [1-2]", x = 0, width = 7,
                                      stringsAsFactors = FALSE))
  skip_if_not("num3" %in% names(probe),
              "engine does not carry medianRng num3 yet (issue 18 PR)")
  f <- makeTableDocx(
    file.path(tempdir(), "median.docx"),
    caption = "Table 1. Baseline characteristics",
    headers = c("Characteristic", "Control (n = 15)", "Treatment (n = 17)"),
    rows = rbind(
      c("Age (yr)",                    "45.3 ± 12.1",  "46.1 ± 11.8"),
      c("Duration (min), median (IQR)", "127 [98, 160]", "133 [101, 155]")))
  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  dur <- res$data[grepl("Duration", res$data$ROW), ]
  expect_identical(dur$MEAN, c(127, 133))
  expect_identical(dur$Q1, c(98, 101))
  expect_identical(dur$Q3, c(160, 155))
})

test_that("the ai guard refuses a docx unless ai = never", {
  f <- syntheticDocxMeanSD()
  expect_error(parseBaselineTable(f, ai = "always", quiet = TRUE),
               "not available for .docx", fixed = TRUE)
  res <- parseBaselineTable(f, ai = "never", quiet = TRUE)
  expect_s3_class(res, "ParsePDFTable")
})

test_that("the subprocess batch path parses a docx (inst/ shipping guard)", {
  f <- syntheticDocxMeanSD()
  res <- parseBaselineTableFiles(f, quiet = TRUE)
  expect_true(res$ok[1])
  expect_identical(res$engine[1], "heuristic-docx")
  expect_true(nrow(res$result[[1]]$data) > 0)
})

test_that("the app accepts a docx upload end to end", {
  src <- syntheticDocxMeanSD()
  d <- file.path(tempdir(), paste0("updocx", basename(tempfile(""))))
  dir.create(d)
  staged <- file.path(d, "manuscript.docx")
  file.copy(src, staged)
  shiny::testServer(app_server, {
    session$setInputs(upload = data.frame(
      name = "manuscript.docx", datapath = staged,
      stringsAsFactors = FALSE))
    g <- reactiveData()
    expect_false(is.null(g))
    expect_true("Age" %in% g$ROW)
    expect_identical(unique(g$TRIAL), "manuscript")
  })
})

test_that("a zip entry ending .docx is accepted, not refused as nested", {
  plan <- .zipEntryPlan(data.frame(
    Name = c("trial1.docx", "trial2.pdf", "inner.zip"),
    Length = c(10000, 10000, 10000), stringsAsFactors = FALSE))
  expect_true(plan$take[plan$Name == "trial1.docx"])
  expect_false(plan$take[plan$Name == "inner.zip"])
})
