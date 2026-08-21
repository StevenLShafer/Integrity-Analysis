# Regressions distilled from the A&A submitted-manuscript corpus
# (C:/Temp/AA/"AA Peer Review Files for Parsing", 654 RCT submissions).
#
# Submitted manuscripts are not journal articles: they open with an
# Editorial Manager cover page, the text is double-spaced with a margin
# line-number rail, and the tables sit at the END of the document - often
# with the caption physically separated from the table, and often running
# over more than one page. Each fixture below rebuilds one of those
# layouts synthetically, in the style of test-real-layouts.R.

# --------------------------------------------------------------------------
# 1. The margin line-number rail
# --------------------------------------------------------------------------
# Manuscripts number every line down the left margin. Those integers sit at
# the far left of the page, so they read as a column of bare numbers - which
# contaminates column clustering and makes prose lines look like data rows.
lineNumberRailPdf <- function(dir = tempdir()) {
  f  <- file.path(dir, "rail.pdf")
  vx <- c(300, 420)
  cells <- list()
  for (i in 1:25)                       # the rail: 1..25 at x = 20
    cells <- c(cells, list(list(x = 20, y = 60 + 26 * i, text = as.character(i))))
  cells <- c(cells,
    list(list(x = 72, y = 86, text = "Table 1. Baseline patient characteristics", adj = 0)),
    rowCells(112, "", c("Control", "Treatment"), vx),
    rowCells(138, "", c("(n = 15)", "(n = 17)"), vx),
    rowCells(164, "Age (yr)",    c("45.3 \u00b1 12.1", "46.1 \u00b1 11.8"), vx),
    rowCells(190, "Weight (kg)", c("63 \u00b1 13",     "68 \u00b1 12"),     vx),
    rowCells(216, "Height (cm)", c("165 \u00b1 7",     "167 \u00b1 7"),     vx),
    rowCells(242, "Sex (M/F)",   c("10/5",             "12/5"),             vx),
    list(list(x = 72, y = 268,
              text = "Values are mean \u00b1 SD or number of patients.", adj = 0)))
  makeTablePdf(f, cells)
}

test_that("a margin line-number rail does not contaminate the table", {
  res <- parseBaselineTableHeuristics(lineNumberRailPdf(), trial = "T",
                                      quiet = TRUE)
  d <- res$data
  expect_equal(nrow(res$arms), 2)
  expect_identical(res$arms$N, c(15L, 17L))
  age <- d[d$ROW == "Age", ]
  expect_equal(age$MEAN, c(45.3, 46.1))
  expect_equal(age$SD,   c(12.1, 11.8))
  # The rail integers must not have become a treatment arm or a data row
  expect_false(any(grepl("^\\d+$", d$ROW)))
})

# --------------------------------------------------------------------------
# 2. Caption physically separated from its table
# --------------------------------------------------------------------------
# Many manuscripts list table captions on their own page (like figure
# legends); the table itself follows on the next page with no caption of its
# own. The caption page has nothing to parse, and the table page has no
# anchor - so before the look-ahead, neither produced the table.
separatedCaptionPdf <- function(dir = tempdir()) {
  f  <- file.path(dir, "separated.pdf")
  vx <- c(300, 420)
  page1 <- list(
    list(x = 72, y = 100, text = "Table 1: Baseline patient characteristics", adj = 0),
    list(x = 72, y = 130, text = "Table 2: Postoperative outcomes by group", adj = 0))
  page2 <- c(
    rowCells(90,  "", c("Saline", "Ketamine"), vx),
    rowCells(116, "", c("(n = 24)", "(n = 26)"), vx),
    rowCells(142, "Age (yr)",    c("52.4 \u00b1 9.7", "51.8 \u00b1 10.2"), vx),
    rowCells(168, "Weight (kg)", c("71 \u00b1 12",    "73 \u00b1 14"),     vx),
    rowCells(194, "Height (cm)", c("168 \u00b1 8",    "169 \u00b1 7"),     vx),
    rowCells(220, "Sex (M/F)",   c("13/11",           "14/12"),            vx))
  makeTablePdfPages(f, list(page1, page2))
}

test_that("a caption on the page before its table still finds the table", {
  res <- parseBaselineTableHeuristics(separatedCaptionPdf(), trial = "T",
                                      quiet = TRUE)
  d <- res$data
  expect_identical(res$arms$N, c(24L, 26L))
  age <- d[d$ROW == "Age", ]
  expect_equal(age$MEAN, c(52.4, 51.8))
  expect_equal(nrow(d), 8)             # 4 variables x 2 arms
})

# --------------------------------------------------------------------------
# 3. A category block whose header line carries only a p-value
# --------------------------------------------------------------------------
# "Race, %                       <0.001" followed by indented percentage
# rows. The p-value made the header line look like a data row, so the
# category children below it were skipped as bare numbers with no header.
pValueHeaderPdf <- function(dir = tempdir()) {
  f   <- file.path(dir, "pheader.pdf")
  vx  <- c(280, 380, 480)
  cells <- c(
    list(list(x = 72, y = 80,
              text = "Table 1. Demographics and baseline characteristics", adj = 0)),
    c(list(list(x = 72,     y = 110, text = "Variable", adj = 0)),
      list(list(x = vx[1],  y = 110, text = "(N = 20)", adj = 0.5)),
      list(list(x = vx[2],  y = 110, text = "(N = 25)", adj = 0.5)),
      list(list(x = 480,    y = 110, text = "P",         adj = 0.5))),
    rowCells(140, "Age, yr", c("55 \u00b1 15", "57 \u00b1 15", "<0.001"), vx),
    rowCells(166, "Race, %", c("", "", "<0.001"), vx),
    rowCells(192, "Caucasian",        c("45", "40"), vx[1:2], labelX = 86),
    rowCells(218, "African American", c("35", "40"), vx[1:2], labelX = 86),
    rowCells(244, "Others",           c("20", "20"), vx[1:2], labelX = 86))
  makeTablePdf(f, cells)
}

test_that("a category header line carrying only a p-value keeps its children", {
  res <- parseBaselineTableHeuristics(pValueHeaderPdf(), trial = "T",
                                      quiet = TRUE, parenIsSD = "sd")
  d <- res$data
  expect_true("Caucasian" %in% names(d))
  race <- d[grepl("^Race", d$ROW), ]
  expect_equal(nrow(race), 2)          # one line per arm
  # the header says "%", so the children are percentages: 45% of 20 is 9,
  # 40% of 25 is 10 (see test-percent-block.R for the conversion itself)
  expect_equal(race$Caucasian, c(9, 10))
})

# --------------------------------------------------------------------------
# 4. A bounds line above the (N = ...) header row
# --------------------------------------------------------------------------
# Quintile tables print the group bounds ("13.1-20.4  20.5-27.3 ...") on a
# line above the arm-N header. That line is full of numbers, so it was taken
# for the first data row - its tokens then seeded the column clustering and
# the arm count exploded.
boundsAboveHeaderPdf <- function(dir = tempdir()) {
  f  <- file.path(dir, "bounds.pdf")
  vx <- c(280, 380, 480)
  cells <- c(
    list(list(x = 72, y = 80,
              text = "Table 1. Baseline characteristics by tertile", adj = 0)),
    rowCells(108, "", c("13", "13.1-20.4", "20.5-27.3"), vx),
    rowCells(134, "", c("(N = 702)", "(N = 709)", "(N = 711)"), vx),
    rowCells(162, "Age, yr",    c("55 \u00b1 15", "57 \u00b1 15", "59 \u00b1 15"), vx),
    rowCells(188, "Weight, kg", c("81 \u00b1 20", "84 \u00b1 21", "86 \u00b1 19"), vx))
  makeTablePdf(f, cells)
}

test_that("a numeric bounds line above the header does not explode the arms", {
  res <- parseBaselineTableHeuristics(boundsAboveHeaderPdf(), trial = "T",
                                      quiet = TRUE)
  expect_equal(nrow(res$arms), 3)
  expect_identical(res$arms$N, c(702L, 709L, 711L))
  age <- res$data[grepl("^Age", res$data$ROW), ]   # label stays "Age, yr"
  expect_equal(age$MEAN, c(55, 57, 59))
})

# --------------------------------------------------------------------------
# 5. A table that continues onto the next page
# --------------------------------------------------------------------------
# Manuscript tables regularly run over the page break, with no repeated
# caption on the continuation page. The parser used to stop at the bottom of
# the caption's page and silently lose the rest.
twoPageTablePdf <- function(dir = tempdir()) {
  f  <- file.path(dir, "twopage.pdf")
  vx <- c(300, 420)
  page1 <- c(
    list(list(x = 72, y = 620,
              text = "Table 1. Baseline patient characteristics", adj = 0)),
    rowCells(646, "", c("Control", "Treatment"), vx),
    rowCells(672, "", c("(n = 30)", "(n = 31)"), vx),
    rowCells(698, "Age (yr)",    c("61.2 \u00b1 8.4", "60.7 \u00b1 9.1"), vx),
    rowCells(724, "Weight (kg)", c("74 \u00b1 11",    "76 \u00b1 12"),    vx),
    rowCells(750, "Height (cm)", c("166 \u00b1 6",    "167 \u00b1 8"),    vx))
  page2 <- c(
    rowCells(90,  "Body mass index", c("26.8 \u00b1 3.1", "27.2 \u00b1 3.4"), vx),
    rowCells(116, "Sex (M/F)",       c("17/13",           "18/13"),           vx),
    rowCells(142, "ASA class I/II",  c("21/9",            "20/11"),           vx),
    list(list(x = 72, y = 190,
              text = "Values are mean \u00b1 SD or number of patients.", adj = 0)))
  makeTablePdfPages(f, list(page1, page2))
}

test_that("a table running onto the next page keeps its continuation rows", {
  res <- parseBaselineTableHeuristics(twoPageTablePdf(), trial = "T",
                                      quiet = TRUE)
  d <- res$data
  expect_identical(res$arms$N, c(30L, 31L))
  expect_true("Body mass index" %in% d$ROW)
  expect_true(any(grepl("^Sex", d$ROW)))
  bmi <- d[d$ROW == "Body mass index", ]
  expect_equal(bmi$MEAN, c(26.8, 27.2))
})

# --------------------------------------------------------------------------
# 6. A wide label-to-value gap that reads as a column gutter
# --------------------------------------------------------------------------
# Word-processed tables leave a wide white gap between the label column and
# the first value column. The gutter detector reads that gap as a two-column
# page and splits the TABLE itself - labels in one band, values in the other
# (the failure observed on real submissions). The full-width reading must
# win over either mangled half.
wideGapTablePdf <- function(dir = tempdir()) {
  f  <- file.path(dir, "widegap.pdf")
  vx <- c(400, 500)
  cells <- c(
    list(list(x = 72, y = 80,
              text = "Table 1. Demographic and baseline characteristics of the study patients",
              adj = 0)),
    rowCells(110, "", c("Placebo", "Drug"), vx, labelX = 72),
    rowCells(136, "", c("(n = 45)", "(n = 44)"), vx, labelX = 72),
    rowCells(162, "Age (yr)",         c("58.3 \u00b1 11.2", "59.1 \u00b1 10.8"), vx),
    rowCells(188, "Weight (kg)",      c("79 \u00b1 15",     "81 \u00b1 16"),     vx),
    rowCells(214, "Height (cm)",      c("171 \u00b1 9",     "170 \u00b1 8"),     vx),
    rowCells(240, "Male sex",         c("28",               "27"),               vx),
    rowCells(266, "Diabetes",         c("9",                "11"),               vx),
    rowCells(292, "Hypertension",     c("21",               "19"),               vx))
  makeTablePdf(f, cells)
}

test_that("a wide label-to-value gap does not split the table into bands", {
  res <- parseBaselineTableHeuristics(wideGapTablePdf(), trial = "T",
                                      quiet = TRUE)
  d <- res$data
  expect_identical(res$arms$N, c(45L, 44L))
  age <- d[d$ROW == "Age", ]
  expect_equal(age$MEAN, c(58.3, 59.1))
  expect_equal(age$SD,   c(11.2, 10.8))
  # The labels must have stayed attached to their values
  expect_false(any(grepl("^Unnamed", d$ROW)))
})
