# Regressions distilled from the real-article corpus (C:/temp/journals).
#
# Each of these reproduces, in synthetic form, a layout that made the first
# version of the parser fail on real journal PDFs. We cannot ship the
# articles themselves, so the typography is rebuilt with the pdf() device.

# A two-column page: body prose down the left column, the table in the right.
# This is the layout of most journals, and it is what broke the original
# engine - clustering words into lines by y across the whole page glued each
# table row onto a sentence of prose.
twoColumnPdf <- function(dir = tempdir(), caption = "TABLE I Demographic data",
                         file = "twocol.pdf") {
  f  <- file.path(dir, file)
  vx <- c(430, 505)                     # arm columns, inside the right column
  # Kept short so the left column ends well before the gutter, as in a real
  # two-column page; long lines would run into the table and there would be
  # no gutter to find.
  prose <- c(
    "according to usual clinical",
    "practice. In the study group,",
    "ten minutes prior to the end",
    "of surgery, the isoflurane",
    "concentration was adjusted",
    "to maintain a depth within",
    "the target range. Reversal",
    "of block was achieved with",
    "neostigmine five minutes",
    "before the end, and both",
    "groups had the agents",
    "discontinued at that point.")
  cells <- list()
  for (i in seq_along(prose))
    cells <- c(cells, list(list(x = 64, y = 100 + 18 * i, text = prose[i], adj = 0)))
  cells <- c(cells,
    list(list(x = 313, y = 118, text = caption, adj = 0)),
    rowCells(154, "", c("Standard", "Treated"), vx, labelX = 313),
    rowCells(172, "", c("(n = 31)", "(n = 29)"), vx, labelX = 313),
    rowCells(190, "Age (yr)",    c("70 ± 6",  "71 ± 5"),  vx, labelX = 313),
    rowCells(208, "Weight (kg)", c("84 ± 16", "82 ± 15"), vx, labelX = 313),
    rowCells(226, "Height (cm)", c("170 ± 7", "169 ± 9"), vx, labelX = 313),
    rowCells(244, "Sex (M/F)",   c("21 / 10", "19 / 10"), vx, labelX = 313),
    list(list(x = 313, y = 268,
              text = "Values are expressed as mean ± SD.", adj = 0)))
  makeTablePdf(f, cells)
}

test_that("a table beside body prose in a two-column page is parsed", {
  res <- parseBaselineTableHeuristics(twoColumnPdf(), trial = "T", quiet = TRUE)
  d <- res$data

  expect_equal(nrow(res$arms), 2)
  expect_identical(res$arms$N, c(31L, 29L))
  expect_equal(res$layout, "columns")

  age <- d[d$ROW == "Age", ]
  expect_equal(age$MEAN, c(70, 71))
  expect_equal(age$SD,   c(6, 5))
  wt <- d[d$ROW == "Weight", ]
  expect_equal(wt$MEAN, c(84, 82))
  # No word of the left-hand prose may leak into a row label
  expect_false(any(grepl("neostigmine|isoflurane|clinical", d$ROW)))
})

test_that("Roman-numeral captions are recognised (Anaesthesia and CJA style)", {
  res <- parseBaselineTableHeuristics(
    twoColumnPdf(caption = "TABLE I Demographic data", file = "roman.pdf"),
    quiet = TRUE)
  expect_match(res$caption, "TABLE I")
  expect_equal(nrow(res$data), 8)          # 4 variables x 2 arms
})

test_that("Arabic-numeral captions still work", {
  res <- parseBaselineTableHeuristics(
    twoColumnPdf(caption = "Table 1 Demographic data", file = "arabic.pdf"),
    quiet = TRUE)
  expect_match(res$caption, "Table 1")
  expect_equal(nrow(res$data), 8)
})

test_that("a 'Table N' mentioned inside a sentence is not taken for a caption", {
  # The first version parsed the prose under such a mention as a table.
  f <- file.path(tempdir(), "mention.pdf")
  prose <- c("The pharmacokinetic estimates are summarised below, as",
             "demonstrated in Table 3 B and C , where reported clearance",
             "values of 1.2 and 3.4 differ between the two populations",
             "studied by the authors of the original investigation.")
  cells <- list()
  for (i in seq_along(prose))
    cells <- c(cells, list(list(x = 64, y = 100 + 18 * i, text = prose[i], adj = 0)))
  # ... and a real caption further down, which is what must be found.
  cells <- c(cells,
    list(list(x = 64, y = 260, text = "Table 1 Patient characteristics", adj = 0)),
    rowCells(284, "", c("(n = 12)", "(n = 14)"), c(300, 420)),
    rowCells(302, "Age (yr)",    c("45 ± 12", "46 ± 11"), c(300, 420)),
    rowCells(320, "Weight (kg)", c("63 ± 13", "68 ± 12"), c(300, 420)))
  makeTablePdf(f, cells)

  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  expect_match(res$caption, "Table 1 Patient characteristics")
  expect_identical(res$arms$N, c(12L, 14L))
  expect_setequal(unique(res$data$ROW), c("Age", "Weight"))
})

test_that("an arm abbreviated 'P' is not discarded as a p-value column", {
  # A placebo arm headed "P" was being dropped as if it were a p-value
  # column. That lost the arm, and corrupted the row label of every row -
  # the label is everything left of the first surviving cell, so it absorbed
  # the discarded arm's number ("Weight (kg) 70").
  f  <- file.path(tempdir(), "placebo.pdf")
  vx <- c(230, 300, 370, 440)
  cells <- c(
    list(list(x = 64, y = 80, text = "Table 1 Patient characteristics", adj = 0)),
    rowCells(110, "Characteristic", c("P", "OD2", "OD4", "OD8"), vx),
    rowCells(134, "n",           c("30", "30", "30", "30"), vx),
    rowCells(152, "Age (yr)",    c("41", "43", "42", "41"), vx),
    rowCells(170, "Weight (kg)", c("70 (8)", "71 (9)", "72 (8)", "70 (10)"), vx),
    rowCells(188, "Height (cm)", c("160 (5)", "160 (6)", "160 (5)", "162 (6)"), vx))
  makeTablePdf(f, cells)

  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  expect_equal(nrow(res$arms), 4)                    # the "P" arm survives
  w <- res$data[res$data$ROW == "Weight", ]
  expect_equal(nrow(w), 4)
  expect_equal(w$MEAN, c(70, 71, 72, 70))            # first arm not swallowed
  expect_equal(w$SD,   c(8, 9, 8, 10))
  expect_true(all(w$N == 30))                        # the "n" row still parses
  expect_false(any(grepl("^Weight [0-9]", res$data$ROW)))
})

test_that("a genuine p-value column is still dropped", {
  f  <- file.path(tempdir(), "pval.pdf")
  vx <- c(280, 400, 500)
  cells <- c(
    list(list(x = 64, y = 80, text = "Table 1 Baseline characteristics", adj = 0)),
    rowCells(110, "Characteristic", c("Group A (n = 40)", "Group B (n = 42)", "P value"), vx),
    rowCells(140, "Age, yr",  c("61.2 (10.4)", "59.8 (11.1)", "0.55"), vx),
    rowCells(158, "Weight, kg", c("70.1 (9.2)", "71.3 (8.8)", "0.67"), vx))
  makeTablePdf(f, cells)

  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  expect_equal(nrow(res$arms), 2)
  expect_false(any(res$data$MEAN %in% c(0.55, 0.67)))
})

test_that("a standard error is recorded as SE, not silently as SD", {
  # Papers print an SD or an SE, never a variance. Converting between them is
  # a modelling decision that needs N and a bias correction, so the parser
  # records what was printed and leaves the conversion to the analysis. Filing
  # an SE as an SD is not a small error: at n = 15 it is out by a factor of 4.
  f  <- file.path(tempdir(), "sem.pdf")
  vx <- c(300, 420)
  cells <- c(
    list(list(x = 72, y = 80, text = "Table 1 Patient characteristics", adj = 0)),
    rowCells(110, "", c("(n = 15)", "(n = 15)"), vx),
    rowCells(140, "Age (yr)",    c("39 (4.06)", "39 (3.80)"), vx),
    rowCells(158, "Weight (kg)", c("71 (3.00)", "76 (2.70)"), vx),
    list(list(x = 72, y = 186,
              text = "Values are mean (standard error).", adj = 0)))
  makeTablePdf(f, cells)

  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  d <- res$data[!is.na(res$data$MEAN), ]
  expect_equal(nrow(d), 4)
  expect_true(all(is.na(d$SD)))            # nothing filed as an SD
  expect_equal(d$SE, c(4.06, 3.80, 3.00, 2.70))
  expect_match(res$dispersion, "^se")
  # The printed granularity of the SE is kept, and differs from the mean's
  expect_equal(d$ROUND_MEAN, c(0, 0, 0, 0))
  expect_equal(d$ROUND_DISPERSION, c(2, 2, 2, 2))
  # and the caller is told, because an SE cannot go into the analysis as-is
  expect_true(any(grepl("standard error", reviewFlags(res))))
})

test_that("a stated SD stays an SD, and an unstated one is flagged", {
  mk <- function(file, foot) {
    f  <- file.path(tempdir(), file)
    vx <- c(300, 420)
    cells <- c(
      list(list(x = 72, y = 80, text = "Table 1 Patient characteristics", adj = 0)),
      rowCells(110, "", c("(n = 20)", "(n = 20)"), vx),
      rowCells(140, "Age (yr)",    c("45.3 ± 12.1", "46.1 ± 11.8"), vx),
      rowCells(158, "Weight (kg)", c("63 ± 13", "68 ± 12"), vx),
      if (nzchar(foot)) list(list(x = 72, y = 186, text = foot, adj = 0)))
    makeTablePdf(f, Filter(Negate(is.null), cells))
    parseBaselineTableHeuristics(f, quiet = TRUE)
  }
  stated <- mk("sd-stated.pdf", "Values are mean ± standard deviation.")
  expect_match(stated$dispersion, "^sd \\(stated")
  expect_equal(stated$data$SD[!is.na(stated$data$MEAN)][1], 12.1)
  expect_true(all(is.na(stated$data$SE)))
  expect_false(any(grepl("standard error", reviewFlags(stated))))

  # No footnote at all: recorded as SD by convention, but SAID so
  silent <- mk("sd-silent.pdf", "")
  expect_match(silent$dispersion, "assumed")
  expect_true(any(grepl("does not say", reviewFlags(silent))))
})

test_that("a 'Data are numbers (%)' footnote stops counts becoming means", {
  # "20 (66.7)" under a footnote reading "Data are numbers (%)" is twenty
  # patients and their percentage. Read as mean and SD it becomes a baseline
  # statistic of 20 with an SD of 66.7 - a value that was never printed.
  f  <- file.path(tempdir(), "npct-footnote.pdf")
  vx <- c(300, 420)
  cells <- c(
    list(list(x = 72, y = 80, text = "Table 1 Patient characteristics", adj = 0)),
    rowCells(110, "", c("Saline", "Lidocaine"), vx),
    rowCells(128, "", c("(n = 30)", "(n = 30)"), vx),
    rowCells(150, "Nausea",   c("20 (66.7)", "29 (96.7)"), vx),
    rowCells(168, "Vomiting", c("10 (33.3)", "1 (3.3)"),   vx),
    list(list(x = 72, y = 196, text = "Data are numbers (%).", adj = 0)))
  makeTablePdf(f, cells)

  res <- parseBaselineTableHeuristics(f, quiet = TRUE)
  # Nothing may be recorded as a mean/SD
  expect_true(all(is.na(res$data$MEAN)))
  expect_false(any(res$data$SD %in% c(66.7, 96.7, 33.3), na.rm = TRUE))
  # ...and the counts survive as categories
  expect_true("Nausea" %in% names(res$data))
  expect_equal(res$data$Nausea[!is.na(res$data$Nausea)][1:2], c(20, 29))
})

test_that("the caption score prefers a baseline table over a results table", {
  expect_gt(.ppCaptionScore("Table 1 Baseline patient characteristics"),
            .ppCaptionScore("Table 2 Postoperative complications"))
  expect_gt(.ppCaptionScore("TABLE I Demographic data"),
            .ppCaptionScore("TABLE II Intraoperative drug usage"))

  # "Characteristics" only means baseline data when it is qualified: this
  # results table was being chosen over the real baseline table.
  expect_gt(.ppCaptionScore("Table 1 Patient characteristics"),
            .ppCaptionScore("Table 3 Characteristics of sensory and motor blocks"))
  expect_lt(.ppCaptionScore("Table 3 Characteristics of sensory and motor blocks"), 3)

  # A baseline table that also mentions intra-operative variables is still a
  # baseline table - the results-table penalty must not cancel "Baseline".
  expect_gte(.ppCaptionScore("Table 1 Baseline and pre- and intra-operative data"), 3)
})

test_that("the AI engine is pointed at the page holding the best caption", {
  # Page selection, not reading, was what made the table fallback fail on a
  # third of the trials it was meant to rescue: it used baseline *vocabulary*
  # to pick a page and landed on prose that merely discussed the results.
  dir <- file.path(tempdir(), "pagepick")
  dir.create(dir, showWarnings = FALSE)
  f <- file.path(dir, "twopage.pdf")

  grDevices::pdf(f, width = 8.5, height = 11, encoding = "WinAnsi.enc")
  op <- graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  # Page 1: prose stuffed with baseline vocabulary, but no table
  graphics::plot.new(); graphics::plot.window(xlim = c(0, 612), ylim = c(0, 792))
  graphics::text(72, 792 - 100, "Baseline demographic characteristics of the",
                 adj = c(0, 1), cex = 0.85)
  graphics::text(72, 792 - 118, "patients were similar between groups (n = 30).",
                 adj = c(0, 1), cex = 0.85)
  # Page 2: the actual captioned table
  graphics::plot.new(); graphics::plot.window(xlim = c(0, 612), ylim = c(0, 792))
  cells <- c(
    list(list(x = 72, y = 80, text = "Table 1 Patient characteristics", adj = 0)),
    rowCells(110, "", c("(n = 15)", "(n = 17)"), c(300, 420)),
    rowCells(140, "Age (yr)",    c("45 ± 12", "46 ± 11"), c(300, 420)),
    rowCells(158, "Weight (kg)", c("63 ± 13", "68 ± 12"), c(300, 420)))
  for (cell in cells)
    graphics::text(cell$x, 792 - cell$y, cell$text,
                   adj = c(if (is.null(cell$adj)) 0 else cell$adj, 1), cex = 0.85)
  graphics::par(op); grDevices::dev.off()

  pages <- pdftools::pdf_data(f)
  expect_equal(.ppBestCaptionPage(pages), 2)      # the table, not the prose
  # The old vocabulary scorer is exactly what got this wrong
  expect_equal(which.max(vapply(pages, .ppScorePage, numeric(1))), 1)
})

test_that("caption page selection returns NULL when no caption exists", {
  f <- file.path(tempdir(), "nocaption.pdf")
  makeTablePdf(f, list(
    list(x = 72, y = 80,  text = "Discussion", adj = 0),
    list(x = 72, y = 110, text = "The groups did not differ appreciably.", adj = 0)))
  expect_null(.ppBestCaptionPage(pdftools::pdf_data(f)))
})

test_that("page banding finds a gutter only when there really is one", {
  # Words are jittered per line, as in real prose: inter-word gaps fall in
  # different places on every line, so only a true gutter has low coverage
  # across the whole block.
  set.seed(42)
  runOfWords <- function(x0, x1, y, n) {
    starts <- sort(stats::runif(n, x0, x1 - 30))
    data.frame(text = "word", x = starts, y = y, width = 28)
  }
  ys <- seq(100, 340, length.out = 16)

  # Two columns with a clear gutter between 300 and 330
  w2 <- do.call(rbind, lapply(ys, function(y)
    rbind(runOfWords(64, 300, y, 5), runOfWords(330, 545, y, 5))))
  expect_equal(nrow(.ppPageBands(w2)), 2)

  # One column spanning the text block: no gutter to find
  w1 <- do.call(rbind, lapply(ys, function(y) runOfWords(64, 545, y, 10)))
  expect_equal(nrow(.ppPageBands(w1)), 1)
})
