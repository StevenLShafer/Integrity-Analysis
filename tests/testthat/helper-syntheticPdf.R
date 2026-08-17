# helper-syntheticPdf.R - build test PDFs with a controlled layout.
#
############################################################################
# Provenance                                                               #
# Ported 2026-08-15 by Claude Code (model: Claude Opus 5, Anthropic) from  #
# testParseCovariateTable.R in the Integrity-Analysis repository (drafted  #
# 2026-08-14 by Claude Code, model Claude Fable 5).                       #
############################################################################
#
# We cannot ship a copyrighted journal article, so the tests build their own
# PDFs with R's pdf() graphics device: text() places every cell at a
# controlled coordinate, mimicking the typography of a journal "Table 1".
# The text layer poppler then sees is exactly the kind a real article PDF
# has - words with x/y positions - so this exercises the whole pipeline:
# page scoring, line building, tokenizing, column clustering, header
# parsing, row classification, and template assembly.

# Each element of `cells`: list(x =, y =, text =, adj = 0).  y is measured
# from the TOP of the page in points (like pdf_data); page is US letter
# (612 x 792 pt).
makeTablePdf <- function(file, cells) {
  grDevices::pdf(file, width = 8.5, height = 11, encoding = "WinAnsi.enc")
  op <- graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 612), ylim = c(0, 792))
  for (cell in cells) {
    adj <- if (is.null(cell$adj)) 0 else cell$adj
    graphics::text(cell$x, 792 - cell$y, cell$text, adj = c(adj, 1), cex = 0.85)
  }
  graphics::par(op)
  grDevices::dev.off()
  file
}

# Convenience: a whole row of cells at one y.
rowCells <- function(y, label, values, valueX, labelX = 72) {
  out <- list(list(x = labelX, y = y, text = label, adj = 0))
  for (i in seq_along(values))
    if (nchar(values[i]) > 0)
      out <- c(out, list(list(x = valueX[i], y = y, text = values[i], adj = 0.5)))
  out
}

# Test fixture 1: three-part layout common in anesthesia journals - mean +/- SD
# cells, a sex M/F fraction, a multi-row category, a median [range] row that
# must be skipped, and a footnote that must terminate the table.
syntheticPdfMeanSD <- function(dir = tempdir()) {
  f  <- file.path(dir, "meanSD.pdf")
  vx <- c(300, 420)
  cells <- c(
    list(list(x = 72, y = 80, text = "Table 1. Baseline patient characteristics", adj = 0)),
    rowCells(110, "", c("Control", "Treatment"), vx),
    rowCells(128, "", c("(n = 15)", "(n = 17)"), vx),
    rowCells(150, "Age (yr)",    c("45.3 ± 12.1", "46.1 ± 11.8"), vx),
    rowCells(168, "Weight (kg)", c("63 ± 13",     "68 ± 12"),     vx),
    rowCells(186, "Height (cm)", c("165 ± 7",     "167 ± 7"),     vx),
    rowCells(204, "Sex (M/F)",   c("10/5",             "12/5"),             vx),
    rowCells(222, "Type of surgery", c("", ""), vx),
    rowCells(240, "Upper abdominal", c("3", "4"), vx, labelX = 82),
    rowCells(258, "Lower abdominal", c("5", "6"), vx, labelX = 82),
    rowCells(276, "Urologic",        c("7", "7"), vx, labelX = 82),
    rowCells(294, "Duration of surgery (min)", c("127 [98-160]", "133 [101-155]"), vx),
    list(list(x = 72, y = 320,
              text = "Values are mean ± SD, number of patients, or median [range].",
              adj = 0))
  )
  makeTablePdf(f, cells)
}

# Test fixture 2: "mean (SD)" and "n (%)" style with a p-value column that
# must be detected and dropped, and arm N in the header line.
syntheticPdfMeanParen <- function(dir = tempdir()) {
  f   <- file.path(dir, "meanParen.pdf")
  vx2 <- c(280, 400, 500)
  cells <- c(
    list(list(x = 72, y = 80,
              text = "Table 2. Demographic and baseline characteristics", adj = 0)),
    c(list(list(x = 72,     y = 110, text = "Characteristic",   adj = 0)),
      list(list(x = vx2[1], y = 110, text = "Group A (n = 40)", adj = 0.5)),
      list(list(x = vx2[2], y = 110, text = "Group B (n = 42)", adj = 0.5)),
      list(list(x = vx2[3], y = 110, text = "P value",          adj = 0.5))),
    rowCells(140, "Age, yr",           c("61.2 (10.4)", "59.8 (11.1)", "0.55"), vx2),
    rowCells(158, "Body mass index",   c("27.3 (4.2)",  "26.9 (3.8)",  "0.67"), vx2),
    rowCells(176, "Male sex, n (%)",   c("24 (60%)",    "25 (60%)",    "0.98"), vx2),
    rowCells(194, "Diabetes, n (%)",   c("12 (30%)",    "10 (24%)",    "0.53"), vx2),
    rowCells(212, "Creatinine, mg/dL", c("0.81 (0.22)", "0.79 (0.25)", "0.71"), vx2),
    list(list(x = 72, y = 240,
              text = "Data are presented as mean (SD) or number (percent).", adj = 0))
  )
  makeTablePdf(f, cells)
}
