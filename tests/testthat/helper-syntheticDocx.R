# helper-syntheticDocx.R - build test .docx manuscripts with a
# controlled structure (issue 19), the Word counterpart of
# helper-syntheticPdf.R.
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-21,
# with R/parseDocx.R. We cannot ship a real submitted manuscript, so the
# tests build their own with officer - real Word tables, prose
# paragraphs, captions, footnotes - which is exactly what
# officer::docx_summary() sees in a genuine submission.

# One .docx: optional prose paragraphs, optional earlier tables (each
# list(caption =, headers =, rows =)), then the table under test with
# its caption before it (the submission convention) and a footnote
# after. `headers` is the character vector of column headings; `rows` a
# character matrix (label column first).
makeTableDocx <- function(file, headers, rows, caption = NULL,
                          prose = character(0), footnote = NULL,
                          tablesBefore = list()) {
  addTable <- function(doc, headers, rows) {
    df <- as.data.frame(rows, stringsAsFactors = FALSE)
    names(df) <- headers
    officer::body_add_table(doc, df, header = TRUE)
  }
  doc <- officer::read_docx()
  for (p in prose) doc <- officer::body_add_par(doc, p)
  for (tb in tablesBefore) {
    if (!is.null(tb$caption))
      doc <- officer::body_add_par(doc, tb$caption)
    doc <- addTable(doc, tb$headers, tb$rows)
  }
  if (!is.null(caption)) doc <- officer::body_add_par(doc, caption)
  doc <- addTable(doc, headers, rows)
  if (!is.null(footnote)) doc <- officer::body_add_par(doc, footnote)
  print(doc, target = file)
  file
}

# The standard fixture: a submission-format manuscript - two pages of
# prose, then the baseline table at the END of the file, caption before
# it - carrying every cell shape the PDF fixtures pin (mean +/- SD, an
# M/F fraction, a multi-row category).
syntheticDocxMeanSD <- function(dir = tempdir()) {
  f <- file.path(dir, "meanSD.docx")
  makeTableDocx(
    f,
    prose = c(
      "Effects of things on other things: a randomized controlled trial.",
      paste("Methods. After ethics approval, adult patients scheduled for",
            "elective surgery were enrolled and gave written informed",
            "consent. Anesthesia was induced and maintained per protocol."),
      paste("Results. The groups were comparable at baseline. Outcomes",
            "are reported in the tables below.")),
    caption = "Table 1. Baseline patient characteristics",
    headers = c("Characteristic", "Control (n = 15)", "Treatment (n = 17)"),
    rows = rbind(
      c("Age (yr)",        "45.3 ± 12.1", "46.1 ± 11.8"),
      c("Weight (kg)",     "63 ± 13",     "68 ± 12"),
      c("Height (cm)",     "165 ± 7",     "167 ± 7"),
      c("Sex (M/F)",       "10/5",             "12/5"),
      c("Type of surgery", "",                 ""),
      c("Upper abdominal", "3",                "4"),
      c("Lower abdominal", "5",                "6"),
      c("Urologic",        "7",                "7")),
    footnote = "Values are mean ± SD or number of patients.")
}
