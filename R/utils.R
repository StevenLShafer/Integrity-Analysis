# utils.R - small text and number helpers shared by both engines.
#
############################################################################
# Provenance                                                               #
# Ported 2026-08-15 by Claude Code (model: Claude Opus 5, Anthropic) at    #
# Steve Shafer's request, from parseCovariateTable.R in the               #
# Integrity-Analysis repository - which Claude Code (model: Claude Fable   #
# 5) drafted on 2026-08-14. The logic here is unchanged from that draft;   #
# the only edits were mechanical: the internal prefix `.pcv` became `.pp`, #
# and the original single file was split into utils.R / tokenize.R /       #
# pageLayout.R / parseBaselineTableHeuristics.R.                          #
#                                                                          #
# Everything in this file is deterministic - it calls no AI service.       #
# Status: run and verified by tests/testthat/test-utils.R.                #
############################################################################

# The plus-minus sign, kept here as an escape so that every source file in
# the package stays pure ASCII (R CMD check notes non-ASCII bytes in R/).
.ppPLUSMINUS <- "\u00b1"

# --------------------------------------------------------------------------
# Font-encoding repair
# --------------------------------------------------------------------------
# Some journals embed fonts whose glyphs are mapped to the wrong Unicode
# points, so poppler faithfully reports characters that are not what is
# printed. Anesthesiology is the worst: in its PDFs a printed "=" comes back
# as U+2AFD or U+2D1D and a printed "+/-" as U+2AFE - the ASCII forms never
# appear at all. That is invisible until you look, and it is fatal here,
# because "45 +/- 12" stops being a mean-and-SD cell and "(n = 20)" stops
# being an arm size. Repairing this recovers whole journals.
#
# Every entry below was read off surrounding context in the corpus rather
# than guessed - e.g. U+2AFE from "Data are presented as mean <U+2AFE> SD",
# U+2D1D from "20% mannitol (n <U+2D1D> 20)". A wrong entry would corrupt
# numbers silently, so anything ambiguous was left alone.
.ppGlyphMap <- c(
  "\u2afe" = "\u00b1",  # mean +/- SD
  "\u2afd" = "=",       # "osmolarity = ", "1 = perfectly relaxed"
  "\u2d1d" = "=",       # "(n = 20)"
  "\u2b0d" = "<",       # "P < 0.05"
  "\u2b0e" = ">",       # "> 150"
  "\u2afa" = "-",       # minus: "stored at -20 C", "kg-1"
  "\u2af9" = "+",       # "Ca2+"
  "\u2c55" = "\u2264",  # <=
  "\u2c56" = "\u2265",  # >=
  "\u2afb" = "\u00d7",  # multiplication sign
  "\u2d1b" = "\u00d7",
  "\u4860" = "\u00b7",  # middle dot, as in "kg-1 . min-1" and BJA decimals
  "\u242e" = "\u00b5",  # micro
  "\u2423" = "\u03b1", "\u2424" = "\u03b2",   # Greek, in labels only
  "\u2425" = "\u03b3", "\u2426" = "\u03b4",
  "\u2440" = "\u03b5",
  # BJA / EJA house font: "p \u00bc 0.04", "n \u00bc 503" - another "=".
  "\u00bc" = "=",
  # ...and its "fl" ligature, which turns desflurane into "des-urane".
  # Affects row labels only, never numbers.
  "\u00af" = "fl")

# Apply the repair to a character vector.
#
# Deliberately NOT chartr(): its "old" and "new" arguments are range
# specifications, so a map containing ">", "-" and "+" in sequence is read as
# the range ">-+" and the call fails. Fixed-string gsub has no such
# interpretation. Only strings that actually contain a non-ASCII byte are
# touched, which keeps the cost off the many PDFs that need no repair.
.ppNormalizeGlyphs <- function(x) {
  if (!length(x)) return(x)
  hit <- grepl("[^\x01-\x7F]", x)
  if (!any(hit)) return(x)
  for (i in seq_along(.ppGlyphMap))
    x[hit] <- gsub(names(.ppGlyphMap)[i], .ppGlyphMap[[i]], x[hit], fixed = TRUE)
  x
}

# pdftools wrappers that apply the repair at the point of reading, so no
# caller has to remember to do it.
# Some documents use U+00B1 - a real plus-minus character - where an EN DASH
# is printed. British Journal of Anaesthesia's 2003 volumes do this
# throughout: "pages 502<pm>6", "propofol 1.5<pm>2.5 mg kg<pm>1",
# "Creutzfeldt<pm>Jakob disease". Left alone, the tokenizer reads "1.5<pm>2.5"
# as a mean of 1.5 with an SD of 2.5 and INVENTS a baseline value, which is
# far worse than missing one.
#
# The tell is a plus-minus touching a letter: a real plus-minus never does,
# but a dash does, in units ("kg<pm>1") and hyphenated names. Two such
# occurrences are taken as proof for the whole document, and every
# plus-minus in it is then read as the dash it actually is.
.ppPlusMinusIsDash <- function(txt) {
  if (!length(txt)) return(FALSE)
  j   <- paste(txt, collapse = " ")
  pat <- paste0("[A-Za-z]", .ppPLUSMINUS, "|", .ppPLUSMINUS, "[A-Za-z]")
  m   <- gregexpr(pat, j)[[1]]
  if (m[1] == -1) return(FALSE)
  length(m) >= 2
}

.ppPdfData <- function(pdfFile) {
  pg   <- pdftools::pdf_data(pdfFile)
  txt  <- unlist(lapply(pg, function(d) if (nrow(d)) d$text else character(0)))
  dash <- .ppPlusMinusIsDash(.ppNormalizeGlyphs(txt))
  lapply(pg, function(d) {
    if (nrow(d)) {
      d$text <- .ppNormalizeGlyphs(d$text)
      if (dash) d$text <- gsub(.ppPLUSMINUS, "-",d$text, fixed = TRUE)
    }
    d
  })
}

# Word boxes from OCR, in the same shape .ppPdfData() returns, so a scanned
# article can go through the identical pipeline.
#
# The units matter. pdf_ocr_data() reports pixels at the rendering dpi, while
# pdf_data() reports PDF points (72 per inch) - and every tolerance in this
# package is in points: a 3-point line gap, a 25-point column gap. Handing the
# parser raw 300-dpi pixels makes every gap look four times too wide and
# nothing clusters into lines or columns at all.
# Render the pages of a PDF to images, OCR them, and DELETE THE IMAGES.
#
# pdftools::pdf_ocr_text() and pdf_ocr_data() render each page to a .png named
# after the PDF in the *current working directory* and leave it there. For a
# package whose output feeds peer-review fraud screening - where the promise is
# that the manuscript is deleted and nothing is retained - leaving full-page
# images of a submission lying about is unacceptable, quite apart from
# littering the caller's working directory.
#
# So the rendering is done explicitly into a temporary directory that is
# removed on exit, including when OCR throws.
.ppOcrPages <- function(pdfFile, dpi = 300, pages = NULL,
                        want = c("data", "text")) {
  want <- match.arg(want)
  if (!requireNamespace("tesseract", quietly = TRUE))
    stop("OCR needs the 'tesseract' package: install.packages(\"tesseract\")",
         call. = FALSE)
  if (is.null(pages)) pages <- seq_len(pdftools::pdf_info(pdfFile)$pages)

  tmp <- tempfile("ppocr")
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  imgs <- pdftools::pdf_convert(
    pdfFile, format = "png", pages = pages, dpi = dpi, verbose = FALSE,
    filenames = file.path(tmp, sprintf("page%04d.png", pages)))

  if (want == "text")
    vapply(imgs, function(i) tesseract::ocr(i), character(1), USE.NAMES = FALSE)
  else
    lapply(imgs, function(i) tesseract::ocr_data(i))
}

.ppOcrData <- function(pdfFile, dpi = 300, pages = NULL) {
  pg <- .ppOcrPages(pdfFile, dpi = dpi, pages = pages, want = "data")
  if (is.data.frame(pg)) pg <- list(pg)
  scale <- 72 / dpi
  lapply(pg, function(d) {
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    empty <- data.frame(text = character(0), x = numeric(0), y = numeric(0),
                        width = numeric(0), height = numeric(0))
    if (!nrow(d)) return(empty)
    bb <- do.call(rbind, lapply(strsplit(d$bbox, ","), as.numeric))
    out <- data.frame(text   = .ppNormalizeGlyphs(d$word),
                      x      = bb[, 1] * scale,
                      y      = bb[, 2] * scale,
                      width  = (bb[, 3] - bb[, 1]) * scale,
                      height = (bb[, 4] - bb[, 2]) * scale,
                      stringsAsFactors = FALSE)
    out[!is.na(out$text) & nzchar(trimws(out$text)), , drop = FALSE]
  })
}

# Plain OCR text, for the AI paths, which want prose rather than word boxes.
.ppOcrText <- function(pdfFile, dpi = 300, pages = NULL) {
  .ppNormalizeGlyphs(.ppOcrPages(pdfFile, dpi = dpi, pages = pages,
                                 want = "text"))
}

.ppPdfText <- function(pdfFile) {
  x <- .ppNormalizeGlyphs(pdftools::pdf_text(pdfFile))
  if (.ppPlusMinusIsDash(x)) x <- gsub(.ppPLUSMINUS, "-",x, fixed = TRUE)
  x
}

# Number of decimal places in the *printed* number (from its text, not its
# value): "63" -> 0, "61.3" -> 1, "0.71" -> 2.  Handles comma and middle-dot
# decimal separators ("61,3", "61\u00b73") used by some journals.
#
# This is the single most important quantity for a Carlisle-style analysis:
# the rounding of the reported mean determines how much of the difference
# between the reported and the reconstructed value is explained by rounding
# alone, so it is read from the glyphs rather than inferred from the number.
# Normalise printed number text to a plain machine-readable number.
#
# The comma is the hazard: European journals use it as a decimal separator
# ("61,3" = 61.3) and English ones as a thousands separator ("4,335" = 4335).
# Treating every comma as a decimal point turned a blood-loss figure of
# 4,335 ml into 4.335 - a thousand-fold error that looks perfectly plausible
# in a spreadsheet. The rule below reads it the way a person would:
#
#   * a comma followed by exactly three digits, in a number made only of such
#     groups, is a thousands separator      "4,335" -> 4335
#   * a number containing both a comma and a dot has commas as thousands
#     separators                            "1,234.5" -> 1234.5
#   * otherwise the comma is a decimal separator, as is the middle dot that
#     BJA and others print                  "61,3" / "61.3" -> 61.3
.ppNumText <- function(txt) {
  txt  <- gsub("[<>\u2212]", "", txt)
  both <- grepl(",", txt, fixed = TRUE) & grepl("[.\u00b7]", txt)
  thou <- grepl("^\\s*[-+]?\\d{1,3}(,\\d{3})+\\s*$", txt)
  txt  <- ifelse(both | thou, gsub(",", "", txt, fixed = TRUE), txt)
  gsub("[,\u00b7]", ".", txt)
}

.ppDecimals <- function(txt) {
  txt <- .ppNumText(txt)
  ifelse(grepl("\\.", txt),
         nchar(sub("^[^.]*\\.", "", txt)),
         0L)
}

# Convert printed number text to numeric (comma / middle-dot decimals,
# thousands separators, stray < > signs from "<0.001"-style cells).
.ppAsNumeric <- function(txt) {
  suppressWarnings(as.numeric(.ppNumText(txt)))
}

# Collapse repeated whitespace and trim.
.ppSquish <- function(x) trimws(gsub("\\s+", " ", x))

# Clean a row label for use as the ROW entry: drop trailing "n (%)" /
# "no. (%)" annotations, a short trailing parenthetical unit like "(kg)"
# or "(yr)" (but NOT one containing "/" - that names categories, e.g.
# "(M/F)"), and trailing separator punctuation.
.ppCleanLabel <- function(label) {
  label <- .ppSquish(label)
  label <- sub("(?i)[,;\u2014-]?\\s*(no\\.?|n)\\s*\\(%\\)\\s*$", "", label, perl = TRUE)
  label <- sub("(?i)[,;\u2014-]?\\s*\\(%\\)\\s*$", "", label, perl = TRUE)
  # trailing "(kg)", "(yr)", "(mmHg)" ... : parenthetical with no slash,
  # 8 characters or fewer inside the parentheses
  label <- sub("\\(([^/()]{1,8})\\)\\s*$", "", label)
  label <- sub("[[:space:],;:\u2014-]+$", "", label)
  .ppSquish(label)
}

# Make `nm` unique against `existing` by appending " 2", " 3", ...
.ppUniqueName <- function(nm, existing) {
  base <- nm
  k <- 2
  while (nm %in% existing) {
    nm <- paste(base, k)
    k <- k + 1
  }
  nm
}

# rbind two data frames that may not share all columns, filling the gaps with
# NA.  Used when merging deterministic and AI-derived rows, which can name
# different category columns.
.ppRbindFill <- function(a, b) {
  if (is.null(a) || nrow(a) == 0) return(b)
  if (is.null(b) || nrow(b) == 0) return(a)
  allCols <- union(names(a), names(b))
  for (cn in setdiff(allCols, names(a))) a[[cn]] <- NA
  for (cn in setdiff(allCols, names(b))) b[[cn]] <- NA
  rbind(a[, allCols, drop = FALSE], b[, allCols, drop = FALSE])
}

# The column layout the Integrity-Analysis app expects before the
# one-column-per-category block.
#
# SD and SE are separate columns, and exactly one of them is populated for any
# continuous row. This is deliberate and it matters: papers print a standard
# deviation or a standard error, never a variance, and converting one to the
# other is a MODELLING decision, not an extraction fact. Recording whichever
# was printed keeps every number traceable to a cell on the page, and leaves
# the conversion - and the small-sample bias correction that goes with it,
# since the sample SD is a biased estimator of sigma by Jensen's inequality -
# to the analysis, which applies it once, in one place (MBESS::s.u in the
# Integrity-Analysis server).
#
# ROUND_DISPERSION is the printed granularity of whichever of SD or SE was
# given. It cannot be inferred from ROUND_MEAN: a table may print "39 (4.06)".
.ppBaseColumns <- function() {
  c("TRIAL", "ROW", "N", "MEAN", "SD", "SE",
    "ROUND_MEAN", "ROUND_DISPERSION", "ROUND_OBSERVATION")
}
