test_that("decimal places are counted from the glyphs, not the value", {
  expect_equal(.ppDecimals("63"),    0)
  expect_equal(.ppDecimals("63.0"),  1)   # the trailing zero is information
  expect_equal(.ppDecimals("61.3"),  1)
  expect_equal(.ppDecimals("0.71"),  2)
  expect_equal(.ppDecimals("61,3"),  1)   # comma decimal separator
  expect_equal(.ppDecimals("61·3"),  1)   # middle-dot decimal separator
})

test_that("printed numbers convert to numeric across journal conventions", {
  expect_equal(.ppAsNumeric("61.3"),   61.3)
  expect_equal(.ppAsNumeric("61,3"),   61.3)
  expect_equal(.ppAsNumeric("61·3"),   61.3)
  expect_equal(.ppAsNumeric("<0.001"), 0.001)
  expect_true(is.na(.ppAsNumeric("NS")))
})

test_that("a thousands separator is not read as a decimal point", {
  # "4,335 ml of fluid" was being read as 4.335 - a thousand-fold error that
  # looks entirely plausible once it is in a spreadsheet.
  expect_equal(.ppAsNumeric("4,335"),   4335)
  expect_equal(.ppAsNumeric("1,234"),   1234)
  expect_equal(.ppAsNumeric("12,345"),  12345)
  expect_equal(.ppAsNumeric("1,234.5"), 1234.5)
  # ...while a genuine European decimal still reads as one
  expect_equal(.ppAsNumeric("61,3"),  61.3)
  expect_equal(.ppAsNumeric("0,75"),  0.75)

  # Decimal counting has to follow the same rule, since ROUND_MEAN is the
  # number of printed decimals
  expect_equal(.ppDecimals("4,335"),   0)
  expect_equal(.ppDecimals("1,234.5"), 1)
  expect_equal(.ppDecimals("61,3"),    1)
})

test_that("a thousands-separated number tokenises as one cell", {
  mk <- function(words) data.frame(
    text = words, x = cumsum(c(0, utils::head(nchar(words), -1) * 6 + 6)),
    width = nchar(words) * 6, stringsAsFactors = FALSE)
  tok <- .ppTokenizeLine(mk(c("Blood", "loss", "4,335", "±", "581")))
  expect_equal(nrow(tok), 1)
  expect_equal(tok$type, "meanSD")
  expect_equal(tok$num1, 4335)
  expect_equal(tok$num2, 581)
  expect_equal(tok$dec1, 0)
})

test_that("row labels are cleaned without losing category information", {
  expect_equal(.ppCleanLabel("Age (yr)"),          "Age")
  expect_equal(.ppCleanLabel("Weight (kg)"),       "Weight")
  expect_equal(.ppCleanLabel("Male sex, n (%)"),   "Male sex")
  expect_equal(.ppCleanLabel("Diabetes  no. (%)"), "Diabetes")
  # A parenthetical naming categories must survive - it is not a unit
  expect_equal(.ppCleanLabel("Sex (M/F)"),         "Sex (M/F)")
})

test_that("mis-mapped font glyphs are repaired before parsing", {
  # Anesthesiology's embedded fonts report a printed "=" as U+2AFD or U+2D1D
  # and a printed plus-minus as U+2AFE. In those PDFs the ASCII forms never
  # appear at all, so "45 +/- 12" stopped being a mean-and-SD cell and
  # "(n = 20)" stopped being an arm size - silently.
  expect_equal(.ppNormalizeGlyphs("Age, mean ⫾ SD, yr 49 ⫾ 13"),
               "Age, mean ± SD, yr 49 ± 13")
  expect_equal(.ppNormalizeGlyphs("(n ⫽ 35)"), "(n = 35)")
  expect_equal(.ppNormalizeGlyphs("(n ⴝ 20)"), "(n = 20)")
  expect_equal(.ppNormalizeGlyphs("P ⬍ 0.05"), "P < 0.05")
  expect_equal(.ppNormalizeGlyphs("⬎ 150"), "> 150")
  expect_equal(.ppNormalizeGlyphs("stored at ⫺20"), "stored at -20")
  expect_equal(.ppNormalizeGlyphs("Ca2⫹"), "Ca2+")
  expect_equal(.ppNormalizeGlyphs("␮g"), "µg")

  # Text that is already correct must pass through untouched
  expect_equal(.ppNormalizeGlyphs("45.3 ± 12.1 (n = 30)"),
               "45.3 ± 12.1 (n = 30)")
  expect_equal(.ppNormalizeGlyphs(character(0)), character(0))
})

test_that("OCR leaves no page images behind", {
  # pdftools' OCR helpers render each page to a .png in the CURRENT WORKING
  # DIRECTORY and leave it there. This package's output feeds peer-review
  # screening, where the promise is that the manuscript is deleted and nothing
  # retained - so full-page images of a submission must not survive the call.
  skip_if_not_installed("tesseract")
  f <- syntheticPdfMeanSD()

  wd <- file.path(tempdir(), "ocrcwd")
  dir.create(wd, showWarnings = FALSE)
  old <- setwd(wd); on.exit(setwd(old), add = TRUE)
  before <- list.files(wd, recursive = TRUE)

  # poppler warns that it has no display font for Symbol when rasterising the
  # fixture's plus-minus; irrelevant to what is under test here.
  txt <- suppressWarnings(.ppOcrText(f, dpi = 100, pages = 1))
  expect_type(txt, "character")

  after <- list.files(wd, recursive = TRUE)
  expect_equal(setdiff(after, before), character(0))
  expect_equal(length(list.files(wd, pattern = "[.]png$")), 0)
})

test_that("BJA's '=' look-alike is repaired", {
  expect_equal(.ppNormalizeGlyphs("p ¼ 0.04"),   "p = 0.04")
  expect_equal(.ppNormalizeGlyphs("n ¼ 503"),    "n = 503")
  expect_equal(.ppNormalizeGlyphs("des¯urane"),  "desflurane")
})

test_that("a document whose plus-minus is really a dash is detected", {
  # BJA 2003: "propofol 1.5±2.5 mg kg±1", "Creutzfeldt±Jakob", "502±6".
  # Read literally, "1.5±2.5" becomes a mean with an SD - invented data.
  bja <- c("Br J Anaesth 2003; 90: 8±13", "propofol 1.5±2.5 mg kg±1 i.v.",
           "Creutzfeldt±Jakob disease")
  expect_true(.ppPlusMinusIsDash(bja))

  # A table that genuinely reports mean ± SD must NOT be flagged
  ok <- c("Age (yr) 59 ± 11", "Height (cm) 161 ± 10",
          "Values are mean ± SD")
  expect_false(.ppPlusMinusIsDash(ok))
  expect_false(.ppPlusMinusIsDash(character(0)))
  # A single stray occurrence is not enough to condemn a document
  expect_false(.ppPlusMinusIsDash(c("mg kg±1", "Age 59 ± 11")))
})

test_that("a repaired cell tokenises as a mean and SD", {
  # The end-to-end point of the repair: before it, this line yielded four
  # unrelated plain numbers and no arm size.
  mk <- function(words) {
    x <- cumsum(c(0, utils::head(nchar(words), -1) * 6 + 6))
    data.frame(text = .ppNormalizeGlyphs(words), x = x,
               width = nchar(words) * 6, stringsAsFactors = FALSE)
  }
  tok <- .ppTokenizeLine(mk(c("Age", "49", "⫾", "13")))
  expect_equal(nrow(tok), 1)
  expect_equal(tok$type, "meanSD")
  expect_equal(tok$num1, 49)
  expect_equal(tok$num2, 13)

  # And the arm-size pattern the header parser looks for now matches
  expect_true(grepl("(?i)\\(?\\s*n\\s*=\\s*\\d+",
                    .ppNormalizeGlyphs("(n ⴝ 20)"), perl = TRUE))
})

test_that("names are made unique against what is already used", {
  expect_equal(.ppUniqueName("Other", character(0)), "Other")
  expect_equal(.ppUniqueName("Other", "Other"),      "Other 2")
  expect_equal(.ppUniqueName("Other", c("Other", "Other 2")), "Other 3")
})

test_that("rbind-fill unions the columns and pads with NA", {
  a <- data.frame(ROW = "x", Male = 1L, stringsAsFactors = FALSE)
  b <- data.frame(ROW = "y", Diabetes = 3L, stringsAsFactors = FALSE)
  m <- .ppRbindFill(a, b)
  expect_equal(nrow(m), 2)
  expect_setequal(names(m), c("ROW", "Male", "Diabetes"))
  expect_true(is.na(m$Diabetes[1]))
  expect_true(is.na(m$Male[2]))
  # Either side being empty is a no-op
  expect_identical(.ppRbindFill(a, NULL), a)
  expect_identical(.ppRbindFill(NULL, b), b)
})
