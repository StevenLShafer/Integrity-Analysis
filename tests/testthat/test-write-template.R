test_that("the written spreadsheet meets the Integrity-Analysis contract", {
  res <- parseBaselineTableHeuristics(syntheticPdfMeanSD(), trial = "Test 1",
                                      quiet = TRUE)
  f <- file.path(tempdir(), "template.xlsx")
  writeIntegrityTemplate(res, f)
  expect_true(file.exists(f))

  # The app reads the first worksheet
  x <- openxlsx::read.xlsx(f)
  expect_equal(nrow(x), nrow(res$data))
  expect_equal(names(x)[seq_along(.ppBaseColumns())], .ppBaseColumns())

  # Category columns must be integer-valued with at least one NA - that is
  # how server.R tells a category column from a continuous one.
  for (cn in c("Male", "Female", "Urologic")) {
    v <- x[[make.names(cn)]]
    expect_true(any(is.na(v)))
    expect_true(all(v[!is.na(v)] == as.integer(v[!is.na(v)])))
  }

  # Continuous rows must be complete
  cont <- x[!is.na(x$MEAN), ]
  expect_false(anyNA(cont[, c("N", "MEAN", "SD", "ROUND_MEAN", "ROUND_OBSERVATION")]))
})

test_that("ROUND_DISPERSION would be mistaken for a category if unlisted", {
  # This pins a coupling with Integrity-Analysis's server.R, which decides a
  # column is categorical when it is numeric, has at least one NA, and holds
  # only integers. On a table with both continuous and categorical rows
  # ROUND_DISPERSION is exactly that, so server.R must list it among the known
  # columns - as ROUND_MEAN and ROUND_OBSERVATION already are. If this test
  # ever fails, the two repositories have drifted apart.
  res <- parseBaselineTableHeuristics(syntheticPdfMeanSD(), quiet = TRUE)
  d <- res$data
  isCategoryByServerRule <- function(x) {
    if (!is.numeric(x)) return(FALSE)
    if (sum(is.na(x)) == 0) return(FALSE)
    xc <- x[!is.na(x)]
    if (!length(xc)) return(FALSE)
    all(xc == as.integer(xc))
  }
  expect_true(any(is.na(d$MEAN)))          # the fixture has categorical rows
  expect_true(isCategoryByServerRule(d$ROUND_DISPERSION))
  expect_true(isCategoryByServerRule(d$ROUND_MEAN))
})

test_that("provenance and skipped rows travel with the file", {
  res <- parseBaselineTableHeuristics(syntheticPdfMeanSD(), trial = "Test 1",
                                      quiet = TRUE)
  f <- file.path(tempdir(), "template2.xlsx")
  writeIntegrityTemplate(res, f)

  expect_setequal(openxlsx::getSheetNames(f),
                  c("Template", "Provenance", "Skipped"))
  prov <- openxlsx::read.xlsx(f, sheet = "Provenance")
  expect_equal(nrow(prov), nrow(res$data))
  expect_true(all(prov$ENGINE == "heuristic"))

  skipped <- openxlsx::read.xlsx(f, sheet = "Skipped")
  expect_true(any(grepl("Duration", skipped$label)))
})

test_that("extraSheets = FALSE writes a single worksheet", {
  res <- parseBaselineTableHeuristics(syntheticPdfMeanSD(), quiet = TRUE)
  f <- file.path(tempdir(), "template3.xlsx")
  writeIntegrityTemplate(res, f, extraSheets = FALSE)
  expect_equal(openxlsx::getSheetNames(f), "Template")
})

test_that("writing rejects anything that is not a parsed table", {
  expect_error(writeIntegrityTemplate(data.frame(a = 1),
                                      file.path(tempdir(), "x.xlsx")))
})
