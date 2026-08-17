# End-to-end tests of the deterministic engine against synthetic PDFs.
# Ported 2026-08-15 from testParseCovariateTable.R (Integrity-Analysis).

test_that("mean +/- SD table: arms, continuous rows, and rounding", {
  pdf <- syntheticPdfMeanSD()
  res <- parseBaselineTableHeuristics(pdf, trial = "Test 1", quiet = TRUE)
  d   <- res$data

  expect_s3_class(res, "ParsePDFTable")
  expect_identical(res$arms$N, c(15L, 17L))
  expect_equal(nrow(d), 10)                 # 5 variables x 2 arms

  w <- d[d$ROW == "Weight", ]
  expect_equal(w$N[1], 15)
  expect_equal(w$MEAN[1], 63)
  expect_equal(w$SD[1], 13)
  # Weight is printed with no decimals; observations get one more digit
  expect_equal(w$ROUND_MEAN[1], 0)
  expect_equal(w$ROUND_OBSERVATION[1], 1)

  a <- d[d$ROW == "Age", ]
  expect_equal(a$MEAN[2], 46.1)
  expect_equal(a$SD[2], 11.8)
  expect_equal(a$ROUND_MEAN[2], 1)
})

test_that("mean +/- SD table: categories expand to one column per level", {
  pdf <- syntheticPdfMeanSD()
  res <- parseBaselineTableHeuristics(pdf, trial = "Test 1", quiet = TRUE)
  d   <- res$data

  s <- d[d$ROW == "Sex", ]
  expect_true(all(c("Male", "Female") %in% names(d)))
  expect_equal(s$Male,   c(10, 12))
  expect_equal(s$Female, c(5, 5))
  # The app requires categorical rows to leave N/MEAN/SD empty
  expect_true(all(is.na(s$N)))
  expect_true(all(is.na(s$MEAN)))
  expect_true(all(is.na(s$SD)))

  g <- d[d$ROW == "Type of surgery", ]
  expect_true(all(c("Upper abdominal", "Lower abdominal", "Urologic") %in% names(d)))
  expect_equal(g$`Upper abdominal`[2], 4)
  expect_equal(g$`Lower abdominal`[2], 6)
  expect_equal(g$Urologic[2], 7)
})

test_that("median [range] is skipped with a reason, and the footnote ends the table", {
  pdf <- syntheticPdfMeanSD()
  res <- parseBaselineTableHeuristics(pdf, trial = "Test 1", quiet = TRUE)

  expect_true(any(grepl("Duration", res$skipped$label)))
  expect_match(res$skipped$reason[1], "median", ignore.case = TRUE)
  # The "Values are mean ± SD ..." footnote must not become a data row
  expect_false(any(grepl("Values", res$data$ROW)))
})

test_that("mean (SD) table: the p-value column is detected and dropped", {
  pdf <- syntheticPdfMeanParen()
  res <- parseBaselineTableHeuristics(pdf, trial = "Test 2",
                                      roundObsDelta = 0, quiet = TRUE)
  d   <- res$data

  expect_equal(nrow(res$arms), 2)
  expect_identical(res$arms$N, c(40L, 42L))
  expect_equal(nrow(d), 10)

  # No p value leaked in as a mean
  means <- d$MEAN[!is.na(d$MEAN)]
  expect_false(any(means %in% c(0.55, 0.67, 0.98, 0.53, 0.71)))
})

test_that("mean (SD) table: parenthesised SDs, rounding, and n (%) rows", {
  pdf <- syntheticPdfMeanParen()
  res <- parseBaselineTableHeuristics(pdf, trial = "Test 2",
                                      roundObsDelta = 0, quiet = TRUE)
  d   <- res$data

  ag <- d[d$ROW == "Age, yr", ]
  expect_equal(ag$MEAN[1], 61.2)
  expect_equal(ag$SD[1], 10.4)
  expect_equal(ag$ROUND_MEAN[1], 1)
  # roundObsDelta = 0 makes observation rounding equal mean rounding
  expect_equal(ag$ROUND_OBSERVATION[1], ag$ROUND_MEAN[1])

  cr <- d[d$ROW == "Creatinine, mg/dL", ]
  expect_equal(cr$ROUND_MEAN[1], 2)

  # A binary n (%) row becomes a count plus its computed complement
  ms <- d[d$ROW == "Male sex", ]
  expect_equal(ms$`Male sex`,     c(24, 25))
  expect_equal(ms$`Not Male sex`, c(16, 17))
})

test_that("an explicit page argument overrides page scoring", {
  pdf <- syntheticPdfMeanSD()
  res <- parseBaselineTableHeuristics(pdf, pages = 1, quiet = TRUE)
  expect_equal(res$pages, 1)
})

test_that("a PDF with no numeric table raises an informative error", {
  f <- file.path(tempdir(), "prose.pdf")
  makeTablePdf(f, list(
    list(x = 72, y = 80,  text = "Discussion", adj = 0),
    list(x = 72, y = 110, text = "This trial found no difference between groups.", adj = 0)))
  expect_error(parseBaselineTableHeuristics(f, quiet = TRUE),
               "No usable baseline table|No table caption")
})
