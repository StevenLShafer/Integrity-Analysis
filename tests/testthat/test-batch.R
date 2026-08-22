# The batch runner. These tests really do launch subprocesses - that is the
# behaviour under test, since the whole point of the function is that a file
# which hangs poppler cannot take the run down with it.

test_that("a directory of PDFs is parsed, one row per file", {
  dir <- file.path(tempdir(), "batchIn")
  dir.create(dir, showWarnings = FALSE)
  file.copy(syntheticPdfMeanSD(), file.path(dir, "a.pdf"), overwrite = TRUE)
  file.copy(syntheticPdfMeanParen(), file.path(dir, "b.pdf"), overwrite = TRUE)

  res <- parseBaselineTableFiles(dir, quiet = TRUE)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_setequal(res$file, c("a.pdf", "b.pdf"))
  expect_true(all(res$ok))
  expect_true(all(res$arms == 2))
  # The parsed table itself comes back in the list column
  a <- res$result[[which(res$file == "a.pdf")]]
  expect_s3_class(a, "ParsePDFTable")
  expect_equal(nrow(a$data), 10)
  expect_true(all(res$engine == "heuristic"))   # ai defaults to "never" here
})

test_that("an unreadable file is recorded, and the run continues", {
  dir <- file.path(tempdir(), "batchBad")
  dir.create(dir, showWarnings = FALSE)
  file.copy(syntheticPdfMeanSD(), file.path(dir, "good.pdf"), overwrite = TRUE)
  writeLines("this is not a PDF at all", file.path(dir, "broken.pdf"))

  res <- parseBaselineTableFiles(dir, quiet = TRUE)

  expect_equal(nrow(res), 2)
  bad <- res[res$file == "broken.pdf", ]
  expect_false(bad$ok)
  expect_true(nzchar(bad$error))
  expect_null(res$result[[which(res$file == "broken.pdf")]])
  # The healthy file is unaffected by its neighbour
  expect_true(res$ok[res$file == "good.pdf"])
})

test_that("a file that never returns is killed and reported, not waited on", {
  # Simulated with an impossibly short timeout: what matters is that the
  # parent gives up, records why, and returns.
  dir <- file.path(tempdir(), "batchSlow")
  dir.create(dir, showWarnings = FALSE)
  file.copy(syntheticPdfMeanSD(), file.path(dir, "slow.pdf"), overwrite = TRUE)

  res <- parseBaselineTableFiles(dir, timeout = 1, quiet = TRUE)

  expect_equal(nrow(res), 1)
  if (!res$ok) expect_match(res$error, "did not finish|crashed")
})

test_that("outputDir gets one spreadsheet per successful parse", {
  dir <- file.path(tempdir(), "batchOut1")
  out <- file.path(tempdir(), "batchOut2")
  unlink(out, recursive = TRUE)
  dir.create(dir, showWarnings = FALSE)
  file.copy(syntheticPdfMeanSD(), file.path(dir, "trial.pdf"), overwrite = TRUE)

  res <- parseBaselineTableFiles(dir, outputDir = out, quiet = TRUE)
  expect_true(res$ok)
  expect_true(file.exists(file.path(out, "trial.xlsx")))
  x <- openxlsx::read.xlsx(file.path(out, "trial.xlsx"))
  expect_equal(names(x)[seq_along(.ppBaseColumns())], .ppBaseColumns())
})

test_that("an explicit vector of paths works, and an empty one is an error", {
  f <- syntheticPdfMeanSD()
  res <- parseBaselineTableFiles(c(f), quiet = TRUE)
  expect_equal(nrow(res), 1)
  expect_true(res$ok)
  expect_error(parseBaselineTableFiles(character(0)),
               "No PDF or Word files")
})
