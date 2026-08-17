test_that("the baseline-table page outscores a prose page", {
  tableWords <- data.frame(
    text = c("Table", "1.", "Baseline", "patient", "characteristics",
             "45.3", "±", "12.1", "(n", "=", "15)"),
    stringsAsFactors = FALSE)
  proseWords <- data.frame(
    text = c("We", "enrolled", "patients", "after", "obtaining", "consent."),
    stringsAsFactors = FALSE)
  expect_gt(.ppScorePage(tableWords), .ppScorePage(proseWords))
})

test_that("words cluster into visual lines by y, ordered top to bottom", {
  words <- data.frame(
    text  = c("b2", "a1", "a2", "b1"),
    x     = c(200, 72, 200, 72),
    y     = c(150, 100, 100, 150),   # two lines, 50 pt apart
    width = 20,
    stringsAsFactors = FALSE)
  lines <- .ppBuildLines(words)
  expect_length(lines, 2)
  expect_equal(.ppLineText(lines[[1]]), "a1 a2")   # y = 100 first
  expect_equal(.ppLineText(lines[[2]]), "b1 b2")
})

test_that("small y jitter within a line does not split it", {
  words <- data.frame(text = c("a", "b"), x = c(72, 200),
                      y = c(100, 102), width = 10,   # 2 pt < yTol
                      stringsAsFactors = FALSE)
  expect_length(.ppBuildLines(words), 1)
})

test_that("column clustering finds the arm columns and assigns to the nearest", {
  mids <- c(300, 302, 298, 420, 418, 422)   # two columns ~120 pt apart
  cols <- .ppClusterColumns(mids)
  expect_equal(cols$n, 2)
  expect_equal(cols$assign(c(301, 419)), c(1L, 2L))
  expect_equal(round(cols$centers), c(300, 420))
})

test_that("jitter under the gap tolerance stays in one column", {
  cols <- .ppClusterColumns(c(300, 310, 318))
  expect_equal(cols$n, 1)
})
