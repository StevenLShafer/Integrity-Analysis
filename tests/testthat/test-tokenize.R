# A line is a data frame of words with x positions; build one by hand so the
# tokenizer can be tested without a PDF.
mkLine <- function(words, x = NULL, width = NULL) {
  if (is.null(x))     x     <- cumsum(c(0, head(nchar(words), -1) * 6 + 6))
  if (is.null(width)) width <- nchar(words) * 6
  data.frame(text = words, x = x, width = width, stringsAsFactors = FALSE)
}

test_that("mean +/- SD is recognised however poppler split the cell", {
  for (words in list(c("45.3", "±", "12.1"), c("45.3±12.1"), c("45.3", "±12.1"))) {
    tok <- .ppTokenizeLine(mkLine(words))
    expect_equal(nrow(tok), 1)
    expect_equal(tok$type, "meanSD")
    expect_equal(tok$num1, 45.3)
    expect_equal(tok$num2, 12.1)
    expect_equal(tok$dec1, 1)
  }
  tok <- .ppTokenizeLine(mkLine(c("45.3", "+/-", "12.1")))
  expect_equal(tok$type, "meanSD")
})

test_that("each cell shape gets its own token type", {
  expect_equal(.ppTokenizeLine(mkLine(c("15", "(60%)")))$type,      "nPct")
  expect_equal(.ppTokenizeLine(mkLine(c("45.3", "(12.1)")))$type,   "numParen")
  expect_equal(.ppTokenizeLine(mkLine("15/10"))$type,               "fraction")
  expect_equal(.ppTokenizeLine(mkLine("12/8/5"))$type,              "fraction")
  expect_equal(.ppTokenizeLine(mkLine("60%"))$type,                 "pctOnly")
  expect_equal(.ppTokenizeLine(mkLine("45.3"))$type,                "plain")
})

test_that("median with a range is recognised so it can be skipped", {
  for (words in list(c("127", "[98-160]"), c("127", "(98", "to", "160)"),
                     c("127", "(98-160)"))) {
    tok <- .ppTokenizeLine(mkLine(words))
    expect_equal(tok$type[1], "medianRng")
  }
})

test_that("digits inside words are not mistaken for cells", {
  expect_equal(nrow(.ppTokenizeLine(mkLine(c("SpO2", "CO2")))), 0)
  expect_equal(nrow(.ppTokenizeLine(mkLine(c("Age", "yr")))), 0)
})

test_that("token x extents come back from the word coordinates", {
  line <- mkLine(c("Age", "45.3", "±", "12.1"))
  tok  <- .ppTokenizeLine(line)
  expect_equal(tok$x0, line$x[2])
  expect_equal(tok$x1, line$x[4] + line$width[4])
  expect_equal(tok$mid, (tok$x0 + tok$x1) / 2)
})

test_that("a line of several cells tokenises left to right", {
  tok <- .ppTokenizeLine(mkLine(c("Age", "45.3", "±", "12.1", "46.1", "±", "11.8")))
  expect_equal(nrow(tok), 2)
  expect_equal(tok$num1, c(45.3, 46.1))
  expect_true(tok$mid[1] < tok$mid[2])
})
