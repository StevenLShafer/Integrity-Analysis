# Arm-N recovery (armNRecovery.R): from the table's own n (%) cells, and
# from the document text (randomization sentences, CONSORT flow labels).
# Motivated by the 654-submission AI comparison of 2026-08-21: 583 skipped
# rows were blocked only on an unknown arm N.

# --------------------------------------------------------------------------
# The count-and-percentage bracket
# --------------------------------------------------------------------------
test_that("a printed count and percentage pin the arm size", {
  # 13 of N = 68.4% (one decimal) -> N = 19, uniquely
  expect_equal(.ppNFromCountPct(13, 68.4, 1), 19L)
  # 20 of N = 66.7% -> N = 30
  expect_equal(.ppNFromCountPct(20, 66.7, 1), 30L)
  # zero-decimal percentages are looser but often still unique
  expect_true(40L %in% .ppNFromCountPct(24, 60, 0))
  # degenerate cells give no evidence
  expect_length(.ppNFromCountPct(0, 0, 1), 0)
  expect_length(.ppNFromCountPct(5, NA, 1), 0)
})

test_that("several cells of one arm intersect to a unique N", {
  # 13/19 = 68.4%, 5/19 = 26.3%: individually loose at times, jointly unique
  expect_equal(.ppDeriveArmN(c(13, 5), c(68.4, 26.3), c(1, 1)), 19L)
  # inconsistent cells (from a junk parse) must refuse, not guess
  expect_true(is.na(.ppDeriveArmN(c(13, 5), c(68.4, 90.0), c(1, 1))))
  # a still-ambiguous set must refuse: 3 (10%) allows N = 29, 30, or 31
  expect_true(is.na(.ppDeriveArmN(c(3), c(10), c(0))))
})

# --------------------------------------------------------------------------
# Text candidates: what counts as an allocation mention
# --------------------------------------------------------------------------
test_that("allocation mentions are found; sample-size calculations are not", {
  txt <- paste("A sample size of n = 25 per group was required to detect a",
               "20% difference with 80% power. Patients were randomly",
               "allocated to the ketamine group (n = 24) or the saline",
               "group (n = 26).")
  cand <- .ppArmNCandidatesFromText(txt)
  expect_setequal(cand$n, c(24L, 26L))     # the hypothetical 25 is excluded
})

test_that("stated randomized totals are found", {
  expect_true(50L %in% .ppRandomizedTotals(
    "A total of 50 patients were randomized."))
  expect_true(112L %in% .ppRandomizedTotals(
    "We randomized 112 subjects across three centers."))
})

# --------------------------------------------------------------------------
# The assignment ladder
# --------------------------------------------------------------------------
test_that("candidates are assigned to arms by name", {
  cand <- .ppArmNCandidatesFromText(paste(
    "Patients were randomly allocated to the ketamine group (n = 24)",
    "or the saline group (n = 26)."))
  fill <- .ppFillArmNFromText(c(NA_integer_, NA_integer_),
                              c("Ketamine", "Saline"), cand, integer(0))
  expect_identical(fill$N, c(24L, 26L))
  expect_match(fill$source[1], "arm name matched")
})

test_that("the last unnamed arm is filled by elimination", {
  cand <- .ppArmNCandidatesFromText(paste(
    "allocated to the ketamine group (n = 24);",
    "the remaining patients received placebo (n = 26)."))
  fill <- .ppFillArmNFromText(c(NA_integer_, NA_integer_),
                              c("Ketamine", NA), cand, integer(0))
  expect_identical(fill$N, c(24L, 26L))
  expect_match(fill$source[2], "only unassigned")
})

test_that("positional assignment requires the stated total to confirm", {
  cand <- .ppArmNCandidatesFromText(paste(
    "randomized to treatment (n = 24) or to control (n = 26)."))
  # nameless arms, no total anywhere: refuse
  fill <- .ppFillArmNFromText(c(NA_integer_, NA_integer_), c(NA, NA),
                              cand, integer(0))
  expect_true(all(is.na(fill$N)))
  # with a confirming total: assign, and say the order is unverified
  fill <- .ppFillArmNFromText(c(NA_integer_, NA_integer_), c(NA, NA),
                              cand, 50L)
  expect_identical(fill$N, c(24L, 26L))
  expect_match(fill$source[1], "by position")
  # a total that does not match the sum: refuse
  fill <- .ppFillArmNFromText(c(NA_integer_, NA_integer_), c(NA, NA),
                              cand, 80L)
  expect_true(all(is.na(fill$N)))
})

# --------------------------------------------------------------------------
# End to end, from synthetic PDFs
# --------------------------------------------------------------------------
test_that("arm N is derived from the table's own n (%) cells", {
  f  <- file.path(tempdir(), "npct.pdf")
  vx <- c(300, 420)
  cells <- c(
    list(list(x = 72, y = 80,
              text = "Table 1. Baseline patient characteristics", adj = 0)),
    rowCells(110, "", c("Ketamine", "Saline"), vx),
    rowCells(140, "Age (yr)",       c("45.3 ± 12.1", "46.1 ± 11.8"), vx),
    rowCells(166, "Male sex",       c("13 (68.4%)",       "9 (50.0%)"),        vx),
    rowCells(192, "Diabetes",       c("5 (26.3%)",        "11 (61.1%)"),       vx),
    rowCells(218, "Hypertension",   c("7 (36.8%)",        "6 (33.3%)"),        vx))
  makeTablePdf(f, cells)
  res <- parseBaselineTableHeuristics(f, trial = "T", quiet = TRUE)
  expect_identical(res$arms$N, c(19L, 18L))
  expect_match(res$armNSource[1], "n \\(%\\) cell")
  # the recovered N flows into the continuous rows and the complements
  age <- res$data[grepl("^Age", res$data$ROW), ]
  expect_identical(age$N, c(19L, 18L))
  flags <- reviewFlags(res)
  expect_true(any(grepl("derived from the table's own printed n", flags)))
})

test_that("arm N is recovered from the randomization sentence by name", {
  f  <- file.path(tempdir(), "textn.pdf")
  vx <- c(300, 420)
  page1 <- list(
    list(x = 72, y = 100, text = "Methods", adj = 0),
    list(x = 72, y = 130,
         text = "Patients were randomly allocated to the ketamine", adj = 0),
    list(x = 72, y = 150,
         text = "group (n = 24) or the saline group (n = 26).", adj = 0))
  page2 <- c(
    list(list(x = 72, y = 80,
              text = "Table 1. Baseline patient characteristics", adj = 0)),
    rowCells(110, "", c("Ketamine", "Saline"), vx),
    rowCells(140, "Age (yr)",    c("52.4 ± 9.7", "51.8 ± 10.2"), vx),
    rowCells(166, "Weight (kg)", c("71 ± 12",    "73 ± 14"),     vx),
    rowCells(192, "Height (cm)", c("168 ± 8",    "169 ± 7"),     vx))
  makeTablePdfPages(f, list(page1, page2))
  res <- parseBaselineTableHeuristics(f, trial = "T", quiet = TRUE)
  expect_identical(res$arms$N, c(24L, 26L))
  expect_match(res$armNSource[1], "^document text")
  flags <- reviewFlags(res)
  expect_true(any(grepl("verify against the CONSORT", flags)))
})

test_that("an N printed in the table header is never overridden", {
  f  <- file.path(tempdir(), "headern.pdf")
  vx <- c(300, 420)
  page1 <- list(
    list(x = 72, y = 130,
         text = "allocated to the ketamine group (n = 99) in error", adj = 0))
  page2 <- c(
    list(list(x = 72, y = 80,
              text = "Table 1. Baseline patient characteristics", adj = 0)),
    rowCells(110, "", c("Ketamine", "Saline"), vx),
    rowCells(136, "", c("(n = 15)", "(n = 17)"), vx),
    rowCells(166, "Age (yr)", c("45.3 ± 12.1", "46.1 ± 11.8"), vx))
  makeTablePdfPages(f, list(page1, page2))
  res <- parseBaselineTableHeuristics(f, trial = "T", quiet = TRUE)
  expect_identical(res$arms$N, c(15L, 17L))
  expect_true(all(is.na(res$armNSource)))
})
