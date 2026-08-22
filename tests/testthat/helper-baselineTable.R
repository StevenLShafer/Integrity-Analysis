# Shared builders for the journal-style baseline table (issue 15's
# generator, issue 17's parser).
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-21,
# for the wide-table round-trip suite (test-wide-table.R). vdShared() and
# buildShared() are the promoted forms of the vd()/build() helpers that
# test-baseline-view.R defines locally (those stay untouched - one issue
# per PR); the fixture is modeled on test-results-workbook.R's two-trial
# frame, extended with a median variable, a category variable, and a
# differing-N line so the round trip exercises every cell shape the
# generator can print.

vdShared <- function(d) shiny::isolate(validateData(d))

buildShared <- function(d) {
  v <- vdShared(d)
  stopifnot(!v$FAIL)
  buildBaselineTables(v$DATA, v$CategoryNames)
}

# The canonical round-trip fixture. Deliberate constraints, each pinned
# to what the wide format can and cannot carry (see R/parseWideTable.R):
#   - ROUND_OBSERVATION == ROUND_MEAN everywhere (the wide table never
#     prints ROUND_OBSERVATION; both readers default it to ROUND_MEAN);
#   - no SE column (the generator prints SD only);
#   - rounding columns NA on category/median lines (validateData coerces
#     NA rounding to 0, which is also what the parser emits);
#   - Height's arm-1 N (14) differs from the arm header's modal N (15),
#     so the "; n = 14" suffix is exercised both ways.
wideFixtureTwoTrials <- function() {
  data.frame(
    TRIAL = c(rep("A", 6), rep("B", 6)),
    ROW = c("Age", "Age", "Height", "Height", "Weight", "Weight",
            "Duration", "Duration", "Sex", "Sex", "Weight", "Weight"),
    N = c(15, 17, 14, 17, 15, 17, 20, 22, NA, NA, 20, 22),
    MEAN = c(45.3, 46.1, 165, 167, 63, 68, 127, 133, NA, NA, 70.2, 72.4),
    SD = c(12.1, 11.8, 7, 7, 13, 12, NA, NA, NA, NA, 10.1, 11.4),
    Q1 = c(NA, NA, NA, NA, NA, NA, 98, 101, NA, NA, NA, NA),
    Q3 = c(NA, NA, NA, NA, NA, NA, 160, 155, NA, NA, NA, NA),
    MALE = c(NA, NA, NA, NA, NA, NA, NA, NA, 10, 12, NA, NA),
    FEMALE = c(NA, NA, NA, NA, NA, NA, NA, NA, 5, 5, NA, NA),
    ROUND_MEAN = c(1, 1, 0, 0, 0, 0, 0, 0, NA, NA, 1, 1),
    ROUND_DISPERSION = c(1, 1, 0, 0, 0, 0, NA, NA, NA, NA, 1, 1),
    ROUND_OBSERVATION = c(1, 1, 0, 0, 0, 0, 0, 0, NA, NA, 1, 1),
    stringsAsFactors = FALSE)
}

# Compare two VALIDATED frames on the columns the wide format carries.
# Both come out of validateData() (sorted by TRIAL then ROW, line order
# within a variable = arm order), so row-by-row identity is the claim.
expectWideRoundTrip <- function(back, orig) {
  testthat::expect_identical(nrow(back), nrow(orig))
  testthat::expect_identical(back$TRIAL, orig$TRIAL)
  testthat::expect_identical(back$ROW, orig$ROW)
  for (cn in c("N", "MEAN", "SD", "Q1", "Q3", "MALE", "FEMALE",
               "ROUND_MEAN", "ROUND_DISPERSION")) {
    if (cn %in% names(orig)) {
      testthat::expect_true(cn %in% names(back),
                            label = paste("column", cn, "survived"))
      testthat::expect_equal(back[[cn]], orig[[cn]], ignore_attr = TRUE,
                             label = paste("column", cn))
    }
  }
}
