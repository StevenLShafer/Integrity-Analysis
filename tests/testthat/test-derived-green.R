# The green-cell plumbing: values the parser DERIVED (percent conversions,
# recovered arm Ns) travel from the parse result into the app's
# parseDerived registry, keyed by the FINAL trial name, so the grid
# renderer can paint them green - "OK to use, but best to check before it
# runs" (Steve, 2026-08-21).
suppressWarnings(suppressPackageStartupMessages({
  library(shiny); library(openxlsx); library(readxl); library(Rfast)
  library(foreach); library(MBESS); library(dqrng)
}))

stageCopy <- function(src) {
  d <- file.path(tempdir(), paste0("up", basename(tempfile(""))))
  dir.create(d)
  f <- file.path(d, basename(src))
  file.copy(src, f)
  f
}

derivedPdf <- function() {
  f  <- file.path(tempdir(), "greencells.pdf")
  vx <- c(300, 420)
  cells <- c(
    list(list(x = 72, y = 80,
              text = "Table 1. Baseline patient characteristics", adj = 0)),
    rowCells(110, "", c("Ketamine", "Saline"), vx),
    rowCells(136, "", c("(n = 20)", "(n = 25)"), vx),
    rowCells(164, "Age (yr)",   c("45.3 ± 12.1", "46.1 ± 11.8"), vx),
    rowCells(190, "Male sex",   c("55%",  "52%"),  vx),
    rowCells(216, "Diabetes, %", c("25",  "20"),   vx))
  makeTablePdf(f, cells)
}

test_that("derived cells reach the registry under the final trial name", {
  pdf <- stageCopy(derivedPdf())
  shiny::testServer(app_server, {
    session$setInputs(pctApprox = TRUE)
    session$setInputs(upload = data.frame(
      name = "greencells.pdf", size = file.size(pdf),
      type = "application/pdf", datapath = pdf,
      stringsAsFactors = FALSE))
    reg <- parseDerived()
    expect_false(is.null(reg))
    expect_true(all(reg$TRIAL == "greencells"))
    # both the count column and its complement are registered
    expect_true(any(reg$COL == "Male sex"))
    expect_true(any(reg$COL == "Not Male sex"))
    expect_true(any(reg$COL == "Diabetes"))
    # the hover note carries the guidance
    expect_true(all(grepl("check against the paper", reg$note)))
    # and the registered rows exist in the displayed frame
    d <- reactiveData()
    expect_true(all(reg$ROW[reg$ROW != "*"] %in% as.character(d$ROW)))
  })
})

test_that("Start With an Empty Table clears the green registry", {
  pdf <- stageCopy(derivedPdf())
  shiny::testServer(app_server, {
    session$setInputs(pctApprox = TRUE)
    session$setInputs(upload = data.frame(
      name = "greencells.pdf", size = file.size(pdf),
      type = "application/pdf", datapath = pdf,
      stringsAsFactors = FALSE))
    expect_false(is.null(parseDerived()))
    session$setInputs(blank = 1)
    expect_null(parseDerived())
  })
})
