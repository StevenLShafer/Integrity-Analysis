# The app pipeline end to end, headless (ISSUES.md issue 4).
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-19,
# consolidating the scratch harnesses that verified each feature PR
# (phase1test, purgetest, combotest - see the session handoffs). Drives
# the real app_server with shiny::testServer; uploads are staged as
# COPIES in per-file subdirectories of tempdir(), the way real clients
# arrive (and the only thing the purge-on-exit handler may delete).
suppressWarnings(suppressPackageStartupMessages({
  library(shiny); library(openxlsx); library(readxl); library(Rfast)
  library(foreach); library(MBESS); library(dqrng)
  # app_ui() builds its page eagerly and calls these unqualified (they
  # are attached by run_app() in production)
  library(shinydashboard); library(shinyjs); library(shinyWidgets)
  library(bslib); library(rhandsontable); library(htmltools)
}))

stageCopy <- function(src) {
  d <- file.path(tempdir(), paste0("up", basename(tempfile(""))))
  dir.create(d)
  f <- file.path(d, basename(src))
  file.copy(src, f)
  f
}

test_that("bundled assets install with the package", {
  for (f in list(c("extdata", "Example.xlsx"),
                 c("extdata", "Template.xlsx"),
                 c("extdata", "IntegrityAnalysis.html"),
                 c("www", "Table.png")))
    expect_true(nzchar(system.file(f[1], f[2],
                                   package = "IntegrityAnalysis")))
})

test_that("app_ui() builds and points at the www/ resource prefix", {
  rendered <- htmltools::renderTags(app_ui())
  html <- paste(rendered$html, rendered$head)
  expect_match(html, "www/app.js", fixed = TRUE)
  expect_match(html, "www/app.css", fixed = TRUE)
  expect_match(html, "www/Table.png", fixed = TRUE)
  # the test-note banner appears only when asked for
  expect_false(grepl("TEST DEPLOYMENT", html, fixed = TRUE))
  withNote <- paste(unlist(htmltools::renderTags(
    app_ui(testNote = "PR #0: banner check"))), collapse = " ")
  expect_match(withNote, "TEST DEPLOYMENT", fixed = TRUE)
})

test_that("Example.xlsx runs the full pipeline; re-run does not append", {
  ex <- stageCopy(system.file("extdata", "Example.xlsx",
                              package = "IntegrityAnalysis"))
  shiny::testServer(app_server, {
    session$setInputs(upload = data.frame(
      name = "Example.xlsx", datapath = ex, stringsAsFactors = FALSE))
    expect_false(is.null(reactiveDataValidated()))
    session$setInputs(go = 1)
    out <- session$env$OUTPUT
    expect_gt(nrow(out), 0)
    # NB the !is.na guard: OUTPUT carries NA spacer rows, and indexing
    # by a condition with NAs returns NA elements
    p <- suppressWarnings(as.numeric(
      out$P[out$ROW == "Summary" & !is.na(out$ROW)]))
    expect_true(all(!is.na(p)) && all(p > 0 & p < 1))
    expect_true(isTRUE(reactiveDone()))
    # the historical regression: a second Analyze must not append
    n1 <- nrow(session$env$OUTPUT)
    session$setInputs(go = 2)
    expect_identical(nrow(session$env$OUTPUT), n1)
  })
})

test_that("uploads are purged from disk when the session ends", {
  staged <- stageCopy(system.file("extdata", "Example.xlsx",
                                  package = "IntegrityAnalysis"))
  expect_true(file.exists(staged))
  shiny::testServer(app_server, {
    session$setInputs(upload = data.frame(
      name = "Example.xlsx", datapath = staged, stringsAsFactors = FALSE))
    expect_false(is.null(reactiveDataValidated()))
  })
  # testServer closed the mock session, firing onSessionEnded
  expect_false(file.exists(staged))
  expect_false(dir.exists(dirname(staged)))   # not even the name survives
})

test_that("a parsed PDF lands in the grid, edits revalidate and analyze", {
  # the synthetic Table-1 PDF (helper-syntheticPdf.R): mean +/- SD rows,
  # an M/F category, a multi-row category, and a median [range] line the
  # parser must skip - no copyrighted articles needed
  pdfPath <- stageCopy(syntheticPdfMeanSD())
  shiny::testServer(app_server, {
    session$setInputs(upload = data.frame(
      name = "meanSD.pdf", datapath = pdfPath, stringsAsFactors = FALSE))
    d <- reactiveData()
    expect_false(is.null(d))
    expect_true(any(grepl("Age", d$ROW)))
    expect_false(is.null(reactiveDataValidated()))
    # edit a mean in the grid, revalidate: the edit must be what analysis
    # sees (the applyEdits test seam accepts a bare data.frame)
    i <- which(!is.na(d$MEAN))[1]
    d$MEAN[i] <- d$MEAN[i] + 1
    session$setInputs(dataGrid = d, applyEdits = 1)
    v <- reactiveDataValidated()
    expect_false(is.null(v))
    expect_true(any(abs(v$MEAN - d$MEAN[i]) < 1e-9, na.rm = TRUE))
    session$setInputs(go = 1)
    expect_true(any(session$env$OUTPUT$ROW == "Summary", na.rm = TRUE))
  })
})
