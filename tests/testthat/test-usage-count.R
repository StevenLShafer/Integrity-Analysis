# Anonymous usage counting (Steve, 2026-08-19): off by default, on only
# in production, and never able to break the app.
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-19,
# with R/usageCount.R. Tests point the counter at an unreachable
# address - the real GoatCounter site must never receive counts from
# the test suite.
suppressWarnings(suppressPackageStartupMessages({
  library(shiny)
}))

test_that("counting is OFF unless run_app enabled it", {
  old <- options(IntegrityAnalysis.countUsage = NULL)
  on.exit(options(old))
  expect_false(countUsage("session"))   # returns before any network
})

test_that("an unreachable counter is swallowed silently and quickly", {
  old <- options(IntegrityAnalysis.countUsage = TRUE,
                 IntegrityAnalysis.countUrl = "http://127.0.0.1:9/count")
  on.exit(options(old))
  t0 <- Sys.time()
  expect_no_error(countUsage("session"))
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 10)
})

test_that("run_app's default wiring: production on, test apps off", {
  # the option is set from the countUsage argument, whose default is
  # is.null(testNote) - inspect the defaults without launching the app
  f <- formals(run_app)
  expect_identical(f$countUsage, quote(is.null(testNote)))
})

test_that("the server counts sessions only through the gate", {
  # with counting disabled (the default in tests), starting a server
  # session must not error and must not need the network
  old <- options(IntegrityAnalysis.countUsage = NULL)
  on.exit(options(old))
  shiny::testServer(app_server, {
    session$setInputs(blank = 1)
    expect_identical(nrow(reactiveData()), 8L)
  })
})
