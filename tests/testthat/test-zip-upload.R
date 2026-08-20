# test-zip-upload.R - the zipped upload (Steve's request, 2026-08-20):
# one archive of many trial files expands into the ordinary multi-file
# upload, defensively (AGENTS.md "Security").
#
# PROVENANCE: written by Claude Code (model Claude Opus 5), 2026-08-20.

# --- the pure entry-classification logic, fed hostile names ------------

test_that(".zipEntryPlan takes the right entries and refuses the wrong ones", {
  entries <- data.frame(
    Name = c("trial1.csv", "sub/dir/trial2.xlsx", "paper.pdf",
             "../escape.csv",              # traversal
             "C:/windows/evil.pdf",        # absolute
             "inner.zip",                  # nested archive
             "notes.txt",                  # unsupported
             "__MACOSX/._trial1.csv",      # mac junk
             "sub/",                       # directory
             "huge.pdf"),                  # oversize
    Length = c(100, 200, 5000, 100, 100, 100, 100, 100, 0,
               999 * 1024^2),
    stringsAsFactors = FALSE)
  plan <- .zipEntryPlan(entries)

  expect_true(all(plan$take[1:3]))
  expect_false(any(plan$take[4:10]))
  expect_match(plan$why[4], "unsafe path")
  expect_match(plan$why[5], "unsafe path")
  expect_match(plan$why[6], "nested archive")
  expect_match(plan$why[7], "not a supported")
  expect_identical(plan$why[8], "")    # junk drops silently
  expect_identical(plan$why[9], "")    # directories drop silently
  expect_match(plan$why[10], "larger than")
})

# --- real archive round trip -------------------------------------------

makeUploadDf <- function(paths, names = basename(paths)) {
  df <- data.frame(name = names, size = file.size(paths), type = "",
                   datapath = paths, stringsAsFactors = FALSE)
  df$ext <- tolower(tools::file_ext(df$name))
  df$stem <- tools::file_path_sans_ext(df$name)
  df
}

test_that("expandZipUploads extracts a real zip into upload rows", {
  skip_if(Sys.which("zip") == "" && !capabilities("libcurl"),
          "no zip tool")
  work <- file.path(tempdir(), "ziptest1"); dir.create(work, showWarnings = FALSE)
  a <- file.path(work, "TrialA.csv")
  b <- file.path(work, "TrialB.csv")
  write.csv(data.frame(x = 1:3), a, row.names = FALSE)
  write.csv(data.frame(x = 4:6), b, row.names = FALSE)
  zp <- file.path(work, "bundle.zip")
  old <- setwd(work); on.exit(setwd(old), add = TRUE)
  st <- tryCatch(utils::zip(zp, c("TrialA.csv", "TrialB.csv"),
                            flags = "-q"), error = function(e) -1L)
  setwd(old)
  skip_if(!file.exists(zp), "could not build a test zip on this machine")

  msgs <- character(0)
  out <- expandZipUploads(makeUploadDf(zp),
                          say = function(...) msgs <<- c(msgs, paste0(...)))
  expect_equal(nrow(out), 2)
  expect_setequal(out$stem, c("TrialA", "TrialB"))
  expect_true(all(file.exists(out$datapath)))
  # extracted under tempdir(), inside the purge guarantee's fence
  tmp <- normalizePath(tempdir(), winslash = "/")
  expect_true(all(startsWith(
    normalizePath(out$datapath, winslash = "/"), tmp)))
  # names stay attributable to the archive
  expect_match(out$name[1], "^bundle[.]zip: ")
  # the data survived the trip
  expect_equal(read.csv(out$datapath[out$stem == "TrialA"])$x, 1:3)
})

test_that("non-zip rows pass through untouched and mixes work", {
  work <- file.path(tempdir(), "ziptest2"); dir.create(work, showWarnings = FALSE)
  lone <- file.path(work, "Lone.csv")
  write.csv(data.frame(x = 9), lone, row.names = FALSE)
  df <- makeUploadDf(lone)
  out <- expandZipUploads(df, say = function(...) NULL)
  expect_identical(out, df)
})

test_that("an unreadable zip is reported, not fatal", {
  work <- file.path(tempdir(), "ziptest3"); dir.create(work, showWarnings = FALSE)
  fake <- file.path(work, "corrupt.zip")
  writeBin(as.raw(1:100), fake)
  msgs <- character(0)
  out <- expandZipUploads(makeUploadDf(fake),
                          say = function(...) msgs <<- c(msgs, paste0(...)))
  expect_equal(nrow(out), 0)
  expect_true(any(grepl("could not be read", msgs)))
})

# --- end to end through the server -------------------------------------

test_that("a zip uploaded to the server becomes a combined table", {
  # stage the zip as a COPY under tempdir() (testServer purge hazard -
  # see AGENTS.md); each csv inside carries no TRIAL column, so each
  # entry becomes its own trial named after the file
  work <- file.path(tempdir(), paste0("zipsrv", basename(tempfile(""))))
  dir.create(work)
  for (nm in c("Alpha", "Beta")) {
    write.csv(data.frame(ROW = "Age", N = c(15, 17), MEAN = c(45, 46),
                         SD = c(12, 11), ROUND_MEAN = 0,
                         ROUND_OBSERVATION = 0),
              file.path(work, paste0(nm, ".csv")), row.names = FALSE)
  }
  zp <- file.path(work, "trials.zip")
  old <- setwd(work); on.exit(setwd(old), add = TRUE)
  tryCatch(utils::zip(zp, c("Alpha.csv", "Beta.csv"), flags = "-q"),
           error = function(e) NULL)
  setwd(old)
  skip_if(!file.exists(zp), "could not build a test zip on this machine")

  shiny::testServer(app_server, {
    session$setInputs(upload = data.frame(
      name = "trials.zip", datapath = zp, stringsAsFactors = FALSE))
    d <- reactiveData()
    expect_identical(nrow(d), 4L)
    expect_setequal(unique(d$TRIAL), c("Alpha", "Beta"))
    # extracted paths joined the purge list
    expect_true(any(grepl("Alpha[.]csv$", session$env$uploadedPaths)))
  })
})
