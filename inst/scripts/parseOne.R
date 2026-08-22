# parseOne.R - parse a single PDF in a throwaway R process.
#
############################################################################
# Provenance                                                               #
# Written 2026-08-15 by Claude Code (model: Claude Opus 5, Anthropic) at   #
# Steve Shafer's request, after a corpus run showed that a malformed PDF   #
# can hang poppler indefinitely. Adapted for IntegrityAnalysis 2026-08-17  #
# (Claude Code, model Claude Fable 5) when the ParsePDF fold-in (PR #9)    #
# turned out to have missed inst/scripts entirely: locally the retired     #
# ParsePDF installation silently supplied this script, so everything       #
# worked; on shinyapps.io there is no ParsePDF and every PDF upload        #
# failed instantly. Found by Steve testing PR #10 with Test1.              #
#                                                                          #
# This script exists because R cannot interrupt a C library that has      #
# stopped returning: setTimeLimit only fires between R evaluations, so a  #
# hang inside poppler is unkillable in-process. Running each file in its  #
# own process makes the operating system, rather than R, responsible for  #
# enforcing the time limit.                                               #
#                                                                          #
# It is invoked by parseBaselineTableFiles(); there is no reason to run it#
# by hand. Arguments: <optionsRds> <outputRds> <pdfFile>. A .docx path    #
# flows through unchanged - parseBaselineTable() dispatches by extension  #
# internally (issue 19), so do NOT "fix" the pdfFile= name here.          #
# Status: run and verified by tests/testthat/test-batch.R, with the       #
# masking ParsePDF installation removed from the machine first.           #
############################################################################

args <- commandArgs(trailingOnly = TRUE)
opts <- readRDS(args[1])

# The parent's library paths must be adopted before the package is loaded:
# the child starts with --vanilla, and both during R CMD check and on
# shinyapps.io the package lives in a library the child would not
# otherwise see.
.libPaths(opts$libPaths)
# devPath set = the parent is a pkgload dev tree: load the same working
# tree, so the subprocess tests the code under test rather than a stale
# installed copy (2026-08-20; see parseBaselineTableFiles.R). Otherwise
# - R CMD check, shinyapps.io - load the installed package as always.
if (!is.null(opts$devPath)) {
  suppressMessages(pkgload::load_all(opts$devPath, quiet = TRUE))
} else {
  suppressMessages(library(IntegrityAnalysis))
}

res <- tryCatch(
  suppressMessages(
    do.call(IntegrityAnalysis::parseBaselineTable,
            c(list(pdfFile = args[3], quiet = TRUE), opts$args))),
  error = function(e)
    structure(list(message = conditionMessage(e)), class = "ppFailure"))

saveRDS(res, args[2])
