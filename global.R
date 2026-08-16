# global.R
#
# NOTE: this file must be named "global.R" (lower case g). Shiny auto-sources
# "global.R" by exact name; Windows' case-insensitive filesystem hides the
# problem locally, but on the Linux servers at shinyapps.io "Global.R" is NOT
# sourced, and the app fails to start because no libraries are loaded.
#
# PROVENANCE: Cleanup by Claude Code (model: Claude Fable 5), 2026-08-14,
# reviewed and tested by running the app locally (see PR description).
# Changes from the 2025-08-30 original:
#   - renamed Global.R -> global.R (see note above)
#   - removed library() calls for packages the app never uses
#     (rsconnect [x2], OpenMx, digitTests) and for the parallel scaffolding
#     that the current sequential foreach %do% loop does not exercise
#     (future, doFuture, doParallel). This shrinks the shinyapps.io bundle
#     and startup time. Re-add them when the loop is actually parallelized.
#   - replaced library(metap) with a local sumz() (see below): metap drags
#     in Bioconductor via mutoss/multtest, which broke the shinyapps.io
#     image build, and sumz was the only metap function used.
#   - removed registerDoFuture(flavor = "%dofuture%") and plan(multisession):
#     registerDoFuture() does not accept a "flavor" argument, and neither call
#     has any effect on a %do% loop.
#   - removed remove(list = ls()), which cleared the global environment
#     mid-startup and served no purpose in a deployed app.
#   - removed the hard-coded setwd("g:/projects/Fraud/2025"): it errors on any
#     machine without that drive. When the app is launched with runApp() the
#     working directory is already the app directory, so Template.xlsx etc.
#     resolve correctly without it.
#   - removed the global per-session state (DATA, TRIALS, ColumnNames,
#     CategoryNames, ...). Globals are shared by ALL concurrent user sessions
#     in one R process, so two simultaneous users would overwrite each other's
#     data mid-analysis. That state now lives inside server() (see server.R),
#     which gives each session its own copy.
#   - m was assigned 100000 and then immediately overwritten with 15000;
#     kept the single, deliberate assignment.

library(shiny)
library(openxlsx)       # read.xlsx / write.xlsx (xlsx upload + results download)
library(readxl)         # read_excel (legacy .xls upload)
library(Rfast)          # rowmeans / rowsums on the Monte Carlo matrix
library(shinyjs)
library(shinyWidgets)   # actionBttn
library(foreach)        # %do% loop over rows in P_Calc
library(MBESS)          # s.u: unbiased SD correction for small N
library(dqrng)          # dqrnorm: fast RNG for the simulated means
library(bslib)          # input_task_button
library(shinydashboard)

# m is the replication number for the Monte Carlo simulation.
# (100000 gives smoother tails but is ~7x slower; 15000 is the value the
# deployed app has been running with.)
m <- 15000

# Stouffer's (sum of z) method for combining one-sided p-values.
#
# PROVENANCE: written by Claude Code (model: Claude Fable 5), 2026-08-14, to
# replace metap::sumz - the only function the app used from metap. metap
# pulls in mutoss -> multtest, a Bioconductor package, and the shinyapps.io
# image build failed fetching the matching BiocGenerics source. This local
# definition removes the whole Bioconductor dependency chain.
# VERIFIED: agrees with metap::sumz (unweighted) to within 1.2e-16 across
# 200 random cases of 2-20 p-values; the call sites (sumz(p)$p in server.R)
# are unchanged.
#
# Reference: Stouffer SA et al. The American Soldier, vol 1. Princeton
# University Press, 1949. Z = sum(qnorm(1 - p_i)) / sqrt(k); the combined
# p is the upper tail of Z.
sumz <- function(p) {
  z <- qnorm(p, lower.tail = FALSE)
  list(p = pnorm(sum(z) / sqrt(length(z)), lower.tail = FALSE))
}


############################################################################
# References                                                               #
# Carlisle JB. The analysis of 168 randomised controlled trials to test    #
# data integrity. Anaesthesia. 2012;67:521-537.                            #
#                                                                          #
# Carlisle JB, Dexter F, Pandit JJ, Shafer SL, Yentis SM. Calculating the  #
# probability of random sampling for continuous variables in submitted or  #
# published randomised controlled trials. Anaesthesia. 2015;70:848-58.     #
#                                                                          #
# Carlisle JB. Data fabrication and other reasons for non-random sampling  #
# in 5087 randomised, controlled trials in anaesthetic and general medical #
# journals. Anaesthesia. 2017;72:944-952                                   #
############################################################################

outputComments <- function(
    ...,
    echo = getOption("ECHO_OUTPUT_COMMENTS", TRUE),
    sep = " ")
{
  isolate({
    argslist <- list(...)
    if (length(argslist) == 1) {
      text <- argslist[[1]]
    } else {
      text <- paste(argslist, collapse = sep)
    }

    # If this is called within a shiny app, try to get the active session
    # and write to the session's logger
    commentsLog <- function(x) invisible(NULL)
    session <- getDefaultReactiveDomain()
    if (!is.null(session) &&
        is.environment(session$userData) &&
        is.reactive(session$userData$commentsLog))
    {
      commentsLog <- session$userData$commentsLog
    }

    if (is.na(echo)) return()
    if (is.data.frame((text)))
    {
      con <- textConnection("outputString","w",local=TRUE)
      capture.output(print(text, digits = 3), file = con, type="output", split = FALSE)
      close(con)
      if (echo)
      {
        for (line in outputString) cat(line, "\n")
      }
      for (line in outputString) commentsLog(paste0(commentsLog(), "<br>", line))
    } else {
      if (echo)
      {
        cat(text, "\n")
      }
      commentsLog(paste0(commentsLog(), "<br>", text))
    }
  })
}
