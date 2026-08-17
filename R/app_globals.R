# app_globals.R — constants shared by the UI and server.
#
# PROVENANCE: was global.R at the repository root until the package
# restructure (phase 1, 2026-08-16); in phase 2 (same date) sumz() and
# outputComments() moved to their own files (R/sumz.R, R/outputComments.R,
# bodies untouched), leaving only the Monte Carlo replication constant here.
# The library() calls that used to open global.R live in run_app()
# (app_run.R). History for the earlier cleanup passes (2026-08-14) is in
# git; the FIX rationale comments travel with the code they explain.

# m is the replication number for the Monte Carlo simulation.
# (100000 gives smoother tails but is ~7x slower; 15000 is the value the
# deployed app has been running with.)
m <- 15000

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
