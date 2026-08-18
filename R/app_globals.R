# app_globals.R — constants shared by the UI and server.
#
# PROVENANCE: was global.R at the repository root until the package
# restructure (phase 1, 2026-08-16); in phase 2 (same date) sumz() and
# outputComments() moved to their own files (R/sumz.R, R/outputComments.R,
# bodies untouched), leaving only the Monte Carlo replication constant here.
# The library() calls that used to open global.R live in run_app()
# (app_run.R). History for the earlier cleanup passes (2026-08-14) is in
# git; the FIX rationale comments travel with the code they explain.

# m is the MAXIMUM replication count per row for the Monte Carlo
# simulation (the final stage of the adaptive scheme - see the header of
# R/P_Calc.R and docs/statistics.md). Rows simulate in stages
# 1,000 -> 10,000 -> m, escalating only while the running mid-p is
# < 0.01, so a typical (unalarming) row costs 1,000 replicates - CHEAPER
# than the old flat 15,000 - while alarming rows get the precision that
# makes a "<0.0001" claim defensible (the 97.5% upper confidence bound
# must clear it, which needs ~30,000+ replicates at zero exceedances).
m <- 100000

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
