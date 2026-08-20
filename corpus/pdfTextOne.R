# pdfTextOne.R - extract the first pages of ONE pdf to a text file.
#
# PROVENANCE: written by Claude Code (model Claude Opus 5), 2026-08-19.
# It exists only to be run as a SUBPROCESS with an OS timeout: poppler
# hangs outright on roughly 2% of real journal PDFs, and a hang inside
# the parent loop stalls the whole run with no way out (it did - a scan
# of 379 corpus PDFs stopped dead on file 26). corpus/buildParseOutcomes.R
# learned this the same way and says so in its header.
#
# Usage (not meant to be run by hand):
#   Rscript corpus/pdfTextOne.R <pdf> <outTxt> [nPages]
suppressPackageStartupMessages(library(pdftools))
a <- commandArgs(trailingOnly = TRUE)
n <- if (length(a) >= 3) as.integer(a[3]) else 2
txt <- tryCatch({
  info <- pdf_info(a[1])
  paste(pdf_text(a[1])[seq_len(min(n, info$pages))], collapse = "\n")
}, error = function(e) "")
writeLines(txt, a[2], useBytes = TRUE)
