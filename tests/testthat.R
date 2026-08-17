# Standard testthat runner. The suite came from the ParsePDF package
# (ISSUES.md issue 9) and builds its own synthetic PDFs with the pdf()
# device (helper-syntheticPdf.R), so no copyrighted articles ship with the
# repository. App-side tests (issue 4) join it here.
library(testthat)
library(IntegrityAnalysis)

test_check("IntegrityAnalysis")
