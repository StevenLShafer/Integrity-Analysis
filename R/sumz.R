# sumz.R — Stouffer's method for combining one-sided p-values.
#
# PROVENANCE: written by Claude Code (model: Claude Fable 5), 2026-08-14, to
# replace metap::sumz - the only function the app used from metap. metap
# pulls in mutoss -> multtest, a Bioconductor package, and the shinyapps.io
# image build failed fetching the matching BiocGenerics source. This local
# definition removes the whole Bioconductor dependency chain.
# VERIFIED: agrees with metap::sumz (unweighted) to within 1.2e-16 across
# 200 random cases of 2-20 p-values; the call sites (sumz(p)$p) are
# unchanged. Moved from app_globals.R to its own file in phase 2 of the
# package restructure (2026-08-16), body untouched.

#' Combine one-sided p-values by Stouffer's (sum of z) method
#'
#' Z = sum(qnorm(1 - p_i)) / sqrt(k); the combined p is the upper tail of Z.
#' Returned as `list(p = ...)` to preserve the `sumz(p)$p` call sites written
#' against `metap::sumz`.
#'
#' @param p numeric vector of one-sided p-values.
#' @return a list with a single element `p`, the combined p-value.
#' @references Stouffer SA et al. The American Soldier, vol 1. Princeton
#'   University Press, 1949.
#' @noRd
sumz <- function(p) {
  z <- qnorm(p, lower.tail = FALSE)
  list(p = pnorm(sum(z) / sqrt(length(z)), lower.tail = FALSE))
}
