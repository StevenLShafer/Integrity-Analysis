# distributionGraphs.R - the PowerPoint deck of actual-vs-expected
# distributions (ISSUES.md issue 16).
#
# PROVENANCE: written by Claude Code (model Claude Opus 5), 2026-08-20,
# to Steve Shafer's design settled that day: the graphs Carlisle used in
# the 2012 Fujii analysis (PMID 22404311), where a reader SEES the
# baseline data hugging the mean more tightly than chance allows,
# rather than taking a p-value's word for it.
#
# THE DECK, three levels that differ in kind (issue 16 discussion):
#   - OVERALL (2+ trials): observed cumulative distribution of the
#     TRIAL p-values against the uniform diagonal expected under honest
#     sampling - the case-making figure of Carlisle 2012.
#   - PER TRIAL (2+ analyzable rows): the same picture within one
#     trial, over its ROW p-values.
#   - PER ROW, only under a p cutoff (default 0.01): a single row has
#     ONE observed value, so its slide shows the EXPECTED distribution
#     of the squared-error statistic - the Monte Carlo draws the engine
#     generates anyway (captured by P_Calc's collector, see
#     .stagedTail) - with a line where the observed value landed. A
#     graphical p-value: the exhibit to put in front of an author for a
#     specific variable. The cutoff keeps a 300-row analysis from
#     becoming 300 slides of wallpaper; the deck should END with the
#     smoking guns, not bury them.
#
# WHY POWERPOINT AND NOT EXCEL TABS (Steve's question, 2026-08-20):
# these graphs are exhibits - shown to editors-in-chief, integrity
# committees, an author being confronted - and slides are that habitat.
# Native Excel charts cannot express these figures cleanly (an ECDF
# against a diagonal, a density with an observed marker), and images
# pasted into worksheet tabs neither scale nor print well. The graphs
# are inserted via rvg as NATIVE OFFICE VECTOR GRAPHICS - every axis,
# step, and label individually editable in PowerPoint - which is what
# "paste as metafile" wants, done portably (EMF is a Windows-only
# device; the server is Linux).

#' A collector for P_Calc's per-row Monte Carlo draws
#'
#' Pass to [P_Calc()]'s `graphs` argument; each simulated row appends
#' `list(trial, row, kind, obs, draws, p)` to `$rows`. An environment so
#' accumulation crosses the per-trial P_Calc calls without copying.
#' @noRd
newGraphCollector <- function() {
  g <- new.env(parent = emptyenv())
  g$rows <- list()
  g
}

# ---- the three plots, as plain plotting code ---------------------------
# Base graphics, wrapped by rvg::dml at insertion. Style: spare, print-
# friendly, self-explanatory off-screen (these slides travel without a
# presenter).

.plotPvalsVsUniform <- function(p, what, subtitle) {
  p <- sort(p)
  n <- length(p)
  op <- graphics::par(mar = c(4.2, 4.2, 2.6, 1), cex = 1.1)
  on.exit(graphics::par(op))
  plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i",
       xlab = paste("p-value per", what),
       ylab = "Cumulative proportion",
       main = "")
  graphics::abline(0, 1, lwd = 2, col = "grey55", lty = 2)
  graphics::lines(c(0, p, 1), c(0, seq_len(n) / n, 1), type = "s",
                  lwd = 2.5, col = "#B2182B")
  graphics::points(p, seq_len(n) / n, pch = 16, col = "#B2182B",
                   cex = 0.9)
  graphics::legend("bottomright", bty = "n",
                   legend = c("Observed", "Expected under honest sampling"),
                   col = c("#B2182B", "grey55"), lwd = c(2.5, 2),
                   lty = c(1, 2))
  graphics::mtext(subtitle, side = 3, line = 0.3, cex = 1.0)
}

.plotRowDistribution <- function(rec) {
  xlab <- switch(rec$kind,
    continuous = "Sum of squared differences of arm means from the pooled mean",
    median     = "Sum of squared differences of arm medians from the pooled median",
    category   = "Chi-square distance of counts from expectation")
  d <- rec$draws
  op <- graphics::par(mar = c(4.6, 4.2, 2.6, 1), cex = 1.1)
  on.exit(graphics::par(op))
  h <- graphics::hist(d, breaks = "FD", plot = FALSE)
  xlim <- range(c(h$breaks, rec$obs))
  graphics::hist(d, breaks = "FD", freq = FALSE, xlim = xlim,
                 col = "grey85", border = "grey60",
                 xlab = xlab, ylab = "Density", main = "")
  graphics::abline(v = rec$obs, col = "#B2182B", lwd = 3)
  graphics::mtext(sprintf(
    "Expected distribution (%d simulations under honest sampling); observed in red.  p = %s",
    length(d), format(signif(rec$p, 3), scientific = rec$p < 1e-4)),
    side = 3, line = 0.3, cex = 1.0)
}

# ---- the deck ----------------------------------------------------------

#' Write the distribution-graphs PowerPoint
#'
#' @param results P_Calc's accumulated output (as passed to
#'   [writeResultsWorkbook()]) - source of the TRIAL p-values.
#' @param collector the [newGraphCollector()] the analysis ran with -
#'   source of row p-values (numeric even where the sheet shows
#'   "<0.0001") and of the per-row expected distributions.
#' @param file path of the .pptx to write.
#' @param rowCutoff per-row slides appear only for rows with
#'   p <= rowCutoff (issue 16: the deck ends with the smoking guns).
#' @return invisibly, the number of slides written.
#' @noRd
writeGraphsPptx <- function(results, collector, file, rowCutoff = 0.01) {
  rows <- collector$rows

  # trial p's: same carry-down walk as writeResultsWorkbook
  trial <- NA_character_
  trials <- character(0); tp <- numeric(0)
  for (i in seq_len(nrow(results))) {
    if (!is.na(results$TRIAL[i])) trial <- as.character(results$TRIAL[i])
    if (!is.na(results$ROW[i]) && results$ROW[i] == "Summary") {
      pv <- suppressWarnings(as.numeric(results$P[i]))
      if (!is.na(pv)) { trials <- c(trials, trial); tp <- c(tp, pv) }
    }
  }

  doc <- officer::read_pptx()
  nSlides <- 0L
  addGraph <- function(title, plotFun) {
    doc <<- officer::add_slide(doc, layout = "Title and Content",
                               master = "Office Theme")
    doc <<- officer::ph_with(doc, title,
                             location = officer::ph_location_type("title"))
    doc <<- officer::ph_with(doc, rvg::dml(code = plotFun()),
                             location = officer::ph_location_type("body"))
    nSlides <<- nSlides + 1L
  }

  # title slide
  doc <- officer::add_slide(doc, layout = "Title Slide",
                            master = "Office Theme")
  doc <- officer::ph_with(doc, "Integrity Analysis - distribution graphs",
                          location = officer::ph_location_type("ctrTitle"))
  doc <- officer::ph_with(doc, paste0(
    format(Sys.Date(), "%Y-%m-%d"), " - ", length(trials), " trial(s). ",
    "One-sided p toward homogeneity throughout: small p = baseline data ",
    "more alike than honest sampling allows."),
    location = officer::ph_location_type("subTitle"))
  nSlides <- nSlides + 1L

  # overall: the Carlisle 2012 figure
  if (length(tp) >= 2) {
    overall <- sumz(tp)$p
    addGraph("All trials: observed vs expected p-value distribution",
             function() .plotPvalsVsUniform(
               tp, "trial",
               sprintf("%d trials; overall Stouffer P = %s", length(tp),
                       format(signif(overall, 3),
                              scientific = overall < 1e-4))))
  }

  # per trial, over its rows
  byTrial <- split(rows, vapply(rows, function(r) r$trial, character(1)))
  for (tr in unique(vapply(rows, function(r) r$trial, character(1)))) {
    rp <- vapply(byTrial[[tr]], function(r) r$p, numeric(1))
    if (length(rp) >= 2) {
      tPee <- tp[match(tr, trials)]
      addGraph(paste0("Trial: ", tr),
               local({ rpv <- rp; tpv <- tPee; function()
                 .plotPvalsVsUniform(
                   rpv, "baseline variable",
                   sprintf("%d variables; trial P = %s", length(rpv),
                           if (is.na(tpv)) "-" else
                             format(signif(tpv, 3),
                                    scientific = tpv < 1e-4)))
               }))
    }
  }

  # per row, under the cutoff: the smoking guns
  for (rec in rows) {
    if (is.na(rec$p) || rec$p > rowCutoff || is.null(rec$draws)) next
    addGraph(paste0(rec$trial, " - ", rec$row),
             local({ r <- rec; function() .plotRowDistribution(r) }))
  }

  print(doc, target = file)
  invisible(nSlides)
}
