# helper-multipagePdf.R - a multi-page variant of makeTablePdf, shared by
# the manuscript-layout and arm-N-recovery tests (submissions put their
# tables at the end of the document, so multi-page fixtures are the norm).
# A multi-page variant of makeTablePdf: `pages` is a list of cell lists.
makeTablePdfPages <- function(file, pages) {
  grDevices::pdf(file, width = 8.5, height = 11, encoding = "WinAnsi.enc")
  op <- graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  for (cells in pages) {
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 612), ylim = c(0, 792))
    for (cell in cells) {
      adj <- if (is.null(cell$adj)) 0 else cell$adj
      graphics::text(cell$x, 792 - cell$y, cell$text, adj = c(adj, 1), cex = 0.85)
    }
  }
  graphics::par(op)
  grDevices::dev.off()
  file
}
