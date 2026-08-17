# Rebuild the three data-entry artifacts to the SD/SE schema worked out in
# the ParsePDF sessions (canonical columns: ParsePDF:::.ppBaseColumns()):
#   TRIAL, ROW, N, MEAN, SD, SE, ROUND_MEAN, ROUND_DISPERSION,
#   ROUND_OBSERVATION, then one integer column per category.
#
#  1. inst/extdata/Template.xlsx  - header-only sheet, canonical names
#  2. inst/extdata/Example.xlsx   - existing 26 rows, renamed to canonical
#     headers, plus SE and ROUND_DISPERSION; SE demonstrated on two rows
#     (values consistent with the SD they accompany, so the file still
#     validates and analyzes cleanly - a shipped example must never fail)
#  3. inst/www/Table.png          - the format illustration, regenerated
#     with the two new columns and the SD-vs-SE warning
suppressMessages({library(openxlsx); library(grid); library(gridExtra)})
repo <- "C:/dev/IntegrityAnalysis"

## 1 ── Template ────────────────────────────────────────────────────────────
# Q1/Q3 added 2026-08-17 (median/IQR support): when both are filled in,
# MEAN is read as the MEDIAN and SD/SE must be empty.
base <- c("TRIAL", "ROW", "N", "MEAN", "SD", "SE", "Q1", "Q3",
          "ROUND_MEAN", "ROUND_DISPERSION", "ROUND_OBSERVATION")
tpl <- data.frame(matrix(nrow = 0, ncol = length(base)))
names(tpl) <- base
write.xlsx(tpl, file.path(repo, "inst/extdata/Template.xlsx"), overwrite = TRUE)

## 2 ── Example ─────────────────────────────────────────────────────────────
ex <- read.xlsx(file.path(repo, "inst/extdata/Example.xlsx"))
names(ex)[names(ex) == "RoundMean"]        <- "ROUND_MEAN"
names(ex)[names(ex) == "RoundObservation"] <- "ROUND_OBSERVATION"

# SE demonstrated on the first continuous row of each trial: value consistent
# with its SD (SE = SD / sqrt(N), at the printed precision), SD retained so
# validation still passes. ROUND_DISPERSION = printed decimals of the SD.
decimals <- function(v) {
  s <- sub("0+$", "", sub("^[^.]*\\.?", "", format(v, scientific = FALSE)))
  nchar(s)
}
cont <- !is.na(ex$SD)
ex$SE <- NA_real_
ex$ROUND_DISPERSION <- NA_real_
ex$ROUND_DISPERSION[cont] <- vapply(ex$SD[cont], decimals, numeric(1))
for (tr in unique(ex$TRIAL)) {
  i <- which(ex$TRIAL == tr & cont)[1]
  if (!is.na(i))
    ex$SE[i] <- round(ex$SD[i] / sqrt(ex$N[i]), 2)
}
if (!"Q1" %in% names(ex)) ex$Q1 <- NA_real_
if (!"Q3" %in% names(ex)) ex$Q3 <- NA_real_
# One median/IQR variable in trial 1, demonstrating the convention:
# quartiles filled -> MEAN is the median, SD/SE empty. Values are
# mildly right-skewed, as median-reported variables usually are.
if (!any(ex$ROW == "Duration", na.rm = TRUE)) {
  tr1 <- ex$TRIAL[1]
  dur <- ex[0, ]
  dur[1:2, "TRIAL"] <- tr1
  dur$ROW <- "Duration"
  dur$N <- c(25, 25)
  dur$MEAN <- c(12.0, 11.5)          # medians
  dur$Q1 <- c(8.0, 8.5)
  dur$Q3 <- c(17.0, 16.0)
  dur$ROUND_MEAN <- 1
  dur$ROUND_DISPERSION <- 1
  dur$ROUND_OBSERVATION <- 1
  ex <- rbind(ex, dur)
}
catCols <- setdiff(names(ex), base)
ex <- ex[, c(base, catCols)]
write.xlsx(ex, file.path(repo, "inst/extdata/Example.xlsx"),
           overwrite = TRUE, keepNA = FALSE)
cat("Example columns:", paste(names(ex), collapse = ", "), "\n")

## 3 ── Table.png ───────────────────────────────────────────────────────────
rows <- rbind(
  c("TRIAL",             "alphanumeric", "No",  "Unique trial identifier (not needed if only 1 trial)"),
  c("ROW",               "alphanumeric", "Yes", "Table row (e.g., 'weight', 'age')"),
  c("N",                 "integer",      "Yes", "Number of subjects for specific row entry"),
  c("MEAN",              "floating",     "Yes", "Mean - or the MEDIAN when Q1 and Q3 are filled in"),
  c("SD",                "floating",     "Yes", "Standard deviation - the analysis requires an SD"),
  c("SE",                "floating",     "No",  "Standard error, if that is what the paper reports - never in the SD column. A row with SE but no SD is flagged for you to convert (SD = SE x sqrt(N))"),
  c("Q1",                "floating",     "No",  "First quartile (25th percentile). If Q1 and Q3 are filled in, MEAN is read as the MEDIAN and SD/SE must be empty"),
  c("Q3",                "floating",     "No",  "Third quartile (75th percentile); reported with Q1 as the IQR"),
  c("ROUND MEAN",        "integer",      "No",  "Rounding (decimal places) of the printed mean"),
  c("ROUND DISPERSION",  "integer",      "No",  "Rounding of the printed SD, SE, or quartiles"),
  c("ROUND OBS",         "integer",      "No",  "Rounding for Observation")
)
tab <- as.data.frame(rows, stringsAsFactors = FALSE)
names(tab) <- c("Name", "Type", "Mandatory", "Description")

# Wrap the long SE description so the table stays within the image width.
wrap <- function(s, width = 78) paste(strwrap(s, width), collapse = "\n")
tab$Description <- vapply(tab$Description, wrap, character(1))

bg     <- "#f4f6f9"   # shinydashboard body background, as in the old image
header <- gridExtra::tableGrob(
  tab, rows = NULL,
  theme = gridExtra::ttheme_minimal(
    core = list(
      fg_params = list(hjust = 0, x = 0.02, fontsize = 15,
                       col = "#333333", fontfamily = "sans"),
      bg_params = list(fill = bg)),
    colhead = list(
      fg_params = list(hjust = 0, x = 0.02, fontsize = 16,
                       fontface = "bold", col = "#222222",
                       fontfamily = "sans"),
      bg_params = list(fill = bg)),
    padding = grid::unit(c(10, 8), "pt")))

title <- grid::textGrob(
  "Format for the data entry spreadsheet",
  x = 0.005, hjust = 0, gp = grid::gpar(fontsize = 17, fontfamily = "sans",
                                        col = "#333333"))
foot <- grid::textGrob(
  paste("Categorical variables are handled as additional columns, with the",
        "name of the column corresponding to the category\n(e.g., 'M', 'F',",
        "ASA1, ASA2, ASA3, etc). The number of columns should equal the",
        "number of categories in the analysis."),
  x = 0.005, hjust = 0, gp = grid::gpar(fontsize = 15.5, fontfamily = "sans",
                                        col = "#333333"))

png(file.path(repo, "inst/www/Table.png"), width = 1450, height = 580,
    res = 96, bg = bg)
grid::grid.newpage()
lay <- grid::grid.layout(3, 1, heights = grid::unit(c(0.065, 0.82, 0.115), "npc"))
grid::pushViewport(grid::viewport(layout = lay))
grid::pushViewport(grid::viewport(layout.pos.row = 1)); grid::grid.draw(title); grid::popViewport()
grid::pushViewport(grid::viewport(layout.pos.row = 2))
# left-justify the table like the original (tableGrob centers by default)
tw <- grid::convertWidth(sum(header$widths), "npc", valueOnly = TRUE)
grid::pushViewport(grid::viewport(x = grid::unit(0, "npc"), just = "left",
                                  width = grid::unit(tw, "npc")))
grid::grid.draw(header); grid::popViewport(); grid::popViewport()
grid::pushViewport(grid::viewport(layout.pos.row = 3)); grid::grid.draw(foot); grid::popViewport()
dev.off()
cat("Table.png written\n")
