<!--
  user-guide.md - THE user documentation for IntegrityAnalysis.

  PROVENANCE: rewritten as Markdown by Claude Code (model Claude Fable 5),
  2026-08-19, at Steve Shafer's request (ISSUES.md issue 14), from Steve's
  Word/PDF original (G:\Projects\Fraud\2025\Old Files\IntegrityAnalysis.docx,
  last edited 2025-09-01). The background narrative is Steve's text, lightly
  edited; every section describing the app was updated to the 2026-08 state:
  one-sided p toward homogeneity, adaptive replicates, the editable grid,
  color-coded cells, median/IQR rows, PDF parsing, multi-file upload, the
  purge guarantee, and the journal-style baseline table download.
  Zipped multi-file upload added 2026-08-20.

  TO REGENERATE THE SERVED HTML after editing this file:
    "C:\Program Files\Quarto\bin\tools\pandoc.exe" docs/user-guide.md
      -s --embed-resources --toc --metadata title="IntegrityAnalysis"
      -c docs/user-guide.css -o inst/extdata/IntegrityAnalysis.html
  (one line; see AGENTS.md). The same file is published by
  .github/workflows/pages.yaml as https://integrityanalysis.io/guide.html,
  which the app's sidebar "View Documentation" link opens.
-->

**A Shiny implementation of the Carlisle–Shafer Monte Carlo analysis of
RCT baseline data.**

The application runs at
<https://steveshafer.shinyapps.io/IntegrityAnalysis/>. The code is open
source at <https://github.com/StevenLShafer/IntegrityAnalysis>.

**Privacy: nothing you upload or enter is retained.** The uploaded PDF or
spreadsheet, any data typed into the table, and the analysis results are
all purged when the session closes. No record of the analysis is kept.
Manuscripts under review are confidential, and the app is built around
that: uploaded files are deleted from disk when the session ends,
downloads are generated straight into your browser, and nothing is
logged. The deployed analysis is fully deterministic and offline — no
document content is ever sent to any third-party service.

**Usage counting:** I tabulate the number of times IntegrityAnalysis is
opened and the number of analyses run — simple counts, and nothing
else. Not even your IP address reaches the counter, because the count
is sent by the server, not by your browser. I want to know whether the
program is being used: there is no point maintaining a program that
nobody uses. — *Steve Shafer*

# Background

For about 15 years, papers published by Yoshitaka Fujii had been
considered sketchy. In 2000, Kranke and colleagues wrote a letter to
Ronald Miller, Editor-in-Chief of *Anesthesia & Analgesia*, with the
snarky title: "Reported data on granisetron and postoperative nausea and
vomiting by Fujii et al. are incredibly nice!" The authors pointed out
the impossibility of nearly every group in every randomized controlled
trial published by Fujii having one headache as an adverse event. Dr.
Fujii's response was "the data are the data." Nothing more was done.

In 2012, John Carlisle published a landmark paper, "The analysis of 168
randomised controlled trials to test data integrity" [1]. Carlisle's
insight was that baseline data in a randomized controlled trial are
samples of the same pre-treatment population. The differences between
sample means reflect the standard deviations within the underlying study
population. One can test whether the means of two groups (say, a control
arm and a treatment arm) are "too close."

For example, say the mean weight of a control group of 6 subjects is
77 ± 30 kg and the mean weight of a treatment group of 6 subjects is
78 ± 30 kg. With a standard deviation of 30, the standard error of each
mean is about 12 (30/√6). Each group mean is just 0.5 kg from the grand
mean of 77.5. A two-tailed t test yields p = 0.96: 96% of the time, the
means would be this far apart *or further*. They are certainly not far
enough apart to suggest different populations.

It is a different question whether they are **too close**. If random
chance says that 96% of the time the means would be further apart, then
only 4% of the time would they be this close. Seen once, that is no big
deal — it is expected in 4% of random samples. Seen again and again, in
variable after variable and trial after trial, something is definitely
amiss.

Reviewing 168 papers by Fujii, Carlisle found far too many baseline
means that were too close. The joint p value across the Fujii trials was
about 10⁻³³. Carlisle's analysis unmasked years of data fabrication and,
at last count, the retraction of 172 papers [2].

There were two technical problems. First, the conventional standard
deviation — the square root of the unbiased variance — is itself a
biased estimate of the population standard deviation. The bias is modest
[3] and did not affect Carlisle's conclusions; this app corrects for it
(see *Statistical details* below). The more significant problem is
**rounding**. Suppose both groups report a mean weight of 77. The
difference between the groups is 0, and under normal statistical theory
a difference of exactly 0 between two random samples is impossible — the
p value degenerates. In the real world it happens all the time, because
published data are rounded.

The way out is to abandon normal theory and instead **replicate the
study by Monte Carlo simulation**, rounding the simulated data exactly
as the published table was rounded. John Carlisle and Steve Shafer spent
several years developing this method and in 2015 published a re-analysis
of the Fujii data [4]: same verdicts, and in simulation the Monte Carlo
approach proved more robust than normal theory. In 2017, Carlisle
applied the approach to 5,087 randomized controlled trials from eight
journals [6]. This app is the current implementation of that method, and
its engine has been validated line by line against Carlisle's 2017
results (see *Validation* below).

# Quick start

1. Open <https://steveshafer.shinyapps.io/IntegrityAnalysis/>.
2. Get your baseline table into the app by any of the four routes below
   (spreadsheet upload, article PDF upload, several files at once, or
   typing into an empty table).
3. Review the table in the editable grid. Fix anything colored (see
   *The data grid* below), then click **Apply Edits & Revalidate**.
4. When the table validates, click **Analyze**. Each trial's p value
   appears as it completes.
5. Download the results, the current table (a valid input file for a
   later session), and the reconstructed baseline table (the
   journal-style view, for comparison against the manuscript).

## The five ways in

**A template spreadsheet.** Download the Template (sidebar), fill it in
(Excel `.xlsx`/`.xls` or `.csv`), and upload it. The Example download
shows a complete, working file. The format is described in *Preparing
your data* below.

**An article PDF.** Upload the article; the app finds the baseline
table ("Table 1") in the text layer and extracts it into the grid. The
extraction is deterministic — the same PDF always yields the same table
— and entirely local. Median rows are extracted too, **when the table
says the bracketed interval is an IQR** (in the row label, caption, or
footnote); a median with a min–max range, or with an unlabeled
interval, is flagged for hand entry instead — the analysis needs
quartiles, and the app will not guess. Extraction is imperfect by
nature: table lines the reader could not use appear as red-flagged rows
in the grid (fill them in from the paper, or delete them), and a
scanned image with no text layer fails cleanly with a message. Whatever
was extracted can be reviewed, corrected, and analyzed without leaving
the app.

**Several files at once — or one after another.** Any mix of
spreadsheets and PDFs in one selection, and any number of uploads in
sequence: **each upload appends to the table already in the grid**
(including edits you have typed but not yet revalidated). Every file
becomes rows in the combined table, distinguished by the TRIAL column; a
file without trial identifiers gets its file name as the trial. If a new
file uses trial labels already in the table, its labels are prefixed
with its file name so nothing silently merges. To start over, click
**Start With an Empty Table**.

**A zip archive of a whole analysis.** Zip any number of spreadsheets
and article PDFs into one `.zip` and upload just that. This is built
for reproducing a multi-trial investigation — the pattern of Carlisle's
2012 review of Fujii's 168 trials
([PMID 22404311](https://pubmed.ncbi.nlm.nih.gov/22404311/)): put one
file per trial in the archive and every entry becomes its own trial in
the combined table, named after its file. Folders inside the archive
are fine (only the file names are used); files that are not
csv/xls/xlsx/PDF are skipped with a note, an archive inside the archive
is not expanded, and a corrupt archive is reported rather than
analyzed. Limits: 300 files and 300 MB uncompressed per archive.

**An empty table.** Click **Start With an Empty Table** and type the
data straight into the grid — eight empty rows and placeholder category
columns (CAT1–CAT3) to start. Add rows with the right-click menu or the
**Add 5 Rows** button; add a named column with **Add Column**.

# The data grid

Every input route lands in the same editable grid, and the grid is the
data of record: what you see is exactly what will be analyzed. Edit any
cell, add or delete rows (right-click), and click **Apply Edits &
Revalidate** to re-check the table.

Validation reports problems by **coloring the cells** — there is no
error text to read. A legend under the grid explains the colors whenever
any cell is painted, and hovering over a painted cell explains that
specific cell:

- **Yellow — missing.** A required value is empty. Enter it, or delete
  the row. Rows with a label but no data at all are left out of the
  analysis (they do not block it).
- **Red — unreadable.** Text where a number belongs — for example
  "n/a" in an SD cell of an uploaded spreadsheet — or a table line the
  PDF reader saw but could not use. For a PDF line, hover over the red
  cell to see the reader's reason (for example, "median [range] —
  integrity analysis needs mean and SD").
- **Blue — incongruent.** The value conflicts with the type of its row:
  an SD on a median/IQR row, continuous entries on a category row, a
  median outside its own quartiles, an SE standing in for a missing SD.

A table with no colors and no legend validated cleanly, and the
**Analyze** button appears.

# Preparing your data

Each line of the table is one cell of the manuscript's baseline table:
one variable in one study arm. Lines that share a ROW label are the arms
of that variable.

## Continuous variables (mean and SD)

Four columns are required:

| Column | Meaning |
|---|---|
| `ROW`  | what is measured — "Weight", "Age", "Duration of surgery" |
| `N`    | number of subjects in the group |
| `MEAN` | the group mean, exactly as printed |
| `SD`   | the group standard deviation, exactly as printed |

A study with one baseline variable (weight) and three arms:

| ROW | MEAN | N | SD | ROUND OBSERVATION | ROUND MEAN |
|---|---|---|---|---|---|
| Weight | 72 | 100 | 10 | 0 | 1 |
| Weight | 76 | 50  | 12 | 0 | 1 |
| Weight | 64 | 50  | 25 | 0 | 1 |

The two rounding columns tell the simulation how the published numbers
were rounded — the heart of the method. `ROUND OBSERVATION` is the
precision of the raw data (0 = integers); `ROUND MEAN` is the decimal
places of the printed mean. If omitted, the app infers them from the
decimal places of the values themselves. An optional `ROUND DISPERSION`
column gives the printed precision of the SD when it differs from the
mean's (a table may print "39 (4.06)").

If the paper reports a **standard error** instead of a standard
deviation, enter it in an `SE` column. The analysis needs an SD; the app
deliberately does not convert for you (the conversion needs N and is a
decision about the analysis), and validation will point at the row — the
SE cell paints blue, the SD cell yellow.

## Median and quartiles

Many papers report skewed variables as median [Q1, Q3]. Enter these
with two additional columns, `Q1` and `Q3`. On a row where both
quartiles are filled in, **the MEAN column holds the median**, and the
SD and SE cells must be empty. The simulation for such rows draws from a
distribution fitted to the three quartile values (a metalog
distribution), so no normality assumption is imposed. The median must
lie between its quartiles; N, the median, and both quartiles are
required.

Medians reported with a **range** rather than quartiles cannot be used —
the range of a sample carries almost no information about the spread of
the population. Such lines are refused with an explanation.

## Categorical variables (counts)

Categorical variables — sex, ASA class, type of surgery — are counts,
entered as **additional named columns**. From the example spreadsheet:

| ROW | N | MEAN | SD | Male | Female | Upper | Lower | Urologic |
|---|---|---|---|---|---|---|---|---|
| Sex | | | | 40 | 10 | | | |
| Sex | | | | 34 | 16 | | | |
| Surgery | | | | | | 7 | 8 | 35 |
| Surgery | | | | | | 7 | 15 | 28 |
| Weight | 15 | 63 | 13 | | | | | |
| Weight | 17 | 68 | 12 | | | | | |

Two arms: the first has 40 men and 10 women, the second 34 and 16. On a
category row, `N`, `MEAN`, and `SD` must be blank (blue cells point at
violations). A column is recognized as categorical when it is numeric,
integer-valued, and not filled on every line.

## Multiple trials

A `TRIAL` column separates trials; each is analyzed independently. With
no TRIAL column, everything is one trial. Lines of one variable need not
be adjacent — a (TRIAL, ROW) pair defines the variable, wherever its
lines sit. The same ROW name ("Age") can appear in any number of trials.

## Column-name flexibility

Column names are case-insensitive and trimmed. The first column
containing "TRIAL" becomes TRIAL and "ROW" becomes ROW — so "My Trial
ID" or "Row Label" are understood. `MEAN`, `N`, and `SD` must be named
exactly that (any capitalization); an *additional* column containing
"MEAN" becomes ROUND MEAN, and the first column containing "OBS" becomes
ROUND OBSERVATION. Carlisle's 2017 spreadsheet columns (`MEASURE`,
`DECM`, `NUMBER`) are also accepted. Unrecognized columns are carried
along untouched and ignored by the analysis.

# The analysis

## The p value: one-sided, toward homogeneity

For each variable, the app simulates the trial many times: for every
arm, N subjects are drawn from a normal distribution with the pooled
mean and (bias-corrected) SD; each simulated observation is rounded like
the raw data; each simulated mean is rounded like the printed mean; and
the weighted sum of squared deviations of arm means from the grand mean
is computed. The **p value is the fraction of simulations at least as
homogeneous as the reported data** (a mid-p: ties count half). Small p
means the printed means are closer together than random sampling can
readily explain — the Fujii signature. This is deliberately one-sided:
the earlier practice of doubling the proximity p was, in retrospect, a
mistake, and this implementation reports the one-sided value only.

A small p value is a **screening signal, not a verdict**. Innocent
explanations include stratified or blocked allocation, correlated
variables, mislabeled SEMs, and transcription errors — Carlisle's 2017
paper [6] discusses them at length. A trial flagged here deserves
scrutiny of the original data, not summary judgment.

**CAVEAT: Chance alone will produce P ≤ 0.05 in 1 in 20 papers, and
P ≤ 0.01 in 1 in 100 papers. Research fraud should never be alleged by
a single manuscript flagged by IntegrityAnalysis. Confirmation such as
multiple suspicious papers (e.g., Fujii, Boldt) should be sought.
Authors or journal editors should be contacted before any public
allegations of research fraud. Journals do not have the authority,
resources, or responsibility for investigating fraud. Journal editors
should refer allegations of fraud to the institution under whose
authority the research was conducted. Institutions are responsible for
ethical conduct of research.**

## How many simulations? (adaptive replicates)

Every row starts with 1,000 replicates. Rows that look unremarkable stop
there; rows running alarming (mid-p < 0.01) escalate to 10,000, and if
still alarming to 100,000. This spends computation where it matters —
boring rows finish fast, alarming rows get precise p values.

With a finite number of replicates, the smallest honestly reportable p
is bounded. A row's p is displayed as **"<0.0001"** only when the upper
95% confidence bound on the p value itself clears 1 in 10,000 —
otherwise the display shows the estimate with its bound. Rows with
p < 0.001 carry an explicit upper bound in the results ("≤ …"), so you
always know how much Monte Carlo noise is in a small p.

## Combining rows into a trial p

The row p values of a trial are combined with Stouffer's Z method [5] —
the same method used in the 2015 and 2017 papers. The combined value is
*not* floored at the single-row resolution: ten rows each at p = 0.02
legitimately combine to a far smaller number. When the trial p is below
0.001, it is reported with a **95% Monte Carlo interval** — the
uncertainty in the trial p arising from the finite simulation counts of
its rows — so a headline number like 3 × 10⁻⁶ is never presented as more
precise than the simulation supports.

## Categorical variables

Category tables are simulated by drawing random tables with the same
margins (row and column totals) as the reported one, and asking how
often the simulated table is at least as homogeneous (chi-square
statistic at least as small) as reported — the same one-sided, mid-p
convention as the continuous rows. Degenerate tables (an arm with no
counts, an empty category) are refused with an explanation rather than
analyzed.

## Statistical details

- Reported SDs are corrected for small-sample bias before simulation
  (the standard deviation of a sample underestimates the population
  value; the correction is the standard Γ-function factor [3]).
- Simulated observations are rounded to `ROUND OBSERVATION` decimals and
  simulated means to `ROUND MEAN` decimals, so the simulation reproduces
  the granularity of the printed table — including printed means that
  tie exactly.
- Median/IQR rows are simulated from a three-term metalog distribution
  fitted to (Q1, median, Q3); the simulated sample's quartiles are
  rounded like the printed ones.
- Very large trials are protected against memory exhaustion by chunking
  the simulation matrices; results are identical, only the batch size
  changes.

# Results and downloads

While the analysis runs, each trial's name and p value are written to
the log as it completes. (The user interface is otherwise occupied
during a long run — live progress display is a known limitation on the
roadmap.)

**Download Results** — one workbook, three worksheets. Together they
answer three different questions: what happened line by line, what the
app believed the data were, and what to report. Ticking **Graph
results** before downloading adds a PowerPoint of actual-vs-expected
distribution graphs, and the download becomes a zip holding both files
(described after the worksheets below).

*Sheet 1, `Test Results`* — the audit trail: every line the analysis
touched, in the order it ran, with a **Summary** row closing each trial
and a blank row between trials.

| Column | Meaning |
|---|---|
| `TRIAL` | the trial identifier, as it appeared in the grid. Blank on the Summary row, which prints beneath its own trial's rows |
| `ROW` | the variable identifier for that line, or `Summary` |
| `P (one-sided toward homogeneity)` | the mid-p described above — small means *more homogeneous than chance*. On the Summary row this is the Stouffer-combined trial p |
| `95% Monte Carlo bound` | how precisely the simulation pinned that number: an upper bound on a row p too small to resolve, and on the Summary row a bootstrap interval for the trial p when it fell below 0.001. Blank when the estimate needs no caveat |
| `Replicates` | how many simulations that row actually received (1,000 / 10,000 / 100,000 — the adaptive scheme stops as soon as the p is resolved, so most rows show 1,000) |

*Sheet 2, `Baseline Tables`* — the reconstruction, one block per trial
stacked down the sheet under a bold `Trial: <name>` heading: variables
as rows, arms as columns, exactly as a journal prints Table 1.

- Column headers carry each arm's N (`Arm 1 (n = 15)`); a line whose own
  N differs — dropouts, missing data — says so in its own cell
  (`; n = 14`).
- A mean/SD variable prints as `mean (SD)`; a median/IQR variable prints
  as `median [Q1, Q3]`; the row label says which.
- A categorical variable becomes a heading line (`Sex, n`) with one
  indented line per category, carrying the counts.
- Every number is formatted at the **printed precision the analysis
  assumed** (the rounding columns), so this sheet is the direct
  comparison against the manuscript page: if it disagrees with the page,
  so did the analysis.

*Sheet 3, `Summary`* — one line per study: the trial name, its combined
`P (one-sided toward homogeneity)`, and the `95% Monte Carlo interval`
where one was reported. When the analysis holds **two or more trials**,
a closing bold row gives the **overall P for the entire analysis** — the
same Stouffer (sum-of-z) combination the app applies within each trial,
now applied across the trial P values. This is the step Carlisle took to
reach a single p for the whole body of Fujii's work
([PMID 22404311](https://pubmed.ncbi.nlm.nih.gov/22404311/)): each trial
may look only mildly improbable, but improbability *accumulates*, and
the overall P is where a pattern across an author's trials becomes
visible. Trials whose P could not be computed (`No values`) are left
out, and the row's label says how many combined. This is the sheet to
keep when screening many manuscripts — the per-line detail stays in
sheet 1 for the ones worth a second look.

**Graph results — the PowerPoint of distributions.** Tick the box next
to Download Results (before or after the analysis — it only changes
what the download builds) and the download delivers a zip: the workbook
above plus `Integrity Analysis Graphs.pptx`. The deck shows the
analysis the way Carlisle's 2012 Fujii paper did
([PMID 22404311](https://pubmed.ncbi.nlm.nih.gov/22404311/)) — you see
the data hugging the mean more tightly than chance allows, instead of
taking a p-value's word for it:

- **All trials** (when there are two or more): the observed cumulative
  distribution of the trial p-values against the diagonal expected
  under honest sampling, annotated with the overall Stouffer P.
- **One slide per trial**: the same picture within the trial, over its
  baseline variables.
- **One slide per suspicious variable** (p ≤ 0.01): the expected
  distribution of the squared-error statistic — the Monte Carlo draws
  the analysis generated anyway — with a red line where the observed
  value landed. A graphical p-value: the exhibit to put in front of an
  author about a specific variable. Only flagged variables get slides,
  so the deck ends with the evidence rather than burying it.

Every graph is inserted as native, editable PowerPoint drawing objects
— axes, bars, and labels can be restyled in PowerPoint for a
presentation or a report figure.

**Download Table** — the current grid as a spreadsheet. This is a valid
input file: for a partially extracted PDF it is the round trip (fill the
gaps in Excel, re-upload), and for hand-typed data it is the checkpoint,
since nothing is retained between sessions.

**Download Baseline Table (journal view)** — a reconstruction of the
baseline table as a journal would print it: variables as rows, arms as
columns, cells as "mean (SD)", "median [Q1, Q3]", or category counts,
one worksheet per trial. Every value is formatted at the printed
precision the analysis assumed, and column headers carry each arm's N.
This is the artifact to lay beside the manuscript's Table 1: it shows
exactly what IntegrityAnalysis believed the baseline data were. If the
reconstruction disagrees with the page, so did the analysis — fix the
grid and rerun.

# Validation

The engine has been validated at two levels.

**Against Carlisle 2017.** John Carlisle generously provided the
spreadsheet of continuous baseline variables behind his 5,087-trial
analysis [6]. Run through this engine, the stored and recomputed trial p
values agree with r = 0.991 across 5,080 trials (mid-p convention,
one-sided). The engine is the 2017 method, faster and with the
refinements described above.

**End to end, from PDF to verdict.** Sixty-one published articles whose
baseline tables parse fully and whose extracted values match Carlisle's
hand-entered data were run through the complete pipeline — PDF upload,
extraction, validation, analysis — and compared with Carlisle's stored
trial p values on the log scale: r = 0.94, median disagreement a factor
of 1.05, and 97% agreement on which trials alarm at p < 0.05.

# Notes and roadmap

- **Screening, then scrutiny.** The method flags improbable homogeneity.
  Editors should treat a flag as reason to look — at correlated
  variables, stratification, SE/SD confusion, and the original data —
  not as proof of misconduct.
- **Tests that were tried and dropped.** Earlier versions computed
  Benford's-law and repeated-digit statistics; validated against the
  5,087-trial data set, neither carried useful signal at baseline-table
  scale, and they were removed.
- **Planned.** Granularity checks (GRIM/SPRITE-style flags for
  impossible means of integer data) and a benchmark of Bayesian
  alternatives are on the issues list, as is live progress feedback
  during long runs, and a REST API so manuscript-handling systems
  (Editorial Manager, ScholarOne) can submit tables directly.
- Questions, feedback, and bug reports to Steve Shafer at
  <steven.shafer@stanford.edu>.

# References

1. Carlisle JB. The analysis of 168 randomised controlled trials to test
   data integrity. *Anaesthesia*. 2012;67:521–537.
   <https://doi.org/10.1111/j.1365-2044.2012.07128.x>
2. Retraction Watch leaderboard.
   <https://retractionwatch.com/the-retraction-watch-leaderboard/>
3. Unbiased estimation of standard deviation.
   <https://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation>
4. Carlisle JB, Dexter F, Pandit JJ, Shafer SL, Yentis SM. Calculating
   the probability of random sampling for continuous variables in
   submitted or published randomised controlled trials. *Anaesthesia*.
   2015;70:848–858. <https://doi.org/10.1111/anae.13126>
5. Stouffer SA, Suchman EA, DeVinney LC, Star SA, Williams RMJ. *The
   American Soldier, vol 1: Adjustment During Army Life.* Princeton
   University Press, 1949.
6. Carlisle JB. Data fabrication and other reasons for non-random
   sampling in 5087 randomised, controlled trials in anaesthetic and
   general medical journals. *Anaesthesia*. 2017;72:944–952.
   <https://doi.org/10.1111/anae.13938>
7. Brown NJL, Heathers JAJ. The GRIM test: a simple technique detects
   multiple anomalies in reporting of results in psychology. *Social
   Psychological and Personality Science*. 2017;8:363–369.
