# CLAUDE.md

Orientation for Claude Code working in this repository. Created 2026-08-16
as part of a session hand-off; see [`handoff/`](handoff/README.md) for the
state of the two threads (app code-review, PDF parsing) that converge here,
and [`ISSUES.md`](ISSUES.md) for the **canonical open-issues list** — read
its "Where things stand" section before starting substantive work.

## What this is

A Shiny app implementing the Carlisle–Shafer Monte Carlo analysis of
baseline data in randomized controlled trials, used in Steve Shafer's work
as a journal editor detecting research fraud (references in README.md).
This is a **separate project from stanpumpR**. Three flat files: `global.R`
(libraries, constants, `sumz()`, `outputComments()`), `ui.R`
(shinydashboard page), `server.R` (validation pipeline + `P_Calc()` Monte
Carlo). Not a package: no renv, no Collate, no tests yet (ISSUES.md
issue 4).

The upload pipeline: file → column-name normalization by grep (any
"MEAN"-containing name that isn't MEAN → `ROUND_MEAN`, "OBS" →
`ROUND_OBSERVATION`, "TRIAL", "ROW"/"GROUP", "NUMBER" → N) → per-line
validation (continuous rows need N/MEAN/SD; category rows must be numeric,
integer-valued, with at least one NA in the column) → per-trial `P_Calc()`:
closed-form weighted means, Monte Carlo of rounded simulated means
(continuous) or simulated chi-square (categorical), rows combined with
Stouffer's `sumz()`.

## Running, testing, deploying

- Use **R 4.5.3**: `"C:\Program Files\R\R-4.5.3\bin\Rscript.exe"` (its 4.5
  user library has all packages; the 4.6 library does not).
- Do not start R while the shell is in another project's directory —
  stanpumpR's renv will hijack the library path.
- Deploy with the exact `deployApp()` call in
  [`handoff/2026-08-16-merged-handoff.md`](handoff/2026-08-16-merged-handoff.md)
  — **always with the explicit `appFiles` list**, because the working tree
  holds Carlisle data spreadsheets that must never be uploaded. PR test
  apps are `IntegrityAnalysis_PR_<n>`; purge them after merging.
- Production: https://steveshafer.shinyapps.io/IntegrityAnalysis/
  (rsconnect account `steveshafer`).
- Headless functional testing pattern (until a real suite exists): drive
  the server with `shiny::testServer`; simulate `input$upload` with a
  one-row data.frame carrying `datapath`, and read `<<-`-mutated state via
  `session$env$...` (the test block only sees a clone).

## Conventions

- **`global.R` stays lower-case** — shinyapps.io (Linux) never sources
  `Global.R`.
- Generous comments; every non-obvious or AI-drafted change carries a
  provenance header (origin, date, run/verified status) and in-place
  `FIX:`/rationale comments. See the tops of `global.R`, `server.R`, and
  `parseCovariateTable.R` for the house style.
- Small, focused commits — one issue each. **Push immediately after
  committing**: two agents have shared this repository, and
  committed-but-unpushed work was already lost once when a merged branch
  was deleted (recovered in `81b03cf`). Unpushed work does not exist.
- No Bioconductor-dependent packages (the shinyapps.io image build breaks
  on them — that is why `metap` was replaced by a local `sumz()`).
- Keep secrets and per-user data out of `outputComments()` logs and out of
  bookmark state.

## Adjacent work

- **`C:/dev/ParsePDF`** (private package): the maintained PDF
  baseline-table parser, to be folded into this repo (ISSUES.md issue 9).
  The in-repo `parseCovariateTable.R` is an earlier copy of that logic.
- **`G:/projects/Fraud/2025`**: Steve's original dev folder (assets,
  Carlisle corpus files, old code in `Old Files/`).
