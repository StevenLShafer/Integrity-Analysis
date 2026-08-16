# Merged hand-off — 2026-08-16

Provenance: written by the code-review session (Claude Code, model Claude
Fable 5) on 2026-08-16. The PDF-parsing summary below is distilled from that
session's own artifacts (ISSUES.md, parseCovariateTable.R provenance
headers); everything else was done and verified in the code-review session.

---

## Current state of the app (code-review thread)

All work is **merged to `main` and deployed to production**. There is no
open branch from this thread.

- **PR #4** (https://github.com/StevenLShafer/IntegrityAnalysis/pull/4),
  merged as `9fc20f8`: full bug-fix pass over `global.R` / `server.R`.
  Every fix carries an in-place `FIX:` comment with rationale. Highlights:
  - Crashes: nonexistent `read.xl()` (.xls uploads), fatal `with()` call
    that suppressed the Download Results button, `is_category()` on text
    columns, validation crash on missing MEAN, NA column-rename when no
    TRIAL column, single-category-column chi-square (`drop = FALSE`).
  - **Statistics (results changed slightly)**: chi-square now really runs
    `B = m` = 15,000 replicates (a numeric `simulate.p.value` had silently
    left the default 2,000); simulated column means round to `ROUND_MEAN`
    (reported-mean precision) instead of `ROUND_OBSERVATION`; large-N
    branch no longer misaligns simulated means across arms; `m1` floored.
  - State: per-session variables moved from the global environment into
    `server()` (concurrent users no longer clobber each other); `OUTPUT`
    resets on upload/re-run; Analyze button lives in its own `GoButton`
    slot and survives a completed run.
  - `Global.R` → `global.R` (Linux/shinyapps.io only sources the
    lower-case name — the deployed app had **never** sourced it).
  - `www/Table.png`, `www/app.js`, `www/app.css`, `IntegrityAnalysis.pdf`
    recovered from `G:/projects/Fraud/2025` and committed (production had
    404'd on all of them).
  - `metap` replaced by a local 6-line Stouffer `sumz()` in `global.R`
    (verified identical to `metap::sumz` within 1.2e-16 over 200 cases).
    Reason: metap → mutoss → multtest drags in **Bioconductor**, and the
    shinyapps.io image build fails fetching matching BiocGenerics sources.
    Do not reintroduce Bioconductor-dependent packages casually.
  - Removed unused libraries (OpenMx, digitTests, rsconnect×2) and inert
    parallel scaffolding (future/doFuture/doParallel calls); the P_Calc
    row loop is sequential `%do%`. Parallelizing it is future work — note
    the reversed-`match()` fix in `P_Calc` was made precisely so
    parallelization won't silently scramble row labels.
- **Production** https://steveshafer.shinyapps.io/IntegrityAnalysis/
  redeployed from `main` and verified (app loads, session starts,
  Table.png serves). The `IntegrityAnalysis_PR_4` test app was terminated
  and purged.
- After the merge, Steve's commit `81b03cf` restored `ISSUES.md` and
  reapplied an SE-column change that had been lost in the branch collision
  (see README.md). Check that commit if the SE column behavior matters to
  your task.

## Verification harness that exists

A headless functional test drives the real server via `shiny::testServer`
(upload Example.xlsx → validate → analyze → re-run; asserts summaries,
done-state, and no result-appending). It lived in the code-review
session's scratchpad and is NOT in the repo — rebuilding it into
`tests/` is part of ISSUES.md issue 4. Two `testServer` facts worth
keeping: the test block sees a *clone* of the server environment, so
variables mutated by `<<-` must be read through `session$env$...`; and
`input$upload` is simulated with a one-row data.frame carrying
`datapath`.

## Runbook (operational facts)

- **R version**: run and deploy with **R 4.5.3**
  (`"C:\Program Files\R\R-4.5.3\bin\Rscript.exe"`); its 4.5 user library
  has the full package set. The 4.6.1 user library is nearly empty.
- **Deploy** (rsconnect account `steveshafer`, server `shinyapps.io`):

  ```r
  rsconnect::deployApp(
    appDir  = "C:/dev/IntegrityAnalysis",
    appName = "IntegrityAnalysis",           # or IntegrityAnalysis_PR_<n> for a PR test
    appFiles = c("global.R","server.R","ui.R",
                 "www/Table.png","www/app.js","www/app.css",
                 "Template.xlsx","Example.xlsx","IntegrityAnalysis.pdf"),
    account = "steveshafer", server = "shinyapps.io",
    forceUpdate = TRUE, launch.browser = FALSE)
  ```

  **Always pass `appFiles` explicitly**: the working tree contains
  Carlisle data spreadsheets and other files that must not be uploaded.
  PR test deployments follow the convention
  `IntegrityAnalysis_PR_<PR number>`; after merging, remove them with
  `rsconnect::terminateApp()` then `rsconnect::purgeApp()`.
- **Hazards found the hard way**:
  - Running `Rscript` while the shell's working directory is another,
    renv-managed project activates **that project's renv** — packages
    install into (and pollute) that project's library. One spill was
    reverted with `renv::restore(clean = TRUE)`. Set the working
    directory to this repo before any R call.
  - In PowerShell double-quoted `-e` strings, `$p` etc. are interpolated
    away by PowerShell before R sees them. Write R code to a temp file
    and `Rscript file.R` instead.
- Untracked working files in the tree (Carlisle data spreadsheets,
  `TestPapers/`, `rsconnect/`, `~$Template.xlsx` Excel lock file) belong
  to Steve / the PDF-parsing thread. Leave them out of commits unless
  asked.

## PDF-parsing thread (summary — ISSUES.md is canonical)

- The real work lives in **`C:/dev/ParsePDF`** (private R package,
  temporary home; will be folded into this repo — ISSUES.md issue 9).
  Deterministic pdftools-based parser validated over the 1,865-article
  corpus in `C:/temp/journals`; ~295 testthat assertions; `R CMD check`
  clean; font-encoding repairs; SD and SE as separate columns with
  `ROUND_DISPERSION`; AI fallbacks validated but disabled in deployment;
  OCR for scans. Numbers and details: ISSUES.md "Where things stand".
- `parseCovariateTable.R` / `testParseCovariateTable.R` in THIS repo are
  an earlier, in-repo version of the parser — the logic now exists in two
  places, and ParsePDF is the maintained one. Expect the in-repo copies
  to be superseded when issue 9 lands.

## Suggested order of work (from ISSUES.md)

1. Issue 3 — validate the Monte Carlo against Carlisle 2017 (mind the
   folded p-value convention described there).
2. Issue 9 — fold ParsePDF into this repo, carrying its test suite.
3. Issue 4 — comprehensive test suite for the app itself.
4. Issue 6 — one-sided-toward-homogeneity p-value (decision made; the
   which-tail question there MUST be resolved before implementing).
