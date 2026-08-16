# Plan: restructure IntegrityAnalysis as an R package repository

Provenance: drafted by Claude Code (model Claude Fable 5) on 2026-08-16 at
Steve Shafer's request ("organize IntegrityAnalysis as a proper package
repository, see stanpumpR as an example"). Plan only — no code has moved yet.
Reviewed against the actual layout of `C:/dev/stanpumpR` and `C:/dev/ParsePDF`
on the same date.

## Why

- **Testability.** A package gives the test suite (ISSUES.md issue 4) a
  standard home (`tests/testthat/`) and a standard runner (`R CMD check`).
  ParsePDF's ~295 assertions (issue 9) then drop in unchanged.
- **Deployment safety.** Today's deploy requires a hand-maintained `appFiles`
  list because the working tree holds Carlisle data spreadsheets that must
  never be uploaded. stanpumpR's pattern deploys **only `app.R`** — the code
  and assets travel inside the installed package, so the entire class of
  "accidentally uploaded the data" mistakes disappears.
- **Discipline.** `R CMD check` on every push (stanpumpR's
  `check-standard.yaml`) catches undeclared dependencies, doc drift, and
  syntax errors before they reach shinyapps.io. ParsePDF already lives at
  this standard; the app should too before the fold-in (issue 9's own
  recommendation).

## The stanpumpR model, applied here

| stanpumpR | IntegrityAnalysis equivalent |
|---|---|
| `DESCRIPTION` + `Collate:` | new — `Package: IntegrityAnalysis`, deps from `global.R`'s `library()` calls |
| `app.R` = `stanpumpR::run_app()` | `app.R` = `IntegrityAnalysis::run_app()` |
| `R/app_globals.R`, `app_ui.R`, `app_server.R`, `app_run.R` | `global.R`, `ui.R`, `server.R` split the same way |
| computation files (`advanceClosedForm0.R`, …) | `P_Calc()`, `sumz()`, validation pipeline, `outputComments()` extracted one-per-file |
| `inst/www` + `addResourcePath()` | `www/Table.png`, `app.js`, `app.css` |
| `inst/extdata` | `Template.xlsx`, `Example.xlsx`, `IntegrityAnalysis.pdf` (served via `system.file()`) |
| `tests/testthat/` + `testthat.R` | new — starts with the `shiny::testServer` harness from the code-review session, grows per issue 4 |
| `.github/workflows/check-standard.yaml` | adopted nearly verbatim |
| `shiny-deploy-production.yaml`, `shiny-pr-*.yaml` | adapted: app name `IntegrityAnalysis`, PR apps `IntegrityAnalysis_PR_<n>` (existing convention), account `steveshafer` |
| `renv/` + `renv.lock` | **deferred** — see "Deliberately later" |

## Phases (each = one small PR, per house convention)

**Phase 1 — scaffolding, no behavior change.**
`DESCRIPTION`, `LICENSE` (MIT, matching stanpumpR — confirm with Steve),
`.Rbuildignore`, `IntegrityAnalysis.Rproj`. Move `global.R` → `R/app_globals.R`,
`ui.R` → `R/app_ui.R` (wrapped in `app_ui()`), `server.R` → `R/app_server.R`
(as `app_server`), plus `R/app_run.R` with `run_app()`. `app.R` becomes the
one-line shim. `www/` → `inst/www` behind `addResourcePath()`; spreadsheets
and PDF → `inst/extdata`, referenced via `system.file()`. The app must run
identically before and after (verify with the testServer harness first —
it becomes the phase's acceptance test).

**Phase 2 — extract computational functions.**
`P_Calc()`, `sumz()`, the per-line validation steps, and `outputComments()`
move to their own files under `R/` with roxygen headers; NAMESPACE generated.
Pure moves — the Monte Carlo must produce bit-identical output (fixed seed
before/after comparison; issue 3's Carlisle validation is the stronger check
and should be green before this phase).

**Phase 3 — tests in-repo (issue 4).**
`tests/testthat.R` + the rebuilt testServer functional test + the
known-answer and input-contract unit tests issue 4 lists. From here on,
`R CMD check` must pass on every PR.

**Phase 4 — ParsePDF fold-in (issue 9).**
Copy the 9 `R/` files (all internals `.pp`-prefixed, no collisions), the
testthat suite with its synthetic-PDF helpers, and `docs/architecture.md`.
Merge deps (`pdftools`, `httr2`, `jsonlite`; `tesseract` in Suggests).
Retire the superseded in-repo `parseCovariateTable.R` /
`testParseCovariateTable.R`.

**Phase 5 — GitHub Actions.**
`check-standard.yaml` first (value is immediate). Then the deploy trio
adapted from stanpumpR: production deploy on push to `main` installs the
package from GitHub and calls `deployApp(appFiles = "app.R")`; PR triage /
deploy / cleanup automate the `IntegrityAnalysis_PR_<n>` lifecycle that is
currently manual. Needs repo secrets: `SHINY_TOKEN`, `SHINY_SECRET`, vars
`SHINY_ACCOUNT=steveshafer`, `SHINY_APP_NAME=IntegrityAnalysis`.

## Deliberately later / open questions

- **renv**: stanpumpR pins with renv, but this machine has a documented
  hazard (a foreign renv hijacking the library path) and CLAUDE.md currently
  promises "no renv". Adopt only when the Actions deploy lands (the workflows
  want a lockfile), as its own PR, and update CLAUDE.md in the same commit.
- **License choice**: MIT assumed (stanpumpR precedent); Steve to confirm —
  the repo currently has no license file at all.
- **Bioconductor ban** carries over: nothing in DESCRIPTION may pull
  Bioconductor (the reason `metap` was replaced by the local `sumz()`).
- **Ordering vs issue 3**: Carlisle-2017 validation should be green before
  phase 2 (function extraction), so refactors have a correctness baseline.
  Phase 1 can proceed before it — it moves files, not logic.
- **`global.R` lower-case rule** becomes moot once nothing is sourced by
  filename, but holds during the transition.
- What stays out of the package: Carlisle corpus spreadsheets, `TestPapers/`,
  `rsconnect/` metadata — untracked today, listed in `.Rbuildignore` and
  never in `inst/`.
