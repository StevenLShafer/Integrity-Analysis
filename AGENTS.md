# AGENTS.md

Orientation for **all AI coding assistants** working in this repository
(Claude Code, ChatGPT Codex, Gemini/Antigravity, ...). This is the
canonical agent documentation — CLAUDE.md is only a pointer here, kept
because Claude Code auto-loads that filename. Created 2026-08-16 as part
of a session hand-off; see [`handoff/`](handoff/README.md) for the state
of the two threads (app code-review, PDF parsing) that converge here, and
[`ISSUES.md`](ISSUES.md) for the **canonical open-issues list** — read
its "Where things stand" section before starting substantive work.

## What this is

A Shiny app implementing the Carlisle–Shafer Monte Carlo analysis of
baseline data in randomized controlled trials, used in Steve Shafer's work
as a journal editor detecting research fraud (references in README.md).
This is a **standalone project, unrelated to any other repository on this
machine**. Since the phase-1 restructure (2026-08-16, ISSUES.md issue 10,
plan in [`docs/package-restructure-plan.md`](docs/package-restructure-plan.md))
it is an **R package**, modeled on stanpumpR: `R/app_globals.R` (constants,
`sumz()`, `outputComments()` — was `global.R`), `R/app_ui.R` (`app_ui()`,
shinydashboard page — was `ui.R`), `R/app_server.R` (`app_server`,
validation pipeline + `P_Calc()` Monte Carlo — was `server.R`),
`R/app_run.R` (exported `run_app()`, attaches libraries, registers
`inst/www` under the `www/` resource prefix). `app.R` is a one-line shim.
Bundled assets live in `inst/www`; `Template.xlsx`, `Example.xlsx`, and
`IntegrityAnalysis.pdf` in `inst/extdata`, resolved with `system.file()`.
No renv (deliberate — see the plan). Since the ParsePDF fold-in
(2026-08-17, issue 9) the package also contains the PDF parser
(`R/parseBaselineTable*.R`, `tokenize.R`, `pageLayout.R`, `aiFallback.R`,
`utils.R`, `writeIntegrityTemplate.R`, all internals `.pp`-prefixed) and
its ~295-assertion testthat suite (`tests/testthat/`, synthetic PDFs, no
fixtures needed); app-side tests are still to come (issue 4).

The upload pipeline (since 2026-08-17 all input modes converge on one
editable grid — Steve's original vision): file (csv/xls/xlsx, **or an
article PDF**, parsed by the deterministic engine in a subprocess and
narrated in the comments log) → the **editable rhandsontable grid** (fix a
missing N or a mistyped SD in place; "Apply Edits & Revalidate") →
column-name normalization by grep (any "MEAN"-containing name that isn't
MEAN → `ROUND_MEAN`, "OBS" → `ROUND_OBSERVATION`, "TRIAL", "ROW"/"GROUP",
"NUMBER" → N) → per-line validation (continuous rows need N/MEAN/SD;
category rows must be numeric, integer-valued, with at least one NA in the
column) → per-trial `P_Calc()`: closed-form weighted means, Monte Carlo of
rounded simulated means (continuous) or simulated chi-square under fixed
margins (categorical), **mid-p** ties, rows combined with Stouffer's
`sumz()` into a **single one-sided p toward excessive homogeneity**
(issue 6: small p = data more similar across arms than random sampling
explains — the fraud signal; heterogeneity is deliberately not reported,
and the categorical branch takes the lower tail, not `chisq.test`'s
upper tail). A
failed PDF extraction is a round trip: the partial table is offered as a
download in the app's own input layout. Local hand-test PDFs, one per
parsing challenge: `corpus/` (see its README).

## Running, testing, deploying

- Use **R 4.5.3**: `"C:\Program Files\R\R-4.5.3\bin\Rscript.exe"` (its 4.5
  user library has all packages; the 4.6 library does not).
- Do not start R while the shell is in another project's directory — an
  renv-managed project there will hijack the library path.
- Run locally: install the package (`R CMD INSTALL --no-multiarch .`) then
  `IntegrityAnalysis::run_app()` — or `shiny::runApp()` on `app.R`.
- Deploy (since the phase-1 package restructure): install the package
  **from GitHub** so rsconnect records the GitHub source, then ship only
  the shim — the old hand-maintained `appFiles` list (and its risk of
  uploading the Carlisle spreadsheets) is gone:

  ```r
  remotes::install_github("StevenLShafer/IntegrityAnalysis")  # or @<branch> for a PR app
  rsconnect::deployApp(appDir = "C:/dev/IntegrityAnalysis",
    appName = "IntegrityAnalysis",       # or IntegrityAnalysis_PR_<n>
    appFiles = "app.R",
    account = "steveshafer", server = "shinyapps.io",
    forceUpdate = TRUE, launch.browser = FALSE)
  ```

  A locally-installed (`R CMD INSTALL`) copy will NOT deploy — shinyapps.io
  can only fetch the package from GitHub, so push first, deploy second.
  PR test apps are `IntegrityAnalysis_PR_<n>`; purge them after merging.
  **A PR test app must identify itself in the UI** (Steve's rule,
  2026-08-16): deploy it with an `app.R` of the form
  `IntegrityAnalysis::run_app(testNote = "PR #<n>: <what to test>")` —
  write that shim to a scratch directory and point `deployApp(appDir=)` at
  it, so the repository's production `app.R` is never edited. The note
  renders as an orange banner under the header, telling the tester which
  PR this is and what to look at without opening GitHub.
  (The pre-package procedure, for history: `handoff/2026-08-16-merged-handoff.md`.)
- Production: https://steveshafer.shinyapps.io/IntegrityAnalysis/
  (rsconnect account `steveshafer`).
- Headless functional testing pattern (until a real suite exists): drive
  the server with `shiny::testServer`; simulate `input$upload` with a
  one-row data.frame carrying `datapath`, and read `<<-`-mutated state via
  `session$env$...` (the test block only sees a clone).

## Conventions

- Case sensitivity: shinyapps.io runs Linux — filenames in code must match
  exactly (the old `Global.R`-vs-`global.R` failure generalizes).
- Generous comments; every non-obvious or AI-drafted change carries a
  provenance header (origin, date, run/verified status) and in-place
  `FIX:`/rationale comments. See the tops of `R/app_globals.R`,
  `R/app_server.R`, and `parseCovariateTable.R` for the house style.
- Small, focused commits — one issue each. **Push immediately after
  committing**: two agents have shared this repository, and
  committed-but-unpushed work was already lost once when a merged branch
  was deleted (recovered in `81b03cf`). Unpushed work does not exist.
- Pull requests (Steve's rules, 2026-08-16): every PR **branches directly
  from `main`** — never stack a PR on another PR's branch; each PR is
  devoted to **one specific issue**, so its testing scope is obvious; and
  the PR description **opens with a plain statement of the change made**.
- `main` is **protected** (2026-08-17): force-pushes and branch deletion
  are blocked for everyone. Admin (Steve's account) can still push
  ordinary commits directly — used for documentation-only changes; code
  goes through PRs. If a push is rejected, do not try to work around the
  protection; rebase onto the current `main` and push normally.
- No Bioconductor-dependent packages (the shinyapps.io image build breaks
  on them — that is why `metap` was replaced by a local `sumz()`).
- Keep secrets and per-user data out of `outputComments()` logs and out of
  bookmark state.

## The parser optimization loop

The PDF parser (the `R/` files prefixed with parse*/tokenize/pageLayout/
aiFallback, folded in from the ParsePDF package 2026-08-17, issue 9) is
the hardest problem in this repository, and it is **deliberately set up to
be re-attacked by large language models every few months**. If you are an
LLM reading this because Steve asked you to look for parser improvements,
this section is your map.

- **The problem**: extract the baseline demographics table ("Table 1") of
  a randomized controlled trial from an article PDF — one row per
  variable per arm, means/SDs/Ns with their printed rounding — using only
  the PDF text layer, deterministically. Journals differ in layout,
  column conventions, and font encodings (several journals print `=` or
  `±` as private-use glyphs; see the repairs pinned in the test suite).
- **The evidence**: [`corpus/ParseOutcomes.csv`](corpus/ParseOutcomes.csv)
  — the master sheet, one row per PDF in the 1,865-article corpus, with a
  binary outcome (**successfully parsed / not successfully parsed**) and a
  `COMMENTS` column: the steps taken when parsing succeeded, and where the
  process failed when it did not. Read the failures, then read the code.
  [`corpus/README.md`](corpus/README.md) documents the columns, the
  current baseline (71.9% parsed; 58% exact value recovery against
  Carlisle), and the regeneration command.
- **The corpus PDFs** are copyrighted and live locally at
  `C:/temp/journals` (`<journal>/<year>/<n.m>.pdf`) — never commit them.
  The test suite needs none of them (synthetic PDFs via `pdf()`).
- **The architecture**: [`docs/parsepdf-architecture.md`](docs/parsepdf-architecture.md)
  (with an HTML twin). Entry points: `parseBaselineTable()` (one PDF,
  deterministic-then-optional-AI), `parseBaselineTableFiles()` (batches;
  one subprocess per file with an OS timeout — never loop over PDFs
  in-process, ~2% hang poppler), `writeIntegrityTemplate()` (emits the
  app's input format). The AI fallback's prompt and JSON schema — part of
  the published method — are `.ppSystemPrompt()` and
  `.ppTableSchemaJson()` in `R/aiFallback.R`, written to be read.
- **The rules**: the deployed path stays deterministic and offline (the
  app screens *unpublished* manuscripts; same input must always give the
  same verdict). Keep the ~295-assertion test suite green and add a
  fixture for every new corpus defect found. Score any change both ways:
  parse rate (`corpus/buildParseOutcomes.R`) **and** value accuracy
  against Carlisle — parsing more tables while misreading more numbers is
  a regression.

## Adjacent work

- **`C:/dev/ParsePDF`**: the parser's former home (now folded in here —
  its git history holds the development record; the repo is retired).
- **`G:/projects/Fraud/2025`**: Steve's original dev folder (assets,
  Carlisle corpus files, old code in `Old Files/`).
