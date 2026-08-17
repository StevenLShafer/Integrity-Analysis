# IntegrityAnalysis — open issues

Working list of outstanding work, newest thinking first. Each entry says what
the work is, why it matters, and what "done" looks like. Closed items move to
the bottom rather than being deleted, so the reasoning survives.

---

## Where things stand — 2026-08-16

Handoff from the PDF-parsing session, which ran alongside a separate
code-review session, so that one new session can pick up both threads.

**Done, in `C:/dev/ParsePDF`** (private, temporary — see issue 9):

- A deterministic parser validated over the **whole** 1,865-article corpus in
  `C:/temp/journals`. 1,341 articles yield a table; 905 trials produce 12,500
  continuous rows; 58% of Carlisle's known mean/SD pairs are recovered exactly,
  with 99.4% agreement on decimal places. ~295 testthat assertions,
  `R CMD check` clean.
- Four **font-encoding faults** repaired, each found by reading the corpus
  character by character. Anesthesiology renders `=` as U+2AFD and `±` as
  U+2AFE; BJA and EJA render `=` as `¼`; BJA 2003 uses a real `±` where an **en
  dash** is printed, which was fabricating mean±SD pairs out of dose ranges.
  Separately, `4,335` was being read as `4.335`.
- **SD and SE are separate columns**, with `ROUND_DISPERSION`. 229 corpus rows
  report a standard error; they were previously filed as SDs, which at typical
  trial sizes is wrong by about a factor of four.
- AI fallbacks (table page, and article prose) validated live against Carlisle
  on articles the deterministic engine scores **zero** on: 81% and 91%.
  **Not enabled in deployment** — see issue 8.
- OCR for scanned articles; 83 of 99 scanned EJA papers had their PMIDs
  recovered from the printed citation line, with no network call.

**Deliverables on disk, `C:/temp/ParsePDF_output/`:**

| File | What it is |
|---|---|
| `ParsePDF_IntegrityAnalysis.xlsx` | the corpus — 12,500 continuous rows, 905 trials, TRIAL = PMID, current SD/SE schema |
| `ParsePDF_verified.xlsx` | same data plus per-row verification tags (only 45 trials verified — see below) |
| `ParsePDF_needs_AI_fallback.csv` | tiered list of what the AI could rescue, with costs |
| `ParsePDF_for_review.csv` | rows where the two reads disagree |
| `ParsePDF_discrepancies_for_review.csv` | 171 diagnostic disagreements with Carlisle, for adjudication |
| `success_by_year.png`, `ParsePDF_success_by_year.csv` | year/journal regression |

**The blind verification pass is DONE (re-run completed 2026-08-16).** The
first attempt (four concurrent shards) had timed out on 811 of 861 calls; the
re-run as **two shards with a 900-second per-call timeout** completed with
**zero timeouts** (confirming the rate-limit diagnosis; total spend for the
whole effort stayed around the original estimate). Results, merged into
`C:/temp/ParsePDF_output/` (`ParsePDF_verified.xlsx`,
`ParsePDF_for_review.csv`):

- 785 of 905 trials produced a comparable blind read (90 pages the model
  declined as having no baseline table, plus tag collisions account for the
  rest).
- **7,560 of 12,500 shipped rows (60.5%) confirmed** by the independent read.
- **2,778 missing arm Ns filled** under the ≥80%-agreement gate: rows carrying
  an N rose **from 41.5% to 63.8%**.
- 3,115 rows written to the review list. The pilot's pattern (disagreements
  are mostly about *which rows belong*, not about the numbers) has not been
  re-examined at this scale — adjudicating that list is the natural next step.

**A collision worth learning from.** This session committed to a local
`fix-r-code-errors` branch without pushing; the review session then merged the
remote branch and deleted the local one, and the work disappeared. It was
recovered from the reflog onto the branch `parsepdf-integration`. **In a
repository two agents share, unpushed work does not exist.**

---

## 1. Build the API

Expose the analysis so other programs can call it — the target is editorial
systems such as Editorial Manager linking to it automatically and silently for
fraud screening during peer review.

**Contract**

| | |
|---|---|
| Input | a single PDF **or** a spreadsheet (xls/xlsx/csv) |
| On pass | run the Monte Carlo; return a CSV of the analysis, plus confirmation the PDF was deleted |
| On fail | return **the partial table**, carrying as much extracted data as possible, plus what is wrong with it |
| Retention | none — the PDF is deleted and the caller is told so |

**Decisions already made**

- **A failure is not a bare error.** It returns the failed table so an editor or
  reviewer can fill the gaps and call the API again, this time with a
  spreadsheet instead of the PDF. A failed scan is a round trip, not a dead
  end — which means *the failure payload must itself be valid input to the next
  call*. ParsePDF's `writeIntegrityTemplate()` already emits exactly that
  layout.
- **No arm N, no analysis.** Without a hard-coded N the service returns a fail
  rather than running the Monte Carlo. This matters more than it sounds: about
  58% of rows ParsePDF extracts from real articles carry no arm N, because many
  tables never print it.
- **No AI in the deployed path.** Two independent reasons: every call would be
  billed to the maintainer's account at unbounded volume, and manuscripts under
  peer review are *unpublished*, so sending one to a third-party API is a
  confidentiality problem. A deterministic engine also guarantees that the same
  submission always yields the same verdict — which matters when the output may
  influence an editorial decision.

**Watch for**

- Annotation must stay out of the data columns. `server.R` decides a column is
  categorical if it is integer-valued with at least one `NA`, so a numeric
  "needs attention" flag would be silently swallowed as a category. Use a text
  column or a separate sheet.
- Return `$skipped` from ParsePDF, not a count: it names each unusable row *and
  why* ("median [range] — integrity analysis needs mean and SD"), which is what
  tells an editor where to look.
- A folder of PDFs must go through `parseBaselineTableFiles()`, never a loop:
  roughly 2% of real PDFs hang poppler indefinitely, R cannot interrupt it, and
  in a multi-user app an in-process hang takes the worker down for everyone.
- Realistic expectation: fed a single PDF, the deterministic path yields a
  fully analysable trial roughly a third of the time. The spreadsheet path is
  the reliable one; the PDF path is a convenience that will often decline.

---

## 2. Point https://integrityanalysis.io at the app

Register/behave so that the domain lands on the shinyapps.io deployment.

Decide whether it should be a redirect or a landing page that explains the
method, links the Carlisle references, and then hands off to the app — the
latter is better for an editor arriving from a journal's instructions, and
gives somewhere to state the data-retention promise before anyone uploads a
manuscript.

---

## 3. Validate the analysis against Carlisle's 2017 manuscript

Reproduce the published results for the 5,087 trials in Carlisle's 2017
*Anaesthesia* paper from the same inputs, as an end-to-end check on the Monte
Carlo.

**Do this before issue 4**, since it settles whether the current implementation
is right, and before issue 5, so optimisation has a correctness baseline to
protect.

**Note the p-value convention.** Carlisle's published values are *folded*: he
and Steve treated P > 0.95 (too heterogeneous) as equally concerning as
P < 0.05 (too homogeneous), so every stored value is < 0.5, with 1 − P recorded
wherever Stouffer's sumz exceeded 0.5. Reproducing his numbers therefore
requires reproducing that convention. Issue 6 then changes it deliberately —
these are two separate steps and conflating them will make validation look like
a bug.

---

## 4. Build a comprehensive test suite

The repository currently has no automated tests.

Priorities, roughly in order of what would catch the most:

- The **input contract**: column-name normalisation (`MEAN`-containing names
  that are not `MEAN` become `ROUND_MEAN`; `OBS` becomes `ROUND_OBSERVATION`),
  and the `is_category` rule (numeric, integer-valued, at least one `NA`).
- **Known-answer Monte Carlo cases** with a fixed seed, so a refactor cannot
  silently change results.
- **Degenerate inputs**: one arm, one variable, zero-SD rows, an arm with N = 1,
  missing N, a category column that sums to more than the arm N.
- **Round-trip**: a spreadsheet written by the app re-imports unchanged.

ParsePDF's suite is a reasonable model — it builds its own PDFs with the `pdf()`
device rather than shipping copyrighted articles, and every regression it has
found is pinned by a fixture.

---

## 5. Optimise the Monte Carlo

Profile before changing anything, and keep issue 3's validation green
throughout.

Likely wins, in the order worth trying: vectorise across replications rather
than looping; pre-allocate the replication matrices; avoid recomputing per-trial
constants inside the replication loop; consider whether the simulation count can
be adaptive (stop early when the p-value is far from any threshold of interest).

Since trials are independent — no cross-talk — trial-level parallelism is
available and is the easiest large win for a whole-corpus run.

---

## 6. Change the p-value to one-sided toward homogeneity

Steve's decision (2026-08-16), reversing the earlier agreement with Carlisle.
The p-value should reflect only the **excessive homogeneity** direction.

**Why:** excessive homogeneity is a demonstrated fraud signal — it is how Fujii
and others were caught. Excessive heterogeneity is *not* known to indicate
fabrication. Folding the two treats a real signal and an unproven one as
equivalent, and it inflates apparent significance for trials whose baseline data
are merely more variable than chance, which is the direction most likely to
produce a false accusation.

**Open before implementing:** which tail is "small"? Read literally, "the
fraction of the Monte Carlo distribution showing less homogeneity" approaches 1
for suspiciously homogeneous data, inverting the usual convention that a small
p is the alarming one. The alternative — P = probability of data *at least as
homogeneous* as observed — keeps small p = concerning. Getting this backwards
inverts the detector, so confirm the intent, and if the first reading is meant,
give the output a name other than "p-value" so nobody applies p < 0.05 to it
reflexively.

---

## 7. Survey other open-source research-integrity screens

Look for additional published, open screens that could be applied to the same
submissions and reported alongside the baseline analysis.

**Already tried and rejected — do not repeat without a reason:**

| Screen | Outcome |
|---|---|
| **Benford's law** | worthless here |
| **Repeating-digit tests** | worthless here |

Both almost certainly fail for the same reason: a baseline table supplies far
too few numbers for either test to have any power. Any candidate screen should
therefore be judged first on whether it can work on the order of tens of
numbers — which rules out most digit-distribution methods before any
implementation effort.

Worth considering instead are screens that use *structure* rather than digit
frequency: internal consistency of means against reported totals, SDs that are
impossible for the stated N and range, granularity tests (GRIM/GRIMMER — does a
reported mean exist for an integer-valued measure at that N?), and terminal
digit balance across arms rather than within a single table.

---

## 8. A URL keyword that unlocks AI parsing (future)

The AI fallback is off in deployment because every call is billed to the
maintainer. A secret keyword in the URL would let him — and only him — turn it
on for a session, e.g. `?ai=<secret>`, with the app comparing SHA-256 of the
supplied value against a stored hash so the secret itself is never in the code.

**The point is publication, not concealment.** The AI algorithm is part of the
academic contribution: it should be visible, tested and citable even though
running it costs money that only the maintainer is paying. The secret gates the
*spending*, not the *method*. That ordering has three consequences worth
building for:

- **Make the deployment able to use somebody else's key.** If a publishing
  house wants to adapt this, they will run their own instance and pay their own
  bills. The gate should therefore be a policy, not a hard-coded off switch:
  something like *AI is enabled when a valid unlock token is supplied, or when
  this deployment sets `INTEGRITY_AI_ALWAYS=true`* — the second being how a
  third party runs it with their own `ANTHROPIC_API_KEY`. Hard-coding "off"
  would force them to fork the logic, which defeats the purpose.
- **The prompts and the JSON schema are the algorithm.** They live in
  `ParsePDF::.ppTableSchemaJson()` and `.ppSystemPrompt()`, written to be read.
  Keep them legible and commented; they are the part a reviewer will want to
  examine, more than the plumbing around them.
- **The evidence should travel with it.** The measured rescue rates — 91% of
  known values on articles with no baseline table, 81% on articles whose table
  the deterministic engine misread, both scored against Carlisle on articles
  where the deterministic engine scores zero — are what make the method a
  contribution rather than an assertion.

*(Resolved: ParsePDF is a temporary repository and will be folded into this one
— see issue 9 — so the algorithm becomes public by moving here.)*

Since the secret will be 32 random characters from a password manager rather
than a memorable phrase, the entropy problem below is already solved; the
remaining points still apply.

The mechanism is right. Four things it needs to be safe:

**1. Make the secret high-entropy, or use a slow hash.** SHA-256 is fast by
design — billions of guesses a second on a GPU. A memorable keyword carries
perhaps 30–40 bits of entropy and would fall to an offline dictionary attack in
minutes *if the hash is public*, and this repository is public. Either use 32+
random characters (brute force then infeasible), or store the secret with a
deliberately slow KDF — `sodium::password_store()` / `password_verify()` gives
argon2 and is the right tool if the secret must be memorable.

**2. Keep the hash out of the repository.** Put it in an environment variable on
shinyapps.io (`INTEGRITY_AI_KEY_SHA256`). Then there is no artefact to attack
offline at all, which is worth more than the choice of hash.

**3. Assume the URL will leak.** Query strings end up in browser history, server
access logs, `Referer` headers sent to third parties, screenshots and shared
links. Treat the token as low-value and rotatable, not as a password. A signed,
expiring token (HMAC over an expiry timestamp) is strictly better than a static
secret, because a leaked URL then stops working by itself.

**4. Cap the spending regardless.** The token protects money, so do not let it
be the only thing that does: a hard per-session and per-day call limit means a
leaked token cannot run up an unbounded bill. Defence in depth, and cheap.

**On injection.** The classic risk does not really arise here: R will not
execute a query string unless something calls `eval(parse(text = ...))`, so the
first rule is simply **never evaluate anything from the URL**. The real hazards
are the ordinary ones — do not interpolate the parameter into a file path (path
traversal), a `system()` call, or HTML output (XSS). Reading it is safe:

```r
q <- shiny::parseQueryString(session$clientData$url_search)
supplied <- q[["ai"]]                      # character or NULL, never evaluated
ok <- !is.null(supplied) &&
      identical(digest::digest(supplied, algo = "sha256", serialize = FALSE),
                Sys.getenv("INTEGRITY_AI_KEY_SHA256"))
```

Compare in constant time if convenient; the timing leak is marginal here but
avoiding it costs nothing.

**Also**: exclude the parameter from any bookmarking, or the secret gets stored
in bookmark state, and make sure it never reaches the log that `outputComments()`
writes.

---

## 9. Fold ParsePDF into this repository

ParsePDF (`C:/dev/ParsePDF`, private) is a temporary home for the PDF parsing
work. It will be merged directly into IntegrityAnalysis rather than kept as a
dependency.

**What comes across**

| | |
|---|---|
| Code | 9 flat files in `R/`, no `Collate:`, no load-time dependencies between them |
| Tests | ~295 testthat assertions, including regression fixtures for every real-corpus defect found so far |
| Docs | `docs/architecture.md` and `.html`, `AGENTS.md` |
| Dependencies added | `pdftools`, `httr2`, `jsonlite`; `tesseract` optional, only for scans |

**Why it is easy, and what to preserve**

Every internal function is prefixed `.pp`, so nothing collides with the app's
own names. The files are self-contained, so the merge is a copy rather than a
rewrite — most likely sourced from `global.R` alongside the existing code.

Two things are worth deliberately preserving, because they are easy to lose in
a merge:

- **The test suite.** This repository has none (issue 4), so folding ParsePDF in
  supplies the parsing half of the one it needs — but only if the tests are
  carried across and kept runnable. They build their own PDFs with the `pdf()`
  device, so they need no fixtures beyond `grDevices`.
- **`R CMD check` discipline.** ParsePDF is a package and is checked clean on
  every change; a Shiny app is not checked at all. Whatever replaces that — even
  just running the testthat suite in CI — should be in place before the merge,
  not after.

**Do this after issue 3** (validation against Carlisle 2017), so the analysis
side is known-good before the parsing side lands on top of it.

---

## 10. Restructure the repository as an R package (stanpumpR model)

Steve's request (2026-08-16): organize IntegrityAnalysis as a proper package
repository, using stanpumpR as the example. Full plan, reviewed against the
actual stanpumpR and ParsePDF layouts, in
[`docs/package-restructure-plan.md`](docs/package-restructure-plan.md).

In one line per phase: (1) scaffolding — `DESCRIPTION`, `R/app_*.R`,
`inst/www`, one-line `app.R`, no behavior change; (2) extract `P_Calc()`,
`sumz()`, validation into documented one-per-file functions; (3) testthat
suite in-repo (subsumes issue 4's home); (4) ParsePDF fold-in (issue 9)
lands as package files + tests; (5) GitHub Actions — `R CMD check` on every
push, then stanpumpR's deploy trio, after which production deploys ship only
`app.R` and the hand-maintained `appFiles` list (and its
never-upload-the-Carlisle-data hazard) disappears.

Sequencing: phase 1 any time; phase 2 only after issue 3's validation is
green, so the refactor has a correctness baseline. renv is deliberately
deferred (see the plan). License file to be chosen (MIT assumed, matching
stanpumpR — confirm).

---

## 11. Live UI feedback while the Monte Carlo runs

Steve's request (2026-08-16). The p-value calculation can take a very long
time, and the user needs feedback while it runs. This is genuinely hard in
Shiny: the server is single-threaded, so while `P_Calc()` computes, the UI
is locked — no reactive flush, no log refresh, no button response.

What exists today, and its limits: `shiny::Progress` ticks once per
**trial** (its `$set()` pushes straight to the websocket, bypassing the
flush, which is why it works at all), and `bslib::input_task_button` shows
a busy state — but within one long trial nothing moves, the comments log
(`invalidateLater(1000)`) freezes, and nothing can be cancelled. Dean's
PR #2 (full-page spinner) is another symptom of the same itch; a spinner
still cannot update *during* the computation.

The real fix is to take the computation **off the main thread**:

- **`shiny::ExtendedTask`** (Shiny ≥ 1.8.1) + {promises}/{future} is the
  designed-for answer, and `input_task_button` — already in the app — is
  its intended companion: the button binds to the task, the UI stays live,
  per-trial results can stream into the log as they complete, and a Cancel
  button becomes possible.
- A worker process (callr/future multisession) reporting progress through a
  file or socket the main session polls with `invalidateLater` is the
  portable fallback.
- Within-trial granularity: P_Calc's row loop can report progress per ROW
  (pass a callback) once progress can actually reach the client.

Do together with issue 5 (optimise the Monte Carlo): parallelising trials
with {future} and moving the loop off the main thread are the same
plumbing, and should be designed once. Test cancellation and the
two-users-at-once case on shinyapps.io, where worker processes are billed
compute.

---

## Closed

*(nothing yet)*
