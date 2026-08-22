# IntegrityAnalysis — open issues

Working list of outstanding work, newest thinking first. Each entry says what
the work is, why it matters, and what "done" looks like. Closed items move to
the bottom rather than being deleted, so the reasoning survives.

---

## Where things stand — 2026-08-21 (session handoff)

**A full Carlisle-2017 validation run is IN FLIGHT**, launched detached
2026-08-21 morning (survives every session): the shipped engine over
all 5,080 One Sheet trials at mMax = 100,000, via the committed runner
`corpus/validateCarlisle2017.R`. Its 100-trial pilot gate PASSED first:
r = 0.9930 vs Carlisle's stored p-values, median |diff| 0.0164, alarm
concordance 99.0%, nothing refused. Progress:
`.NewCarlisle/validation2017/run.log` (a stats block prints at the
end); results accumulate in `results.csv` every 25 trials; if the
machine restarts, rerun `Rscript corpus/validateCarlisle2017.R` - done
trials are skipped. NEXT SESSION: read the stats block, record it in
issue 3, and adjudicate the outliers (|diff| > 0.10; ~112 expected
from the 2026-08-17 run, plus ~12 known his-p=1 artifacts).

**A parallel session is active in this repository** (ParsePDF glyph
work): PR #53 is OPEN and theirs; branch symbol-pua-and-dash-evidence;
worktree C:/Temp/ia-glyphs and private library C:/Temp/ia-lib are
THEIRS - do not touch. Their branch predates renv and the cleanup fix;
both reconcile when they merge main. See AGENTS.md "Working alongside
other sessions".

**The pipeline is fully mechanized and verified end to end** (all on
main): every PR runs the 586-passing-assertion suite + security
tripwire inside R CMD check on the renv-pinned environment (R 4.5.3,
121 runtime packages, lockfile refresh policy in AGENTS.md "renv");
production deploys only after R-CMD-check passes, at the tested SHA,
from a staged directory; preview apps purge on PR close with 15-minute
retries that fail red; the CRAN canary checks latest-everything every
Monday.

**The app** (production, deployed): five ways in including zipped
multi-file upload; editable validated grid with issue colors; adaptive
one-sided Monte Carlo; three-tab results workbook with an overall
Stouffer P; optional PowerPoint of actual-vs-expected distribution
graphs (Graph results checkbox). Security threat model and standing
conclusions in AGENTS.md "Security".

**Corpus efforts** (see corpus/README.md and the project memories):
Carlisle manual queue ~3,500 trials with a daily filing loop
(.NewCarlisle/inbox -> fileDownloads.R); Boldt (104 queued) and Fujii
(185) worklists built, both 100% manual - every legal automated route
is exhausted and the reasoning recorded.

**Open issues:** 1 (API), 3 (validation - in flight above), 5 (MC
parallelism; pairs with 11), 7 (survey), 8 (AI in deployment - BYOK
direction), 11 (live analysis feedback), 12 (median/IQR validation).

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

**Closed 2026-08-19** - see the Closed section at the bottom.

---

## 3. Validate the analysis against Carlisle's 2017 manuscript

**FULL RUN COMPLETE (2026-08-21, 57.8 min): the current shipped engine
reproduces Carlisle 2017.** All 5,080 trials joined, zero refused:
r = 0.9930, median |diff| = 0.0127, 90.3% within 0.05, alarm
concordance (p < 0.05 both ways) 99.0%; 39 his-p=1 z=+inf artifact
trials reported separately. Against the 2026-08-17 lost-scratchpad
benchmark (r = 0.991, concordance 97.4%): equal or better where it
matters. REMAINING: adjudicate the 121 outliers (|diff| > 0.10; 50 of
them disagree on the p < 0.05 alarm) - results and per-trial values in
`.NewCarlisle/validation2017/results.csv`; the worst five are recorded
in the results file (e.g. NEJM 670: ours 0.129 vs his 0.634).

Prior status for context (now historical): Corrected archaeology: the full
validation ALREADY RAN once, 2026-08-17, in a session scratchpad -
5,080 trials, r = 0.991, median |diff| 0.0095, 92% within 0.05,
alarm-zone concordance 97.4% - but its scripts were never committed
and the engine has since gained the adaptive staged scheme. Steve
ratified mid-p for ties (2026-08-20; already the app's convention).

The validation is now a COMMITTED, reproducible artifact:
`corpus/validateCarlisle2017.R` (pilot mode: 100 seeded trials at
m = 15,000; full mode: every trial at m = 100,000; resumable via
`.NewCarlisle/validation2017/results.csv` - rerunning skips done
trials). It reads the One Sheet raw rows through validateData()'s
native Carlisle aliases and joins to the repaired wide file by
journal + trial with the A&A >= 1235 numbering offset. The full run
was launched detached overnight 2026-08-21; check
`.NewCarlisle/validation2017/run.log` for progress and the final
stats block. REMAINING AFTER THE RUN: record the stats here, and
adjudicate the ~112 unexplained outliers (|diff| > 0.10) the
2026-08-17 run left open (plus ~12 known his-p=1 z=+inf artifacts,
reported separately by the script).

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
a bug. (Pilot correction: the stored `p.value` column itself is the raw
one-sided value — it exceeds 0.5 freely; `p.value 2-sided` is the folded one.)

**Pilot results — 2026-08-16, 100 random trials, m = 15,000.** Run through the
app's own `validateData()` → `P_Calc()` from `One Sheet Carlisle Data.xlsx`,
compared against the repaired `Carlisle Data with PMIDs.xlsx`:

- **Direction confirmed**: our PLE correlates with his `p.value` as-is
  (r = +0.88), so both count small = too homogeneous.
- **Tie handling is the method difference.** As shipped, `P_Calc` counts all
  simulated ties into PLE (P(<) + P(=)), and agreement is mediocre (median
  |diff| 0.076, 36% within 0.05, always in the ours-higher direction — the
  signature of tie inflation on a rounding-discretized statistic). Recomputing
  as **mid-p** (P(<) + P(=)/2, recoverable from P_Calc's own PLE/PGE since
  PEQ = PLE+PGE−1): per-variable r = 0.996 with median |diff| 0.006; per-trial
  r = 0.995, median |diff| 0.013, **93% within 0.05**. Carlisle's published
  values are, in effect, mid-p. The residual is consistent with two
  independent Monte Carlos of finite size.
- **Decision needed from Steve before the full 5,087-trial run**: adopt mid-p
  (matches Carlisle; arguably the standard choice for discrete statistics),
  keep the current full-tie convention (more conservative — higher p, fewer
  alarms), or fold the choice into issue 6's redesign, which is deciding the
  p-value's meaning anyway. Validation should then be run with whichever
  convention is chosen for the comparison, and the app's convention documented
  either way.
- **Bookkeeping**: the two files' journal names differ (one-sheet uses BJA /
  CJA / EJA / NEJM / JAMA / "Anesthesia and Analgesia"; the wide file uses
  full journal names, `&amp;` included) — an 8-entry lookup joins them, to be
  used for the full run. Per-journal trial counts agree to within a few
  trials. The wide file's formula columns (`p.value 2-sided`, `Variables`)
  have no cached values and read as NA from R — recompute, don't read.

**FULL VALIDATION — 2026-08-17, VALIDATED.** All 5,080 joinable trials
(100% join with the lookup; zero rows dropped), m = 15,000, mid-p build
(PR #8): **r = 0.991 against Carlisle's stored p.value, median |diff|
0.0095, 92% within 0.05, alarm-zone (folded p < 0.05) concordance 97.4%**
(734 flagged by both, 75 ours-only, 55 his-only). The residual is
consistent with two independent Monte Carlos. Two artifact families
explain most of the worst tail, neither a method problem:

- **A&A numbering drift in the One Sheet**: its Anesthesia & Analgesia
  sequence skips the wide file's trial ~1234 and renumbers, so one-sheet
  trials ≥ 1235 correspond to wide trial t+1 (five more trials missing at
  the end account for the 1,282 vs 1,288 count). Correcting the offset
  realigned 49 trials from median |diff| ≈ 0.24 to ≈ 0.011. The One Sheet
  file itself has not been repaired — do that (or always apply the offset)
  before using it as a Carlisle ground-truth again.
- **The known his-p = 1 artifact** (12 of the 124 trials still off by
  > 0.10): a variable p of exactly 1 maps to z = +∞ (see the with-PMIDs
  repair notes); Steve's adjudication of those 33 rows is still pending.
  The other ~112 outliers are unadjudicated — most look like Monte Carlo
  extremes and per-variable count mismatches; the sorted list is
  `carlisle_final_validation.csv` (session scratchpad, also sent to Steve).

Remaining before closing: merge PR #8 (the mid-p convention the validation
ran under), and decide how far to adjudicate the 112 unexplained outliers.

---

## 4. Build a comprehensive test suite

**Closed 2026-08-19** - see the Closed section at the bottom.

---

## 5. Optimise the Monte Carlo

**Status 2026-08-20:** the adaptive staged scheme (1,000 -> 10,000 ->
100,000 replicates, escalating only while a row alarms) shipped
2026-08-17 and is the big practical win - typical rows now cost 1,000
replicates. Chunked simulation bounds memory; dqrng supplies fast
draws. Remaining: profiling, vectorisation of what is left, and
trial-level parallelism - which is the same plumbing as issue 11 and
should be designed with it.

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

**Closed 2026-08-20** (implemented 2026-08-17) - see the Closed section.
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

**Candidates raised by Steve, 2026-08-17:**

- **SPRITE** (https://shiny.ieis.tue.nl/sprite/, Heathers et al.):
  reconstructs the possible samples behind a reported mean/SD of a bounded
  integer scale. Passes the too-few-numbers test because it works on a
  *single* reported pair, not a distribution of digits. Complementary, not
  overlapping: it catches impossible or wildly implausible mean/SD pairs
  within one row, where Carlisle–Shafer catches improbable *agreement
  across arms*. Natural fit as a per-row plausibility flag (with GRIM/
  GRIMMER) in the validation pass — cheap, deterministic, no simulation.
- **Barnett's Bayesian baseline method**
  (https://f1000research.com/articles/11-783,
  https://aushsi.shinyapps.io/baseline/): a Bayesian re-formulation of the
  Carlisle approach modeling under/over-dispersion of baseline t-statistics
  with posterior probabilities instead of a frequentist p. Same evidence,
  different inferential wrapper — so it is a *cross-check*, not an
  independent signal. Worth reporting alongside if the posterior framing
  helps editors; the corpus/TEST mass-test set (61 verified PDFs +
  Carlisle expectations) is exactly the benchmark to run both methods on
  and compare verdicts, as Steve proposed.

Evaluation plan when picked up: run Carlisle–Shafer, Barnett, and
SPRITE/GRIM flags over corpus/TEST; compare per-trial calls; adopt what
adds discrimination, report what merely agrees as corroboration.

---

## 8. A URL keyword that unlocks AI parsing (future)

**Direction update 2026-08-20:** the likelier shape is now
publisher-supplied API keys (BYOK) - a publisher passes their own
Anthropic key per request (API) or per call (R package;
`parseBaselineTableAI(apiKey=)` already accepts one), charges land on
their account, and supplying the key doubles as informed consent to
send manuscript text to the model. A URL keyword would bill Steve;
BYOK bills the beneficiary. Security requirements if built: the key is
never stored or logged, and dies with the session.

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

**Closed 2026-08-20** (merged 2026-08-17, PR #9; the old repository is
retired) - see the Closed section.
---

## 10. Restructure the repository as an R package (stanpumpR model)

**Closed 2026-08-20** (phase 5, the deploy trio, completed 2026-08-19;
the deploy gained its test gate 2026-08-20) - see the Closed section.
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
PR #2 (full-page spinner, closed 2026-08-20 as superseded) was another
symptom of the same itch; a spinner still cannot update *during* the
computation. (2026-08-20: download-side feedback exists - the graphs
build counts its slides and greys its button - but the ANALYSIS itself
still only ticks per trial.)

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

## 12. Median/IQR rows (IMPLEMENTED 2026-08-17; validation approach open)

Steve's design (2026-08-17), confirmed after discussion: **Q1 and Q3
columns** (two quartiles, not a single IQR width — the quartiles carry the
asymmetry, and papers print `median [Q1–Q3]` anyway); when both are filled
in, **MEAN is read as the MEDIAN** and SD/SE must be empty. Validation
enforces completeness (both quartiles, N, median), unambiguity (no SD/SE
alongside), and Q1 ≤ median ≤ Q3.

**The Monte Carlo null** reconstructs the common population from the pooled
(N-weighted) median and quartiles with a **3-term metalog** (Keelin 2016):
matches m/Q1/Q3 exactly including skew, closed-form quantile function
(`X = a1 + a2·logit(u) + a3(u−½)·logit(u)`), reduces to a symmetric
logistic when the quartiles are symmetric. Steve's shared Gemini analysis
(https://share.gemini.google/p0YqsFwQKEIw) surveyed the options; note its
printed a3 coefficient is off by a factor of 2 against its own derivation —
the correct value is `a3 = 2(Q1+Q3−2m)/ln 3`, pinned by an
exact-quantile-recovery unit test. Rows with |a3/a2| > 1.667 (metalog
validity bound) are refused with a message rather than mis-simulated.
Simulated arm MEDIANS are rounded as printed (observations to
ROUND_OBSERVATION, medians to ROUND_MEAN) and compared by the same
between-arm sum of squares, lower mid-p tail, Stouffer combination.

**Validation status**: no Carlisle-style ground truth exists for median
rows. In place: a calibration property test (honest lognormal trials →
p roughly uniform; in the testthat suite) and direction tests. Worth
considering later: a larger calibration study across N, skew, and rounding
regimes, and checking the metalog null against other plausible shapes for
sensitivity.

---

## 13. Color-coded grid cells replace the parse-error list

**Closed 2026-08-20** (implemented 2026-08-18) - see the Closed section.
---

## 14. Documentation moves to HTML

**Closed 2026-08-19** - see the Closed section at the bottom.

---

## 15. Journal-style baseline table view for editors

**Closed 2026-08-20** (implemented 2026-08-19) - see the Closed section.
---

## 16. Graphs of actual vs expected squared-error distributions

**Closed 2026-08-20** - see the Closed section at the bottom.

---

## 18. Parse engine emits median [Q1, Q3] (IMPLEMENTED 2026-08-21)

Steve's decision (2026-08-21, while scoping the docx work): the engine's
unconditional skip of `medianRng` cells ("integrity analysis needs mean
and SD") predated issue 12 - the app has accepted median/Q1/Q3 rows
since then (metalog null) - so the skip was stale, and median support
belongs in the shared parse engine now rather than as a future issue.

As built (R/tokenize.R + R/parseBaselineTableHeuristics.R):

- The `medianRng` token gained the comma/semicolon separator ("127
  [98, 160]" - the printed IQR form, which the dash-only pattern never
  matched) and now carries its THIRD number (`num3`/`dec3`; extraction
  is structured via `.ppMedianParts`, because a bare number grep read
  the separator dash of "127 [98-160]" as a minus sign).
- **Emission is gated on text, never numbers**: an IQR and a min-max
  range both straddle the median, so they are numerically
  indistinguishable, and feeding a range into the quartile-matched
  metalog would be a correctness bug in a fraud-screening verdict. The
  row's own label outranks the caption/footnote (one table can print
  IQR and range rows side by side); "interquartile range" is recognized
  as an IQR statement, not a range statement. Verdicts: IQR stated ->
  emit MEAN(=median)/Q1/Q3 (SD/SE empty, ROUND_MEAN from the printed
  median); range stated -> skip "the analysis needs quartiles (Q1/Q3),
  not the range"; nothing stated -> skip "median with an unlabeled
  interval - if it is an IQR, enter median/Q1/Q3 by hand". A median
  outside its own interval also refuses.
- Q1/Q3 columns appear in the output only when a median row was
  actually emitted - `.ppBaseColumns()` is unchanged, because the AI
  path and the hybrid merge index by it and adding two always-empty
  columns would clutter every parse.

The wide-spreadsheet parser (issue 17) applies the same gate on its own
cells; the docx parser (issue 19) inherits this engine change for free.
Validation: tests/testthat/test-parse-median.R (all three gate
verdicts, both separators, label-beats-footnote, end-to-end
validateData acceptance); the existing median [range] fixtures in
test-parse-synthetic.R and test-app-pipeline.R still skip, now with the
quartile-focused reason.

---

## Closed

### 2. Point https://integrityanalysis.io at the app (closed 2026-08-19)

Went with the landing-page design the issue recommended, served by
GitHub Pages from `site/` in this repository (published by
`.github/workflows/pages.yaml` on every push to main):

- https://integrityanalysis.io - landing page: the method (screening
  signal, never a verdict), usage, the data-retention promise stated
  before anyone uploads, Carlisle 2012/2015/2017 by DOI, Launch buttons.
- https://integrityanalysis.io/app - stanpumpr.io-pattern full-screen
  iframe of the shinyapps deployment, keeping the domain in the address
  bar (shinyapps serves its own HTTPS inside the frame - no plan
  upgrade needed).
- https://integrityanalysis.io/guide.html - the user guide, republished
  from the same file the app serves.

DNS stayed at pairNic (four A records to GitHub Pages IPs + www CNAME
to stevenlshafer.github.io, replacing the old HTTP-only forwarder).
Let's Encrypt certificate provisioned by GitHub; HTTPS enforced. When
certificate provisioning stalls after a DNS change, removing and
re-adding the Pages custom domain restarts it (it issued within a
minute of the toggle). Built for inviting journal editors-in-chief
after John Carlisle's review.


### 4. Build a comprehensive test suite (closed 2026-08-19)

Done across the 2026-08 feature PRs and the consolidation PR. The suite
(tests/testthat/, 514 assertions) now covers every priority the issue
listed: the input contract (`test-input-contract.R` - name
normalisation, Carlisle aliases, `is_category`, graceful failure on
missing required columns, which was found CRASHING the session and
fixed in the same PR); known-answer Monte Carlo under fixed seeds
(`test-known-answer.R`, including the documentation's worked example at
p = 0.0495 and the categorical direction check); degenerate inputs
(zero-SD arms, N = 1, single arm, degenerate categoricals, single-line
categoricals, empty rounding columns); and the round-trip
(`test-app-grid.R` - Download Table re-imports and revalidates
identically). The app pipeline runs headlessly end to end
(`test-app-pipeline.R`: upload -> validate -> analyze, re-run
non-append, purge-on-exit, synthetic-PDF-to-grid-to-analysis), plus the
feature suites added with their PRs (cell colors, appending uploads,
baseline view, median/IQR, adaptive m). Everything runs from synthetic
data and synthetic PDFs - no corpus or Carlisle files needed - so the
suite is CI-ready (the GitHub Actions issue remains open in the
restructure plan, phase 5).

TESTING RULE learned the hard way: never hand a real file's path to
`input$upload` in `testServer` - stage a copy in its own subdirectory
of `tempdir()` first (the purge-on-exit handler deletes uploaded
paths).


### 14. Documentation moves to HTML (closed 2026-08-19)

Done exactly as the issue recommended. The Word original became
`docs/user-guide.md`, maintained in the repository and rendered by
pandoc to the self-contained `inst/extdata/IntegrityAnalysis.html`
(regeneration command in the Markdown's header comment and AGENTS.md).
The sidebar's "Download Documentation" button is now a **View
Documentation** link to https://integrityanalysis.io/guide.html, opened
in a new tab - the very same file, republished by
`.github/workflows/pages.yaml`, so the guide a reader sees is always the
guide the deployed app was built from and no one reads a stale copy out
of their Downloads folder. No PDF is generated; nobody asked for the
file back.

Closing it also completed the guide's "Results and downloads" section,
which now describes all three worksheets of the results workbook - the
`Test Results` audit trail column by column, the `Baseline Tables`
reconstruction and its formatting rules, and the one-line-per-study
`Summary` - rather than listing the tabs.


### 16. Graphs of actual vs expected squared-error distributions (closed 2026-08-20)

Built the day it was filed. A **Graph results** checkbox sits beside
Download Results; ticked, the download becomes a zip of the workbook
plus `Integrity Analysis Graphs.pptx`. The deck follows the granularity
settled that morning: an all-trials slide (observed cumulative
distribution of trial p-values against the uniform diagonal - the
Carlisle 2012 Fujii figure), one slide per trial over its variables,
and one slide per variable with p <= 0.01 showing the expected
distribution of the squared-error statistic with the observed value
marked in red - the Monte Carlo draws the engine had always generated
and discarded, now kept by a collector that leaves the returned results
bit-identical (pinned by test).

Steve's Excel-tabs alternative was considered and declined: native
Excel charts cannot express these figures (an ECDF against a diagonal,
a density with an observed marker), metafile paste is a Windows-only
idiom and the server is Linux, and images in worksheet tabs neither
scale nor print. The metafile INSTINCT - editable vector graphics - is
delivered better in the deck itself: rvg inserts every graph as native
PowerPoint drawing objects, so axes, bars, and labels can be restyled
by hand. Slides are also simply the right habitat for what these are:
exhibits, shown to editors and integrity committees.

Implementation: officer + rvg (both CRAN); the collector hooks
.stagedTail's first stage (1,000 draws, 8 KB per row, zero RNG
disturbance); the PowerPoint is built at download time only when the
box is ticked. R/distributionGraphs.R, tests in
test-distribution-graphs.R.


### 6. One-sided p toward homogeneity (closed 2026-08-20; implemented 2026-08-17)

The issue's one "still to do outside the code" - the PDF documentation
still describing the old two-column output - was resolved by issue 14:
the PDF is retired, and docs/user-guide.md documents the single-column
one-sided p (Steve confirmed complete, 2026-08-20).

The app reports a single one-sided p toward excessive homogeneity - the
demonstrated fraud signal (Fujii); heterogeneity is deliberately not
reported. As built: mid-p with the Davison-Hinkley floor per row;
"<0.0001" shown only when the 97.5% Clopper-Pearson bound licenses it;
Stouffer across rows, unfloored (accumulation IS the signal), with a
parametric-bootstrap interval when the trial p < 0.001. Downstream, the
same convention runs through the Summary sheet's overall P (2026-08-20)
and the distribution graphs (issue 16). The tie-convention question for
the Carlisle comparison lives in issue 3.

### 9. ParsePDF folded in (closed 2026-08-20; merged 2026-08-17, PR #9)

The parser lives in R/ as package code (8 parser files + the module
doc), its 295-assertion suite came with it, and the old private
repository is retired. Validated at r = 0.991 against Carlisle before
the move (issue 3 pilot). The corpus tooling (corpus/) and the
optimization loop (AGENTS.md) operate on it in place; the 2026-08-20
security review covered it (parses run in a time-limited subprocess;
poppler never executes document content).

### 10. Package restructure (closed 2026-08-20; phase 5 done 2026-08-19)

All five phases of docs/package-restructure-plan.md are live:
DESCRIPTION/R-package scaffolding, code moved to R/ with roxygen, the
testthat suite, R CMD check green in CI, and the deploy trio
(production deploy, per-PR preview apps, close-time cleanup). Since
then the pipeline gained the security tripwire (2026-08-20) and the
deploy now waits mechanically on R-CMD-check success, deploying the
exact tested SHA.

### 13. Color-coded grid cells (closed 2026-08-20; implemented 2026-08-18)

The grid carries the diagnosis: yellow = missing, red = unreadable
(parser skip rows keep their reason on hover), blue = incongruent, with
a legend that appears only when something is flagged. validateData()
returns the per-cell issue map; a custom handsontable renderer paints
it (writing via textContent - see the security review). The issue
taxonomy doubles as the API's machine-readable issues[] codes when
issue 1 is built. Incomplete parses under-detect (measured 2026-08-18:
a real alarm diluted from p = 0.014 to 0.099), which is why gaps are
conspicuous rather than silent.

### 15. Journal-style baseline table view (closed 2026-08-20; implemented 2026-08-19)

buildBaselineTables() reconstructs each trial's Table 1 from the
VALIDATED data - variables as rows, arms as columns with their Ns,
"mean (SD)" / "median [Q1, Q3]" / category counts, every value at the
printed precision the analysis assumed. Available as its own download
and as the results workbook's middle sheet: if the reconstruction
disagrees with the manuscript page, so did the analysis. Still a
candidate for the API response (issue 1).
