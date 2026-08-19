# The parsing corpus and master outcome sheet

This folder is the working surface for the **parser optimization loop**
(see AGENTS.md, "The parser optimization loop"): Steve's plan is to revisit
the PDF parser every few months with large language models, looking for
further optimization. An LLM arriving here should be able to see, without
any other context, **what the parser was asked to read, whether it
succeeded, and where it failed** — and then go read the code in `R/` with
concrete failures in hand.

## What is here

- **`ParseOutcomes.csv`** — the master sheet. One row per PDF in the
  corpus:

  | Column | Meaning |
  |---|---|
  | `PDF` | path relative to the corpus root (journal/year/file) |
  | `PMID` | PubMed ID where known (79.7%): from the filename for `PMID_<n>.pdf` files, else from `pmid_map.csv` — the committed lookup holding PMIDs recovered by OCR of printed citation lines (scanned EJA papers). Blank means no PMID has ever been matched for that file |
  | `OUTCOME` | `successfully parsed` / `not successfully parsed` — did the **deterministic** engine return a baseline table (AI fallback never used here, so the sheet measures exactly the code in `R/`) |
  | `COMMENTS` | success: the steps (table page found, layout, arms and how many carry an N, lines → variables, continuous rows, skipped lines, runtime). Failure: **where** the process stopped (table-page identification, or parsing after the page was found) and the error |
  | `PAGE … SECONDS` | the same diagnostics as raw columns, so analyses need not parse `COMMENTS` |

- **`buildParseOutcomes.R`** — regenerates the sheet by running the parser
  fresh over a corpus directory (chunked, resumable, one subprocess per
  PDF with an OS timeout — never a plain loop; ~2% of real PDFs hang
  poppler). Run it after any parser change:

  ```
  Rscript corpus/buildParseOutcomes.R C:/temp/journals C:/temp/ParseOutcomes_work
  ```

## Growing the corpus: the acquisition pipeline

The 1,865 PDFs on hand cover about a third of Carlisle's 5,088 trials.
Three scripts, run in this order, close as much of the gap as the law
allows — and make the remainder an explicit worklist rather than a
vague backlog. All three write into the gitignored `.NewCarlisle/`.

1. **`downloadNewCarlisle.R`** — the only script that fetches anything.
   It downloads exactly the papers in **PMC's open access subset**, which
   is explicitly licensed for bulk retrieval, and records every PMID's
   outcome in `.NewCarlisle/manifest.csv`. Result: of 5,084 PMIDs, 4,771
   have no PMC record, 308 are in PMC but not bulk-licensed, and **5 were
   downloadable**. "Free to read" is not the same as licensed; free-to-
   read deposits and publisher backfiles are deliberately skipped.

2. **`unpaywallDiscovery.R`** — metadata only, downloads nothing. Asks
   Unpaywall where a legal open copy of each DOI lives and under what
   license, into `.NewCarlisle/unpaywall.csv`. This separates a CC-
   licensed copy (a script may fetch it) from "bronze" (free to read on
   the publisher's site, no license — a human may read it, a script may
   not). One request per second; resumable.

3. **`buildDownloadList.R`** — the worklist. Joins the trial-level master
   sheet (`Carlisle Data with PMIDs and DOIs.xlsx`, sheet `All Data`)
   against the two files above plus `pmid_map.csv` and
   `ParseOutcomes.csv`, and writes
   `.NewCarlisle/DownloadPriorityList.xlsx`: one row per trial, **sorted
   ascending by Carlisle's trial p-value**, so the most homogeneous — most
   worth examining — baseline tables come first. Sheet `Queue` holds the
   trials with no automated route and no PDF yet; `Full list` holds all
   5,088. Each row carries PubMed, publisher, Stanford-proxy and open-
   access links and the file name to save under (`PMID_<pmid>.pdf`).
   Re-run it as the census and the downloads progress.

4. **`downloadLicensedOA.R`** - the second and last automated fetch.
   The census showed a few dozen articles are openly licensed somewhere
   the PMC pass could never see (a university repository, a publisher's
   own site), so this re-queries those DOIs, keeps the locations whose
   OWN license is CC or public domain, and downloads their direct PDFs.
   **"other-oa" is excluded on purpose**: Unpaywall uses it for "open
   access, license unstated", which is free to read, not licensed to
   retrieve. It yielded 3 more papers; 34 of the candidates have no
   licensed *direct* PDF (landing pages only) and stay in the queue.

5. **`fileDownloads.R`** - the other half of the manual loop. Papers
   arrive from publishers under names like `NEJMoa063186.pdf` or
   `1-s2.0-S0007091217363808-main.pdf`, and every one has to become
   `PMID_<pmid>.pdf`. This identifies each file from its own contents -
   a DOI in the text, a PMID, the DOI hiding in the file name, or the
   title matched against the master sheet and confirmed by the volume
   and page printed on page 1 - then moves it into `.NewCarlisle`.
   Anything it cannot pin down is left alone and listed; a mis-filed PDF
   would become a wrong baseline table later, so it never guesses.

**The daily loop**, once the queue exists:

```
open .NewCarlisle/DownloadPriorityList.xlsx, work down the Queue sheet
save the PDFs into .NewCarlisle/inbox   (any names; the filer sorts them)
Rscript corpus/fileDownloads.R          (identify, rename, move)
Rscript corpus/buildDownloadList.R      (queue shrinks by what you filed)
```

The filer reads `.NewCarlisle/inbox` and **not** the Downloads folder:
Steve reviews for many journals, so that folder holds confidential
manuscripts no script here should be opening.

Steve works down the `Queue` **a handful per day** through Stanford's
Lane Library. Individual downloads are within Lane's terms; systematic
bulk retrieval is not, and no script in this repository attempts it.

## What is deliberately NOT here

**The PDFs themselves.** The corpus is 1,865 published journal articles
(Anaesthesia, Anesthesiology, Anesthesia & Analgesia, BJA, CJA, EJA —
2000s vintages), which are copyrighted and must never be committed. They
live locally at **`C:/temp/journals`**, organized as
`<journal>/<year>/<n.m>.pdf`. The test suite does not need them: it
builds its own synthetic PDFs with the `pdf()` device
(`tests/testthat/helper-syntheticPdf.R`).

## Current baseline (2026-08-16 engine)

- 1,865 PDFs → **1,341 parsed (71.9%)**.
- 905 trials yield continuous rows — 12,500 in all; 58% of Carlisle's
  known mean/SD pairs recovered exactly, 99.4% agreement on decimal
  places.
- Failure modes, roughly in order: no baseline table identified in the
  text layer; scanned image with no text layer (needs `ocr = TRUE`);
  poppler hang/crash (40 s timeout); table page found but no parsable
  arm/variable structure.
- Ground truth for scoring value-level accuracy: Carlisle's spreadsheets
  at the repository root (see AGENTS.md; the `One Sheet` file's A&A
  numbering drifts by +1 from trial 1235 — see ISSUES.md issue 3).

## The local test set

Seven PDFs sit in this folder locally (gitignored — they are copyrighted
articles) for hand-testing PRs in the app. Each exercises a different
parsing challenge; the original corpus file is in the name, so its row in
`ParseOutcomes.csv` has the full diagnostics. Expected behavior when
uploaded to the app:

| File | Challenge | Expected in the app |
|---|---|---|
| `Test1_clean_…` | fully parseable, 2 arms with N, 10 continuous rows | parses, validates, **Analyze runs** (p ≈ 0.78) |
| `Test2_missingN_…` | table parsed but **no arm carries an N** (5 arms, 20 continuous rows) | parses; every row's N cell paints yellow (missing); fix via grid or extracted-table download |
| `Test3_categorical_…` | parses with **no continuous rows** — categories only | parses; chi-square-only analysis path |
| `Test4_skippedlines_…` | 29 of its table lines are **skipped as unusable** | parses; each skipped line becomes a grid row with a red ROW cell (hover for the parser's reason), excluded from analysis until filled in or deleted |
| `Test5_scanned_…` | **scanned image, no text layer** | fast clean failure: "scanned image" message, Template guidance |
| `Test6_hang_…` | **poppler does not finish** — the parser subprocess hits its timeout | ~60 s wait, then clean failure; the app must stay responsive |
| `PMID_12693995.pdf` | text layer present but **table-page identification declines** | fast clean failure; prime optimization-loop material |

## The TEST corpus (corpus/TEST) and its selection rule

61 PDFs (gitignored, copyrighted) used for mass end-to-end testing
against the Carlisle ground truth (`buildTestSet.R` built it;
`runMassTest.R` runs it; `compareResults.R` scores it on the log scale).

**The selection rule is SUBSET verification, and this matters when
reading comparisons:** a PDF qualified if it parsed fully (all arms with
N, ≥ 3 continuous variables) and **every extracted (MEAN, SD) pair
matched a Carlisle pair** — i.e., nothing extracted was *wrong*. It did
NOT require that everything Carlisle hand-entered was extracted. As
measured 2026-08-18: 47 of 61 parses recover exactly Carlisle's variable
set; **14 of 61 recover fewer** (median 2 fewer, worst 6); none recover
more. Trial-level p-value comparisons for those 14 therefore compare
*different variable sets* — an input difference, not an engine
difference (both adjudicated outliers below confirmed the engine matches
Carlisle within Monte Carlo noise on identical input).

## Optimization-loop specimens

Catalogued parses worth studying when re-attacking the parser (add to
this list as adjudications find more):

- **PMID_12693995.pdf** (in this folder): text layer present, "Table 1
  Participants' characteristics" printed on page 4, yet table-page
  identification declines. The page-id heuristic's cleanest known miss.
- **PMID_14984519.pdf** (in TEST/): the subtle one. The parse recovers
  SEVEN variables — the same count as Carlisle — yet three of Carlisle's
  value-pairs (his variables 3, 4, 6) are absent, so the parser's rows do
  not map onto his (suspect an arm-column or row-merge artifact).
  Consequence measured 2026-08-18: the missed variables carried the
  homogeneity signal, diluting a genuine alarm from p = 0.014 to
  p = 0.099 — incomplete parses can **under-detect**, which is why the
  grid workflow and the planned issue-13 cell coloring must make gaps
  conspicuous rather than silent.

## Ground rules for optimization passes

1. **Deterministic first.** The deployed path must stay deterministic and
   offline (confidential manuscripts; reproducible verdicts). AI-assisted
   parsing exists (`parseBaselineTableAI()`) but is fallback-only and off
   in deployment.
2. **Regressions are cheap to catch**: the testthat suite (~295
   assertions) pins every corpus defect found so far — font-encoding
   repairs, the SD/SE separation, `4,335` vs `4.335`, en-dash vs ±. Keep
   it green; add a fixture for every new defect found.
3. **Score before and after.** Regenerate `ParseOutcomes.csv` and compare
   parse rates; for value-level accuracy, score against Carlisle. A change
   that parses more tables but mis-reads more numbers is a regression.
