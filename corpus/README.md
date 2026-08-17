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
