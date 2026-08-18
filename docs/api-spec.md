# IntegrityAnalysis API — specification (v0 draft)

Provenance: drafted by Claude Code (model Claude Fable 5), 2026-08-17, at
Steve Shafer's request, implementing the contract decided in ISSUES.md
issue 1. Status: **specification only** — reviewed by no one yet;
implementation follows Steve's sign-off.

## Purpose

Let editorial systems (Editorial Manager, ScholarOne) submit a manuscript
automatically and silently during peer review and receive a baseline-data
integrity screen, without a human operating the Shiny app. The API is the
programmatic face of exactly the pipeline the app runs: deterministic PDF
parsing → validation → one-sided Monte Carlo toward excessive homogeneity.

## Non-negotiable properties (from issue 1 and the app's guarantees)

1. **Deterministic.** No AI in the deployed path — same submission, same
   verdict, always. (Manuscripts under review are confidential; nothing
   may leave the server; verdicts may influence editorial decisions.)
2. **Zero retention.** The uploaded file and all derived data are deleted
   when the request completes; the response *says so* explicitly. No
   request bodies in logs; access logs carry request IDs, sizes, and
   timings only.
3. **A failure is a round trip, not a dead end.** A submission the
   pipeline cannot fully analyze returns the **partial table it did
   extract** plus machine-readable reasons, in a form that is itself
   valid input to the next call.
4. **No arm N, no analysis.** Refuse rather than guess (`reviewFlags()`
   is the gate); ~58% of parsed rows lack N, so this is the common
   partial outcome for PDF submissions.

## Transport and authentication

- REST over HTTPS; implementation target: R + {plumber} behind a reverse
  proxy, deployed separately from the Shiny app (same package, so one
  statistical engine — the API imports IntegrityAnalysis and calls
  `parseBaselineTableFiles` / `validateData` / `P_Calc`).
- Auth: per-integrator API key (`Authorization: Bearer <key>`), issued
  per publisher/editorial system. Keys gate spending and rate limits,
  not the method (the method is published).
- Rate limiting: per-key requests/hour and concurrent-jobs caps
  (defense in depth; PDF parsing is CPU-bound).
- Versioned base path: `/v1`.

## Endpoints

### POST /v1/analyze
Multipart upload, exactly one file: `file` = PDF **or** spreadsheet
(xlsx/xls/csv in the app's input layout). Optional form fields:
`m` (replications, default 15000, capped), `format` (`json` | `csv`,
default json).

**200 — full analysis** (`status: "analyzed"`):
```json
{
  "status": "analyzed",
  "engine": "deterministic",
  "trials": [{
    "trial": "manuscript",
    "p": 0.0132,
    "interpretation": "one-sided toward excessive homogeneity; small p = baseline data more similar across arms than random sampling explains",
    "rows": [{"row": "Age", "type": "continuous", "p": 0.021},
             {"row": "Sex", "type": "categorical", "p": 0.155}]
  }],
  "table": [ ...the analyzed table, app input layout... ],
  "retention": {"file_deleted": true, "data_retained": false}
}
```

**200 — partial extraction** (`status: "incomplete"`): the same shape,
plus per-cell issues instead of p-values:
```json
{
  "status": "incomplete",
  "table": [ ...partial table, valid as next-call input... ],
  "issues": [
    {"row": 3, "column": "N", "code": "missing",
     "message": "no arm N printed for 'Weight'"},
    {"row": 7, "column": null, "code": "skipped",
     "message": "median [range] line - needs mean/SD or median/Q1/Q3"}
  ],
  "retention": {"file_deleted": true, "data_retained": false}
}
```
Issue `code` taxonomy is shared with the app's color-coded grid
(ISSUES.md issue 13): `missing` (yellow), `unreadable` (red),
`incongruent` (blue), `skipped`, `refused` (e.g. quartiles too skewed).

**4xx/5xx**: `unsupported_media_type` (not PDF/xlsx/xls/csv),
`payload_too_large` (cap 50 MB), `unauthorized`, `rate_limited`,
`parse_failed` (nothing extractable; message mirrors the app's
plain-language failure), `internal` (request ID for support; no content
echoed).

### GET /v1/health
Liveness + engine version + package version. No auth.

### GET /v1/spec
This document (machine-readable OpenAPI once implemented).

## Processing rules

- PDF path: `parseBaselineTableFiles()` (subprocess, OS timeout 60 s —
  the poppler-hang defense is mandatory server-side). Spreadsheet path:
  read directly. Then `validateData()`; a FAIL maps to
  `status: "incomplete"` with the issue list, never a bare error.
- Statistics identical to the app: mean(SD), median(Q1,Q3) metalog,
  categorical lower-tail chi-square; mid-p; Stouffer per trial; single
  one-sided P.
- Synchronous response for single manuscripts (parse ≈ 1–5 s, analysis
  ≈ 1–2 s per trial at m = 15000). No batch endpoint in v1 — editorial
  systems submit one manuscript per event.

## Open decisions for Steve

- Hosting (shinyapps.io cannot host plumber: a small VM/container or
  Posit Connect).
- Key issuance process and per-key quotas; whether a public
  low-rate-limit demo key exists.
- Whether the response includes the reconstructed journal-style baseline
  table view (issue 15) for the editor email workflow.
