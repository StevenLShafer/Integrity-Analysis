# Hand-off — repo rename and relaunch — 2026-08-16

Provenance: written by a Claude Code session (model Claude Opus 5) on
2026-08-16, at Steve Shafer's request, as the final act before closing the
session and relaunching Claude Code from this directory. All steps below
were executed and verified in that session.

## What changed

1. **The hyphen is gone.** The GitHub repository was renamed
   `StevenLShafer/Integrity-Analysis` → `StevenLShafer/IntegrityAnalysis`
   (Steve ran `gh repo rename`; verified). The local clone was already
   renamed by Steve to `C:\dev\IntegrityAnalysis`. Old GitHub URLs
   redirect permanently — but **never create a new repo named
   `Integrity-Analysis`**, which would kill the redirect.
2. **`origin` updated** to `https://github.com/StevenLShafer/IntegrityAnalysis.git`;
   fetch verified, `main` in sync.
3. **All repository docs decoupled from other projects.** References to
   other repositories on this machine were removed from `CLAUDE.md` and
   `handoff/2026-08-16-merged-handoff.md`; the useful warnings they
   carried (a foreign renv hijacking the library path if R starts in
   another project's directory; the `IntegrityAnalysis_PR_<n>` test-app
   naming) were kept in generalized form. Hyphenated names/paths were
   updated throughout (`ISSUES.md` title, handoff `deployApp()` appDir,
   PR #4 URL, resume instructions).
4. **Session memory seeded for this directory.** Sessions launched from
   `C:\dev\IntegrityAnalysis` use the Claude memory folder
   `C:\Users\steve\.claude\projects\C--dev-IntegrityAnalysis\memory\`,
   which was empty because all prior work ran from a different working
   directory. It has been populated with the project's accumulated
   knowledge (project locations, runtime facts, Steve's design vision,
   ParsePDF status and ground-truth corpus locations, the Carlisle
   Data.xlsx column-shift defect, the p-value direction decision, and
   Steve's working preferences). The equivalent notes were removed from
   the old memory folder so nothing points across projects in either
   direction.

## What did NOT change

- No code was touched: `global.R`, `server.R`, `ui.R` are exactly as
  merged in PR #4. Production was not redeployed (nothing to deploy —
  doc-only changes).
- The untracked working files (Carlisle data spreadsheets, `TestPapers/`,
  `rsconnect/`, `parseCovariateTable.R`, `testParseCovariateTable.R`)
  remain untracked, deliberately.

## Where to pick up

Read, in order: `CLAUDE.md` → `ISSUES.md` ("Where things stand", then the
numbered issues) → `handoff/2026-08-16-merged-handoff.md`. The suggested
sequence in ISSUES.md is: validate against Carlisle 2017 (issue 3), then
the test suite (issue 4) and the ParsePDF fold-in (issue 9). The blind
verification pass over the 905-trial corpus still needs its sequential
re-run (see "Where things stand").
