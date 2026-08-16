# Hand-off folder

Written 2026-08-16 by the code-review session (Claude Code, model Claude
Fable 5) at Steve Shafer's request, to merge two parallel Claude Code
dialogs into one starting point for a new session rooted in this
directory:

1. **The code-review dialog** — reviewed, fixed, and deployed the Shiny
   app (PR #4). Its full state is in
   [2026-08-16-merged-handoff.md](2026-08-16-merged-handoff.md).
2. **The PDF-parsing dialog** — built the ParsePDF package and the
   deterministic baseline-table parser. Its own hand-off is the
   "Where things stand — 2026-08-16" section at the top of
   [../ISSUES.md](../ISSUES.md), which is the **canonical open-issues
   list**; nothing in this folder supersedes it.

## How to resume

Start a new Claude Code session with this repository as the working
directory (`cd C:\dev\IntegrityAnalysis` then `claude`). The root
`CLAUDE.md` is read automatically and points here. Read, in order:

1. `CLAUDE.md` (orientation, runbook, conventions)
2. `ISSUES.md` (what to do next, and the PDF-parsing thread's state)
3. `handoff/2026-08-16-merged-handoff.md` (the code-review thread's
   state, operational details, and hazards discovered the hard way)
4. `handoff/2026-08-16-rename-and-relaunch.md` (the repo's rename from
   `Integrity-Analysis` to `IntegrityAnalysis`, and the decoupling of
   all docs and session memory from other projects)

## House rule learned from a real collision

Two agents shared this repository and one lost committed-but-unpushed
work when the other deleted a merged branch (recovered from the reflog
in commit `81b03cf`). **In a repository two agents share, unpushed work
does not exist. Push immediately after committing.**
