# How IntegrityAnalysis computes and reports its p-value

Provenance: written by Claude Code (model Claude Fable 5), 2026-08-17,
with the adaptive-replicates implementation; the scheme is Steve Shafer's
decision following the replicate-count analysis (independently convergent
with a Gemini analysis he commissioned). This is the user-facing
explanation; it will fold into the full documentation rewrite (issue 14).

## What the p-value means

For every baseline variable (each ROW of the table), IntegrityAnalysis
asks one question: **if the arms really were random samples from a single
population, how often would their printed summaries agree this well?**
The answer is a one-sided p-value toward *excessive homogeneity*:

- **Small p** = the arms are more alike than random sampling explains —
  the demonstrated fraud signal (it is how Fujii's fabricated trials were
  caught).
- Large p = nothing remarkable. Excessive *heterogeneity* is deliberately
  not reported: it is not a known fabrication signal, and reporting it
  invites false accusations against merely-variable data.

The per-variable p-values combine across the trial with Stouffer's
method into a single trial p.

## Where the numbers come from, and why they carry uncertainty

Each row's p is estimated by simulation: the app draws many replicate
trials under the random-sampling hypothesis, rounds the simulated
summaries exactly as the paper rounded its own, and counts how often the
simulated arms agree at least as well as the printed ones (ties count
half — the "mid-p" convention, which reproduces Carlisle's published
2017 values, r = 0.991 over 5,080 trials).

A simulated p-value is itself an estimate. If 0 of 1,000 replicates
agree as well as the printed data, the true p could still plausibly be
0.003 — so reporting "p < 0.001" from 1,000 replicates overstates the
evidence. IntegrityAnalysis is a screening tool whose verdicts may be
challenged, so it reports only what the simulation actually supports.

## The adaptive scheme

1. **Staged replicates.** Every row starts with 1,000 replicates. If its
   running p is ≥ 0.01, the simulation stops — extra precision on an
   unremarkable p changes nothing. Otherwise it escalates to 10,000, and
   if still < 0.01, to 100,000. Computation concentrates exactly on the
   rows where precision matters.
2. **No literal zeros.** A row where *no* replicate matched is floored at
   1/(replicates + 1) (Davison & Hinkley) — the smallest value the
   simulation can honestly claim.
3. **"< 0.0001" is a confidence statement, not an estimate.** A row
   displays "<0.0001" only when the one-sided 97.5% upper confidence
   bound (exact Clopper–Pearson, ties counted fully — conservative) on
   its simulated count clears 0.0001. At zero exceedances this needs
   roughly 30,000+ replicates; at 100,000 replicates the bound is
   3.7 × 10⁻⁵, comfortably below. Rows with p < 0.001 also show the
   bound explicitly ("<=4.6e-05"), and every row reports how many
   replicates it used.
4. **The trial p is not floored.** Combining rows is exact arithmetic —
   no simulation noise is added — and accumulation across rows is the
   fraud signal: eight individually unremarkable rows at p = 0.01
   legitimately combine to about 5 × 10⁻⁹. What the trial p inherits is
   the rows' simulation uncertainty, which is propagated (parametric
   bootstrap over the rows' binomial counts) and shown as a 95% Monte
   Carlo interval whenever the trial p < 0.001, e.g.
   "p = 3.1e-07 (95% MC interval 1.2e-07 to 8.9e-07)".

## Reading the results table

| Column | Meaning |
|---|---|
| P | The one-sided p toward homogeneity. "<0.0001" means the 97.5% upper confidence bound clears 0.0001. Text entries ("Only 1 Row", "Quartiles too skewed to simulate", ...) are refusals: the row could not be analyzed, with the reason. |
| 95% Monte Carlo bound | For rows: the upper confidence bound, shown when P < 0.001. For the Summary row: the 95% bootstrap interval of the trial p. |
| Replicates | Simulations this row actually used (1,000 for unremarkable rows; up to 100,000 for alarming ones). |

## One sentence for the skeptical reader

Every "<" statement this tool prints is licensed by an exact upper
confidence bound on its own simulation, not by a point estimate — the
number reported is the one the tool is prepared to defend.
