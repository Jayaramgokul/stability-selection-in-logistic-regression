# 6. The simulation study

*Thesis chapter 3. Example: [`R/06_simulation_study.R`](../R/06_simulation_study.R).*

## Why simulate

Morris, White & Crowther (2019) describe how a simulation study should be
designed around the task being evaluated. For **model selection**, the
natural measures are how often the right predictors are chosen. The thesis
uses this. The key point is that in a simulation the truth is known, so a
method's selections can be scored against it. With real data alone, that is
impossible.

## The design (section 3.2)

| Element | Choice in the thesis |
|---|---|
| **Performance measures** | How often each predictor is selected; how often each *number* of predictors is selected |
| **True predictors** | The 8 predictors of the fitted model (section 1.3) |
| **Noise, type 1** | 6 extra predictors not in the true model. Some are related to true predictors through multicollinearity, others are pure noise: carer's qualification, social class, induction day, religious ethos, parent–teacher meetings, teacher adequacy |
| **Noise, type 2** | A normal random variate with mean 0 and SD 4 added to the linear predictor |
| **Sample sizes** | 500 and 5,000 |
| **Methods** | LRT, stepwise, lasso, stability selection around the lasso |
| **Scenarios** | 1. true predictors only; 2. true + noise predictors; 3. true predictors + random noise in the linear predictor |
| **Runs** | 1,000 per setting; 500 subsamples inside each stability-selection run |

## One run (pseudo-code, sections 3.3–3.5)

1. Draw a bootstrap sample of size n from the data.
2. Compute the linear predictor from the true model (adding the N(0, 4) term
   in scenario 3), then simulate a new response from it.
3. Apply the selection method and record which predictors it keeps.
4. Add to a counter for each predictor and for the number selected.

Repeating this 1,000 times gives the selection frequencies analysed in
chapter 4.

## The example

The script runs a smaller version on simulated students: n = 500, all three
scenarios, all four methods, 40 runs, and 100 stability subsamples per run so
it finishes in about two minutes. It writes every run to
`output/simulation_runs.csv` for the chapter-4 analysis in
[08](08-comparing-methods.md). The `RUNS` and `SUBSAMPLES` environment
variables restore the thesis's settings.

Average number of the 8 true predictors selected, in one example run:

| Scenario | Lasso | LRT | Stability | Stepwise |
|---|---|---|---|---|
| 1 true only | 0.75 | 2.95 | 7.20 | 4.35 |
| 2 true + noise | 0.68 | 3.00 | 6.22 | 3.98 |
| 3 true + random noise | 0.28 | 1.05 | 2.58 | 1.85 |

In scenario 2, stability selection also keeps the most noise predictors (2.3
of 6 on average, against 0.1–1.0 for the others). This is the "inclusive"
behaviour the thesis reported.

These are simulated students with assumed level frequencies, so the numbers
illustrate the design and should not be read as a reproduction of the
thesis's tables.
