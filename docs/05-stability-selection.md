# 5. Stability selection

*Thesis section 2.4 and pseudo-code 3.5. Example: [`R/05_stability_selection.R`](../R/05_stability_selection.R).*

## What the thesis did

Stability selection (Meinshausen & Bühlmann, 2010) was wrapped around the
lasso. The thesis steps:

1. On a sample, run cross-validation and keep the λ values between
   `lambda.min` and `lambda.1se` as a set L.
2. Draw a subsample of about half the size, **without replacement**.
3. Fit the lasso at the λ values in L and note which predictors are selected.
4. Repeat steps 2–3 **500 times**. The proportion of subsamples in which a
   predictor is selected is its **selection probability**.
5. Take each predictor's highest selection probability over L. If it is at
   least **0.7**, the predictor is selected.

## The idea

Section 2.4 of the thesis sets out the problem. Selection methods are often
unstable: a slightly different sample gives a different set of predictors
(Křížek et al., 2007; Morozova et al., 2015; Nogueira & Brown, 2016).
Multicollinearity is the usual cause, because the lasso and stepwise pick one
predictor from a correlated group somewhat arbitrarily.

Stability selection does not trust a single fit. It asks how often each
predictor survives across many perturbed versions of the data, and keeps only
the predictors that survive consistently. Meinshausen & Bühlmann show that
this also controls false selections in finite samples.

## The example

On one simulated sample of 500 students, with the 8 true and 6 noise
predictors and 500 half-size subsamples:

- **Selected:** four true predictors pass the 0.7 threshold (negative and
  positive teacher interaction, attitude to primary maths, qualification
  sought).
- **Near misses:** three more true predictors fall just short, at 0.60–0.69.
- **Noise:** the noise predictors reach at most 0.56.
- **Smallest λ:** for **all 14 predictors**, the highest selection proportion
  occurs at the smallest λ in the set.

The [chart](../R/output/05_stability_paths.png) shows these stability paths, with true predictors in blue, noise
predictors in red and the threshold dotted.

## The smallest λ always wins

The thesis observed this in its own experiments (section 5.2): the lowest λ
always produced the highest selection counts, so the step of trying a range of
λ values "didn't play much of a meaningful role" and added computing time. The
final thesis code reflects this. In the simulation loops it fits the lasso
only at the last, and smallest, value in L (`Luniq[length(Luniq)]`).

The thesis lists better ways of choosing λ for stability selection as future
work.

## In the code

[`code/original/thesis_simulation_study_2020.R`](../code/original/thesis_simulation_study_2020.R):
- **Subsample loop:** the `for (q in 1:1000)` blocks, each containing the
  500-subsample loop `sample(..., 250, replace = F)`.
- **Threshold:** `which(stabfinal[q, ] >= 350)`, which is 350 of 500, so 0.7.
