# 2. Likelihood ratio test

*Thesis section 2.1. Example: [`R/02_likelihood_ratio_test.R`](../R/02_likelihood_ratio_test.R).*

## What the thesis did

The likelihood ratio test (Neyman & Pearson, 1928) was the simplest of the
four selection methods, and it was **non-sequential**:

1. Fit the full model with every candidate predictor.
2. Test each predictor by comparing the full model with the model that leaves
   it out.
3. Keep the predictors whose test is significant at the 5% level.

In R this is `drop1(model, test = "LRT")`, followed by keeping the rows with
`Pr(>Chi) <= 0.05`.

## The idea

If a predictor matters, removing it should make the model fit noticeably
worse. The test statistic is twice the drop in log-likelihood, which equals
the increase in deviance (eq 2.1):

```
LRT = −2 ( ln L_reduced − ln L_full ) = deviance_reduced − deviance_full
```

If the predictor has no effect, the statistic follows a chi-squared
distribution. Its degrees of freedom equal the number of parameters removed.
For a factor that means **one per indicator column**: a four-level predictor
is tested on 3 degrees of freedom, as a whole.

## The example

On a simulated sample of 500 students:

- **By hand and with `drop1()`:** dropping the negative-teacher-interaction
  factor gives the same statistic both ways, 2.32 on 3 df.
- **As a selection rule:** at the 5% level the LRT keeps only 2 of the 8 true
  predictors (attitude to primary maths and positive teacher interaction),
  even though all eight are in the model that generated the data.

## What to keep in mind

Each test is run on its own, so several tests are run at once. The thesis
notes that this raises the chance of a false inclusion (Miller, 1981). It also
means that with a small sample, a real but modest effect may not reach
significance. This is the "missing true predictors" behaviour the simulation
measured.

## In the code

[`code/original/thesis_simulation_study_2020.R`](../code/original/thesis_simulation_study_2020.R),
the LRT blocks: `LRT = drop1(glm(...), test = "LRT")`, then
`LRT_Coeff = Beta_Significant[which(LRT$'Pr(>Chi)' <= 0.05) - 1]`.
