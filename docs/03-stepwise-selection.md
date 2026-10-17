# 3. Stepwise selection with AIC

*Thesis section 2.2. Example: [`R/03_stepwise_selection.R`](../R/03_stepwise_selection.R).*

## What the thesis did

Stepwise selection (Efroymson, 1960) used the Akaike Information Criterion,
following the steps in Hosmer & Lemeshow (2000):

1. Fit the intercept-only model and compute its AIC.
2. Try adding each candidate predictor; add one if it lowers AIC.
3. Once a predictor is in, also try removing each predictor; remove one if
   that lowers AIC.
4. Stop when no addition or removal lowers AIC any further.

In R: `step(glm(y ~ 1, ...), scope = list(lower = ~1, upper = ~ all predictors))`.

## The idea

AIC rewards fit and penalises size (eq 2.2):

```
AIC = −2 ln L + 2p          (p = number of parameters)
```

The penalty stops the model from adding predictors that barely improve the
fit. A four-level factor adds three parameters, so it must improve the fit by
more to get in. The thesis also mentions BIC, which penalises size more
heavily.

## The example

On a simulated sample of 500 students, the search goes like this:

1. It starts from the intercept only (AIC 523.8).
2. It adds attitude to primary maths, then positive teacher interaction, then
   year in school, then qualification sought.
3. It stops at AIC 504.9, with 4 of the 8 true predictors.

Computing AIC by hand, as −2 log-likelihood + 2 × 10 parameters, gives the
same value that `step()` reports.

## What to keep in mind

Stepwise follows a **single path** through the possible models, so the
predictors it ends with depend on the sample. The thesis points to this
instability (Breiman, 1996; Whittingham et al., 2006): a slightly different
sample can lead down a different path.

## In the code

[`code/original/thesis_simulation_study_2020.R`](../code/original/thesis_simulation_study_2020.R),
the `AIC = step(glm(Ynew ~ 1, ...), scope = ...)` blocks.
