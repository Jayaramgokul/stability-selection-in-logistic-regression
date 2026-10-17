# 4. The lasso for logistic regression

*Thesis section 2.3. Example: [`R/04_lasso.R`](../R/04_lasso.R). Thesis figure: [Fig 2.1](../figures/thesis/fig2_1_lasso_cross_validation.png).*

## What the thesis did

The lasso (Tibshirani, 1996) adds a penalty on the size of the coefficients
to the logistic log-likelihood (eq 2.10):

```
β̂_λ = argmin_β { −ℓ(β) + λ Σ |β_j| }
```

The penalty strength λ was chosen by **10-fold cross-validation** (Greenshtein
& Ritov, 2004) with `cv.glmnet(..., family = "binomial", alpha = 1)`. The
selection used **`lambda.1se`**: the largest λ whose cross-validated deviance
is within one standard error of the best.

## The idea

As λ grows, it becomes more "expensive" to keep a coefficient large, so
coefficients shrink. Because the penalty uses absolute values, some
coefficients shrink to **exactly zero**, which removes those predictors from
the model. The lasso therefore selects and estimates in one step.

Cross-validation (section 2.3, steps 1–4):
1. Split the data into k folds.
2. For each λ, fit on k − 1 folds and measure the deviance on the fold left
   out.
3. Average over the folds.

The thesis's Fig 2.1 is the resulting curve, with the λ that shrinks every
coefficient to zero at one end.

`glmnet` reports two values:
- **`lambda.min`**, the λ with the best cross-validated deviance;
- **`lambda.1se`**, a simpler model that is statistically as good.

## The example

On a simulated sample of 5,000 students (the size section 2.4 uses for
cross-validation), both rules keep all 8 predictors. The 1-SE rule zeroes
more indicator columns (14 non-zero against 18). The [chart](../R/output/04_lasso.png) shows the
cross-validation curve and the coefficient paths, with both λ values marked.

On a sample of 500, the 1-SE rule is much more conservative. It can remove
every predictor, which matters for the erratum below.

## A factor counts as selected if any of its levels is

The lasso works on indicator columns, not factors. The thesis code mapped
non-zero columns back to predictor names: a predictor counted as selected if
any of its levels kept a non-zero coefficient. The examples here use the same
rule.

## Erratum (2026)

In the thesis code's scenario-1 lasso loop (true predictors only), the list
of selected predictors was reset once, before the loop, instead of in each
run. Predictors selected in earlier runs were counted again in later ones.
This affects the lasso panel of Fig 4.1 and the related statements. See the
[README](../README.md#erratum-2026) for details and the corrected counts.
