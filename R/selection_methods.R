# The four model selection methods compared in the thesis, written as small
# functions so the simulation examples can call them. Each follows the choices
# made in the thesis (chapter 2) and its R code (code/original/):
#
#   LRT        drop1(test = "LRT") on the full model; keep predictors with p <= 0.05
#   Stepwise   step() from the intercept-only model, AIC criterion, adding and removing
#   Lasso      cv.glmnet (10-fold, binomial); coefficients at lambda.1se
#   Stability  lasso refitted on 500 half-size subsamples without replacement, for every
#              lambda between lambda.min and lambda.1se; a predictor is selected if its
#              highest selection proportion over those lambdas is at least 0.7
#
# Each function returns the names of the selected predictors. A factor predictor
# counts as selected if any of its levels is, as in the thesis code.

suppressPackageStartupMessages(library(glmnet))

select_lrt <- function(d, candidates, response = "not_interested") {
  fit <- glm(reformulate(candidates, response), family = binomial, data = d)
  tests <- drop1(fit, test = "LRT")
  p <- tests[["Pr(>Chi)"]][-1]
  candidates[!is.na(p) & p <= 0.05]
}

select_stepwise <- function(d, candidates, response = "not_interested") {
  null <- glm(reformulate("1", response), family = binomial, data = d)
  chosen <- step(null, scope = list(lower = ~1, upper = reformulate(candidates)), trace = 0)
  intersect(candidates, attr(terms(chosen), "term.labels"))
}

lasso_design <- function(d, candidates) model.matrix(reformulate(candidates), d)[, -1]

select_lasso <- function(d, candidates, response = "not_interested") {
  X <- lasso_design(d, candidates)
  cv <- cv.glmnet(X, d[[response]], family = "binomial", alpha = 1, nfolds = 10)
  b <- coef(cv, s = "lambda.1se")[-1, 1]
  predictors_from_columns(names(b)[b != 0], candidates)
}

# 'regenerate' re-draws the response in each subsample from the true model, as the
# thesis code did; supply a function(d) returning a new 0/1 response.
stability_paths <- function(d, candidates, regenerate, subsamples = 500, response = "not_interested") {
  X <- lasso_design(d, candidates)
  cv <- cv.glmnet(X, d[[response]], family = "binomial", alpha = 1, nfolds = 10)
  lambdas <- cv$lambda[cv$lambda >= cv$lambda.min & cv$lambda <= cv$lambda.1se]
  counts <- matrix(0, length(lambdas), length(candidates), dimnames = list(signif(lambdas, 3), candidates))
  for (s in seq_len(subsamples)) {
    rows <- sample(nrow(d), floor(nrow(d) / 2), replace = FALSE)
    sub <- d[rows, ]
    y <- regenerate(sub)
    fit <- glmnet(X[rows, ], y, family = "binomial", alpha = 1, lambda = lambdas)
    for (k in seq_along(lambdas)) {
      b <- fit$beta[, k]
      chosen <- predictors_from_columns(names(b)[b != 0], candidates)
      counts[k, chosen] <- counts[k, chosen] + 1
    }
  }
  list(lambdas = lambdas, probability = counts / subsamples)
}

select_stability <- function(d, candidates, regenerate, subsamples = 500, threshold = 0.7, ...) {
  paths <- stability_paths(d, candidates, regenerate, subsamples, ...)
  best <- apply(paths$probability, 2, max)
  candidates[best >= threshold]
}

# Re-draw the response for a set of students from the true (thesis) model.
regenerate_from_truth <- function(noise_sd = 0) {
  function(d) {
    X <- model.matrix(reformulate(TRUE_PREDICTORS), d)
    eta <- drop(X[, names(THESIS_COEFFICIENTS)] %*% THESIS_COEFFICIENTS)
    if (noise_sd > 0) eta <- eta + rnorm(nrow(d), 0, noise_sd)
    rbinom(nrow(d), 1, plogis(eta))
  }
}
