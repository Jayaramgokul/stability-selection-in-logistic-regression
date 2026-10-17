# 01. Logistic regression (thesis section 1.3)
#
# Fits the thesis model to simulated students and shows that maximum likelihood,
# found by Newton-Raphson as described in the thesis, recovers the coefficients
# that generated the data. Run from the R/ folder:  Rscript 01_logistic_regression.R

source("simulate_data.R")
set.seed(2020)

d <- simulate_students(6359)                       # the thesis worked with 6,359 students
X <- model.matrix(reformulate(TRUE_PREDICTORS), d)
y <- d$not_interested
cat(sprintf("%d students, %d columns in the design matrix (intercept + %d indicators)\n\n",
            nrow(X), ncol(X), ncol(X) - 1))

# Newton-Raphson by hand: score vector S and Hessian H of the log-likelihood (eqs 1.12-1.15).
beta <- rep(0, ncol(X))
for (iteration in 1:25) {
  p <- plogis(drop(X %*% beta))
  S <- crossprod(X, y - p)                           # first derivatives
  H <- -crossprod(X, X * (p * (1 - p)))              # second derivatives
  step <- solve(H, S)
  beta <- beta - drop(step)
  if (max(abs(step)) < 1e-10) break
}
cat(sprintf("Newton-Raphson converged in %d iterations\n", iteration))

fit <- glm(reformulate(TRUE_PREDICTORS, "not_interested"), family = binomial, data = d)
comparison <- data.frame(thesis_value = THESIS_COEFFICIENTS,
                         newton_raphson = round(beta, 4),
                         glm = round(coef(fit)[names(THESIS_COEFFICIENTS)], 4))
print(comparison)
cat(sprintf("\nLargest difference between Newton-Raphson and glm(): %.2e\n",
            max(abs(beta - coef(fit)[colnames(X)]))))

# Reading a coefficient: the odds ratio for students who never liked primary maths.
b <- coef(fit)["like_maths_primaryNever"]
ci <- exp(confint.default(fit)["like_maths_primaryNever", ])
cat(sprintf("\nNever liked primary maths vs always: odds ratio %.2f (95%% CI %.2f to %.2f)\n", exp(b), ci[1], ci[2]))
cat(sprintf("Null deviance %.1f on %d df; residual deviance %.1f on %d df\n",
            fit$null.deviance, fit$df.null, fit$deviance, fit$df.residual))

cat("\nWhat this shows: logistic regression has no closed-form solution, so the coefficients are found\n",
    "iteratively. Newton-Raphson, using the score vector and Hessian, gives the same answer as glm(),\n",
    "and with 6,359 students the estimates sit close to the values that generated the data. As in the\n",
    "thesis, the largest effect is from attitude to maths at primary level.\n", sep = "")
