# 04. Lasso for logistic regression (thesis section 2.3)
#
# Fits the L1-penalised logistic model (eq 2.10), chooses lambda by 10-fold cross
# validation as in the thesis, and draws the two plots the method is usually read
# from. Run from the R/ folder:  Rscript 04_lasso.R

source("simulate_data.R")
source("selection_methods.R")
set.seed(2020)

d <- simulate_students(5000)                       # section 2.4 cross-validates on a sample of 5,000
X <- lasso_design(d, TRUE_PREDICTORS)
cv <- cv.glmnet(X, d$not_interested, family = "binomial", alpha = 1, nfolds = 10)

for (rule in c("lambda.min", "lambda.1se")) {
  b <- coef(cv, s = rule)[-1, 1]
  chosen <- predictors_from_columns(names(b)[b != 0], TRUE_PREDICTORS)
  cat(sprintf("%-10s = %.4f: %2d non-zero indicator columns, %d of 8 predictors (%s)\n",
              rule, cv[[rule]], sum(b != 0), length(chosen), paste(chosen, collapse = ", ")))
}
cat("The thesis used lambda.1se: the largest lambda whose cross-validated deviance is within one\n",
    "standard error of the minimum.\n", sep = "")

png(output_path("04_lasso.png"), width = 1400, height = 600, res = 140)
par(mfrow = c(1, 2))
plot(cv, main = "")
title("Cross-validated deviance (cf. thesis Fig 2.1)", line = 2.5, cex.main = 0.9)
plot(cv$glmnet.fit, xvar = "lambda")
abline(v = log(c(cv$lambda.min, cv$lambda.1se)), lty = 3)
title("Coefficient paths", line = 2.5, cex.main = 0.9)
invisible(dev.off())
cat(sprintf("\nChart: %s\n", output_path("04_lasso.png")))

cat("\nWhat this shows: as lambda grows the penalty shrinks coefficients, and some reach exactly zero,\n",
    "so the lasso selects and estimates at once. Cross validation picks lambda; the 1-SE rule\n",
    "deliberately prefers a simpler model, so it keeps fewer predictors than lambda.min.\n", sep = "")
