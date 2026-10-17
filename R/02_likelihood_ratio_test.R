# 02. Likelihood ratio test (thesis section 2.1)
#
# Computes the LRT for one predictor by hand from two deviances (eq 2.1), checks it
# against drop1(test = "LRT"), then uses it as the thesis did to select predictors.
# Run from the R/ folder:  Rscript 02_likelihood_ratio_test.R

source("simulate_data.R")
source("selection_methods.R")
set.seed(2020)

d <- simulate_students(500)
full <- glm(reformulate(TRUE_PREDICTORS, "not_interested"), family = binomial, data = d)
reduced <- update(full, . ~ . - teacher_negative)

lrt <- deviance(reduced) - deviance(full)                       # -2 ln(L_reduced) + 2 ln(L_full)
df <- df.residual(reduced) - df.residual(full)                  # 3 indicator columns removed
cat(sprintf("Dropping teacher_negative: LRT = %.2f on %d df, p = %.4f\n", lrt, df, pchisq(lrt, df, lower.tail = FALSE)))

tests <- drop1(full, test = "LRT")
cat(sprintf("drop1() gives:              LRT = %.2f on %d df, p = %.4f\n\n",
            tests["teacher_negative", "LRT"], tests["teacher_negative", "Df"], tests["teacher_negative", "Pr(>Chi)"]))

print(tests[-1, c("Df", "LRT", "Pr(>Chi)")])
kept <- select_lrt(d, TRUE_PREDICTORS)
cat(sprintf("\nKept at the 5%% level: %d of 8 true predictors (%s)\n", length(kept), paste(kept, collapse = ", ")))

cat("\nWhat this shows: the LRT compares the full model with the model missing one predictor. Each\n",
    "factor is tested as a whole, on as many degrees of freedom as it has indicator columns. Used as\n",
    "a selection rule on a sample of 500, it keeps only the predictors whose effect is strong enough\n",
    "to be detected, even though all eight are in the true model.\n", sep = "")
