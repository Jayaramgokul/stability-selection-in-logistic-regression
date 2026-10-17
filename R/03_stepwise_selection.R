# 03. Stepwise selection with AIC (thesis section 2.2)
#
# Runs the thesis's stepwise procedure: start from the intercept-only model, add or
# remove one predictor at a time while AIC falls (eq 2.2). Prints each step.
# Run from the R/ folder:  Rscript 03_stepwise_selection.R

source("simulate_data.R")
set.seed(2020)

d <- simulate_students(500)
null <- glm(not_interested ~ 1, family = binomial, data = d)
chosen <- step(null, scope = list(lower = ~1, upper = reformulate(TRUE_PREDICTORS)), trace = 0)

cat("Steps taken (AIC after each step):\n")
path <- chosen$anova
for (i in seq_len(nrow(path))) cat(sprintf("  %-28s AIC %.2f\n", ifelse(path$Step[i] == "", "(intercept only)", path$Step[i]), path$AIC[i]))

final <- attr(terms(chosen), "term.labels")
k <- length(coef(chosen))
cat(sprintf("\nAIC by hand: -2 log-likelihood + 2 x %d parameters = %.2f (step() reports %.2f)\n",
            k, -2 * as.numeric(logLik(chosen)) + 2 * k, AIC(chosen)))
cat(sprintf("Selected %d of 8 true predictors: %s\n", length(final), paste(final, collapse = ", ")))

cat("\nWhat this shows: stepwise selection follows a single path through the candidate models, adding\n",
    "or removing whichever predictor lowers AIC most. The 2 x parameters penalty means a factor with\n",
    "several levels must improve the fit by more to be added. Which predictors are chosen depends on\n",
    "the sample, which is the instability the thesis set out to measure.\n", sep = "")
