# 05. Stability selection around the lasso (thesis section 2.4, pseudo-code 3.5)
#
# Follows the thesis steps on one sample of 500 students with true and noise predictors:
#   1. cross-validate to get the lambdas between lambda.min and lambda.1se;
#   2. draw a half-size subsample without replacement;
#   3. fit the lasso at each of those lambdas and note the predictors selected;
#   4. repeat 500 times to get each predictor's selection proportion at each lambda;
#   5. select a predictor if its highest proportion over the lambdas is at least 0.7.
# Run from the R/ folder:  Rscript 05_stability_selection.R

source("simulate_data.R")
source("selection_methods.R")
set.seed(2020)

candidates <- c(TRUE_PREDICTORS, NOISE_PREDICTORS)
d <- simulate_students(500)
paths <- stability_paths(d, candidates, regenerate_from_truth(), subsamples = 500)
best <- apply(paths$probability, 2, max)
at <- apply(paths$probability, 2, which.max)

cat(sprintf("%d lambdas between lambda.min (%.4f) and lambda.1se (%.4f)\n\n",
            length(paths$lambdas), min(paths$lambdas), max(paths$lambdas)))
cat(sprintf("%-24s %-6s %s\n", "predictor", "type", "highest selection proportion (at lambda)"))
for (p in names(sort(best, decreasing = TRUE))) {
  cat(sprintf("%-24s %-6s %.2f (%s)%s\n", p, ifelse(p %in% TRUE_PREDICTORS, "true", "noise"),
              best[p], rownames(paths$probability)[at[p]], ifelse(best[p] >= 0.7, "  selected", "")))
}
lowest <- which.min(paths$lambdas)
cat(sprintf("\nPredictors whose highest proportion occurs at the smallest lambda: %d of %d\n",
            sum(apply(paths$probability, 2, function(x) x[lowest] == max(x))), length(candidates)))

png(output_path("05_stability_paths.png"), width = 1000, height = 650, res = 140)
matplot(log(paths$lambdas), paths$probability, type = "l", lty = 1,
        col = ifelse(candidates %in% TRUE_PREDICTORS, "#3182bd", "#d8543b"),
        xlab = "log(lambda)", ylab = "Selection proportion", ylim = c(0, 1))
abline(h = 0.7, lty = 3)
legend("bottomleft", c("true predictor", "noise predictor", "threshold 0.7"),
       col = c("#3182bd", "#d8543b", "black"), lty = c(1, 1, 3), bty = "n", cex = 0.8)
invisible(dev.off())
cat(sprintf("Chart: %s\n", output_path("05_stability_paths.png")))

cat("\nWhat this shows: instead of trusting one lasso fit, stability selection asks how often each\n",
    "predictor survives across many half-samples. Proportions are highest at the smallest lambda,\n",
    "where the penalty is weakest, which is the pattern the thesis noted when it found the lowest\n",
    "lambda always carried the highest selection counts (section 5.2).\n", sep = "")
