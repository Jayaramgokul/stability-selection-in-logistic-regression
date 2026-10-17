# Erratum check (2026): the scenario-1 lasso loop in the thesis code.
#
# In code/original/thesis_simulation_study_2020.R, line 157 sets
#     Lasso_Coeff_rd = c()
# once, before the 1,000-run loop that starts on line 158, instead of inside it.
# Predictors selected in earlier runs are therefore counted again in every later run.
# This script reruns that block twice on the original data: as written, and with the
# reset moved inside the loop. Everything else is kept as in the thesis code
# (bootstrap samples, responses simulated from the fitted model, cv.glmnet,
# lambda.1se, a predictor counted if any of its levels is selected).
#
# It needs the Growing Up in Ireland file used in the thesis (gui_sim_2.csv), which is
# available to approved researchers from the Irish Social Science Data Archive and is
# not included in this repository.
#
#   Rscript check_lasso_counter.R path/to/gui_sim_2.csv [runs]

suppressPackageStartupMessages(library(glmnet))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("usage: Rscript check_lasso_counter.R path/to/gui_sim_2.csv [runs]")
runs <- if (length(args) >= 2) as.integer(args[2]) else 150

gui_sim <- read.csv(args[1], header = TRUE, stringsAsFactors = TRUE)
maths_engagement <- gui_sim$bcce46a
levels(maths_engagement) <- c("Others", "Not interesting", "Others")     # as in the thesis code
gui_sim$maths_engagement <- maths_engagement
fit <- glm(maths_engagement ~ trans_diff_qrt + acce14 + acce16a + tposscale + tnegscale + bphc02a + bcce37 + bcce50,
           data = gui_sim, family = "binomial")
true_predictors <- c("trans_diff_qrt", "acce14", "tposscale", "tnegscale", "bphc02a", "bcce37", "acce16a", "bcce50")

lasso_counts <- function(n, reset) {
  set.seed(904849011)                                                    # the thesis seed
  selected_so_far <- c()
  counts <- integer(runs)
  for (r in seq_len(runs)) {
    if (reset) selected_so_far <- c()
    s <- gui_sim[sample(nrow(gui_sim), n, replace = TRUE), ]
    y <- rbinom(n, 1, plogis(predict(fit, newdata = s)))
    X <- model.matrix(~ trans_diff_qrt + acce14 + tposscale + tnegscale + bphc02a + bcce37 + acce16a + bcce50, s)
    cv <- cv.glmnet(X, y, family = "binomial", alpha = 1)
    b <- glmnet(X, y, family = "binomial", alpha = 1, lambda = cv$lambda.1se)$beta[, 1]
    columns <- names(b)[abs(b) > 0]
    for (p in true_predictors) selected_so_far <- c(selected_so_far, regmatches(columns, regexpr(p, columns)))
    counts[r] <- length(intersect(unique(selected_so_far), true_predictors))
  }
  tabulate(counts + 1, 9)
}

for (n in c(500, 5000)) {
  cat(sprintf("\nSample size %d, %d runs: number of runs selecting 0, 1, ..., 8 true predictors\n", n, runs))
  cat("  as written (reset outside the loop):", lasso_counts(n, reset = FALSE), "\n")
  cat("  reset inside the loop:              ", lasso_counts(n, reset = TRUE), "\n")
}
