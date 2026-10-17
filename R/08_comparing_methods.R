# 08. Comparing the methods (thesis chapter 4)
#
# Analyses output/simulation_runs.csv from 06_simulation_study.R the way the thesis did:
#   4.2  a logistic regression of 'selected' on method, predictor and scenario;
#   4.3  histograms of the number of true predictors selected, a Kruskal-Wallis test
#        across methods, and pairwise Wilcoxon tests with Holm correction.
# Run from the R/ folder after 06:  Rscript 08_comparing_methods.R

source("simulate_data.R")
path <- output_path("simulation_runs.csv")
if (!file.exists(path)) stop("Run 06_simulation_study.R first to create ", path)
runs <- read.csv(path)
runs$method <- factor(runs$method, levels = c("Stepwise", "LRT", "Lasso", "Stability"))

# 4.2: how method and predictor change the probability that a true predictor is selected (scenario 1).
s1 <- subset(runs, scenario == "1 true only" & type == "true")
meta <- glm(selected ~ method + predictor, family = binomial, data = s1)
cat("Scenario 1, logistic model of selection (log-odds relative to Stepwise):\n")
print(round(coef(summary(meta))[grep("^method", rownames(coef(summary(meta)))), c(1, 4)], 4))

# 4.3: distribution of the number of true predictors selected per run.
counts <- aggregate(selected ~ scenario + method + run, data = subset(runs, type == "true"), FUN = sum)
names(counts)[names(counts) == "selected"] <- "n_true"
for (sc in unique(counts$scenario)) {
  part <- subset(counts, scenario == sc)
  kw <- kruskal.test(n_true ~ method, data = part)
  pw <- suppressWarnings(pairwise.wilcox.test(part$n_true, part$method, p.adjust.method = "holm", exact = FALSE))
  cat(sprintf("\nScenario %s: median true predictors by method: %s\n", sc,
              paste(sprintf("%s %g", levels(part$method), tapply(part$n_true, part$method, median)), collapse = ", ")))
  cat(sprintf("  Kruskal-Wallis chi-squared = %.1f, p = %.2g\n", kw$statistic, kw$p.value))
  cat("  Pairwise Wilcoxon p-values (Holm-adjusted):\n")
  print(round(pw$p.value, 4))
}

png(output_path("08_true_predictors_selected.png"), width = 1400, height = 1000, res = 140)
part <- subset(counts, scenario == "2 true + noise")
par(mfrow = c(2, 2))
for (m in levels(part$method)) {
  share <- table(factor(part$n_true[part$method == m], levels = 0:8)) / sum(part$method == m)
  barplot(100 * share, main = paste(m, "method"), xlab = "No of true predictors selected",
          ylab = "% of runs", col = "grey30", border = NA)
}
invisible(dev.off())
cat(sprintf("\nChart (scenario 2, cf. thesis Fig 4.2): %s\n", output_path("08_true_predictors_selected.png")))

cat("\nWhat this shows: the counts of predictors selected are skewed, not normal, so the thesis used\n",
    "rank-based tests: Kruskal-Wallis to ask whether any method differs, then pairwise Wilcoxon tests\n",
    "with Holm correction to find which. The logistic model adds the other view: how much each method\n",
    "raises or lowers the odds of keeping a true predictor.\n", sep = "")
