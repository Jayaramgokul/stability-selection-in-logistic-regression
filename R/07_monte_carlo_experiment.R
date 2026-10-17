# 07. The preliminary Monte Carlo experiment (thesis Appendix A)
#
# Reproduces the thesis's first experiment, which used only assumed 'population' values:
#   P(not interested) = 0.25 for girls and 0.18 for boys
#   => beta0 = -1.0986 and beta1 = -0.4177
# Draw 1,000 samples of 500, fit a logistic regression to each, average the estimates,
# and count how often the gender effect is significant at 5% (the power).
# The thesis drew gender from the survey data; here it is 50/50.
# Run from the R/ folder:  Rscript 07_monte_carlo_experiment.R

set.seed(2020)

beta0 <- uniroot(function(b) plogis(b) - 0.25, c(-10, 0))$root            # as in the thesis code
beta1 <- uniroot(function(b) plogis(beta0 + b) - 0.18, c(-10, 10))$root
cat(sprintf("Population values: beta0 = %.4f, beta1 = %.4f (thesis: -1.0986, -0.4177)\n\n", beta0, beta1))

draws <- t(replicate(1000, {
  male <- rbinom(500, 1, 0.5)
  y <- rbinom(500, 1, plogis(beta0 + beta1 * male))
  fit <- summary(glm(y ~ male, family = binomial))$coefficients
  c(fit[1, 1], fit[2, 1], fit[2, 4])
}))
colnames(draws) <- c("beta0", "beta1", "p_value")

cat(sprintf("Monte Carlo average over 1,000 samples of 500: beta0 = %.4f, beta1 = %.4f\n",
            mean(draws[, "beta0"]), mean(draws[, "beta1"])))
cat(sprintf("Share of samples where beta1 is significant at 5%% (power): %.1f%%\n",
            100 * mean(draws[, "p_value"] < 0.05)))
cat("Thesis results:  beta0 = -1.0920, beta1 = -0.4337, power 50.1%\n")

png("output_07_monte_carlo.png", width = 1200, height = 500, res = 140)
par(mfrow = c(1, 2))
hist(draws[, "beta0"], breaks = 50, main = "Estimates of beta0", xlab = "beta0", col = "grey80", border = "white")
abline(v = beta0, col = "#d8543b", lwd = 2)
hist(draws[, "beta1"], breaks = 50, main = "Estimates of beta1", xlab = "beta1", col = "grey80", border = "white")
abline(v = beta1, col = "#d8543b", lwd = 2)
invisible(dev.off())
dir.create("output", showWarnings = FALSE)
invisible(file.rename("output_07_monte_carlo.png", "output/07_monte_carlo.png"))
cat("Chart: output/07_monte_carlo.png (red line = population value)\n")

cat("\nWhat this shows: averaged over many samples the estimates land on the population values, so the\n",
    "set-up works. But a real difference of 25% against 18% is detected only about half the time with\n",
    "500 students. That was the thesis's first sign that a sample can easily miss a true predictor,\n",
    "the problem the main simulation then studied.\n", sep = "")
