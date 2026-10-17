# 06. The simulation study (thesis chapter 3)
#
# A compact version of the thesis design:
#   performance measures  how often each predictor is selected; how many true / noise predictors are selected
#   predictors            8 true; scenario 2 adds 6 noise predictors (3 related to true ones, 3 unrelated)
#   scenarios             1 true only; 2 true + noise predictors; 3 true + N(0, 4) noise in the linear predictor
#   sample size           500 (the thesis also ran 5,000)
#   methods               LRT, stepwise, lasso, stability selection around the lasso
#
# The thesis ran 1,000 runs per setting and 500 subsamples for stability selection.
# This example uses fewer (RUNS, SUBSAMPLES below) so it finishes in a few minutes.
# Writes output/simulation_runs.csv, which 08_comparing_methods.R analyses.
# Run from the R/ folder:  Rscript 06_simulation_study.R

source("simulate_data.R")
source("selection_methods.R")
set.seed(2020)

RUNS <- as.integer(Sys.getenv("RUNS", "40"))
SUBSAMPLES <- as.integer(Sys.getenv("SUBSAMPLES", "100"))
N <- 500

scenarios <- list(
  "1 true only"           = list(candidates = TRUE_PREDICTORS, noise_sd = 0),
  "2 true + noise"        = list(candidates = c(TRUE_PREDICTORS, NOISE_PREDICTORS), noise_sd = 0),
  "3 true + random noise" = list(candidates = TRUE_PREDICTORS, noise_sd = 4)
)

rows <- list()
for (sc in names(scenarios)) {
  cfg <- scenarios[[sc]]
  for (run in seq_len(RUNS)) {
    d <- simulate_students(N, noise_sd = cfg$noise_sd)            # steps 1-4 of the pseudo-code
    selected <- list(
      LRT       = select_lrt(d, cfg$candidates),
      Stepwise  = select_stepwise(d, cfg$candidates),
      Lasso     = select_lasso(d, cfg$candidates),
      Stability = select_stability(d, cfg$candidates, regenerate_from_truth(cfg$noise_sd), subsamples = SUBSAMPLES)
    )
    for (m in names(selected)) {
      rows[[length(rows) + 1]] <- data.frame(
        scenario = sc, run = run, method = m, predictor = cfg$candidates,
        type = ifelse(cfg$candidates %in% TRUE_PREDICTORS, "true", "noise"),
        selected = as.integer(cfg$candidates %in% selected[[m]]))
    }
  }
  cat(sprintf("Scenario %-22s done (%d runs)\n", sc, RUNS))
}
runs <- do.call(rbind, rows)
write.csv(runs, output_path("simulation_runs.csv"), row.names = FALSE)

cat("\nAverage number of true predictors selected (out of 8):\n")
true_counts <- aggregate(selected ~ scenario + method + run, data = subset(runs, type == "true"), FUN = sum)
print(round(xtabs(selected ~ scenario + method, aggregate(selected ~ scenario + method, true_counts, mean)), 2))
noise_counts <- aggregate(selected ~ method + run, data = subset(runs, type == "noise"), FUN = sum)
cat("\nAverage number of noise predictors selected in scenario 2 (out of 6):\n")
print(round(tapply(noise_counts$selected, noise_counts$method, mean), 2))
cat(sprintf("\nResults: %s\n", output_path("simulation_runs.csv")))

cat("\nWhat this shows: every method sees the same simulated samples, so differences between them come\n",
    "from the methods, not the data. Recording which predictors each method keeps, run after run,\n",
    "turns 'how good is this method?' into selection frequencies that can be compared. Chapter 4 of\n",
    "the thesis analyses those frequencies; 08_comparing_methods.R does the same here.\n", sep = "")
