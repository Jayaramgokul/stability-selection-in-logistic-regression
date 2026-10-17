# 8. Comparing the methods

*Thesis chapter 4. Example: [`R/08_comparing_methods.R`](../R/08_comparing_methods.R). Thesis figures: [4.1](../figures/thesis/fig4_1_true_predictors_only.png), [4.2](../figures/thesis/fig4_2_true_with_noise_predictors.png), [4.3](../figures/thesis/fig4_3_noise_predictors_selected.png), [4.4](../figures/thesis/fig4_4_true_with_random_noise.png).*

## What the thesis did

The simulation produced two kinds of result, and the thesis analysed each
differently.

**1. Selection frequency of each predictor (section 4.2).** A logistic
regression modelled "was this predictor selected?" on three factors:
- the **method** (4 levels);
- the **predictor** (8 true, or 6 noise);
- the **sample size** (500 or 5,000).

Its coefficients show how much each design choice raises or lowers the odds
of selection. Separate models were fitted for each scenario.

**2. Selection frequency of the number of predictors (section 4.3).**
- **Histograms** showed how many true (and noise) predictors each method
  selected per run.
- **Kruskal–Wallis test:** the counts were skewed, so ANOVA's normality
  assumption did not hold, and the thesis used this rank-based alternative
  (Kruskal & Wallis, 1952) to ask whether any method differs.
- **Pairwise Wilcoxon tests:** these then compared methods two at a time,
  with **Holm correction** for the multiple comparisons. The code uses
  `pairwise.wilcox.test(..., p.adj = "holm")`.

## Findings reported in the thesis

- **Noise predictors (scenario 2):** stability selection kept the most true
  predictors and also let in the most noise predictors. The thesis describes
  this as its "overly inclusive" nature.
- **Random noise (scenario 3):** every method lost the true predictors much
  more often. Stability selection was almost equally likely to select six
  predictors or none.
- **Sample size:** a sample of 5,000 increased selection for every method, in
  every scenario.
- **Scenario 1 (true predictors only):** the thesis reported the lasso as the
  best method, with stability selection failing to improve on it. **This
  result is affected by the erratum below.**

## The example

The script reads the runs from [06](06-simulation-design.md) and repeats both
analyses:
- the logistic model of selection in scenario 1;
- for each scenario, the median number of true predictors per method, the
  Kruskal–Wallis test and the Holm-adjusted pairwise Wilcoxon p-values;
- [histograms](../R/output/08_true_predictors_selected.png) in the style of the thesis figures.

In the example run, the Kruskal–Wallis test finds differences between methods
in all three scenarios (p < 10⁻⁸).

## Erratum

Fig 4.1's lasso panel, and the scenario-1 statements about the lasso in
sections 4.2, 4.3(a) and 5.1, come from a loop whose list of selected
predictors was not reset between runs. Corrected, the lasso at `lambda.1se`
selects few or no predictors at n = 500. Stability selection therefore did
improve on the lasso in scenario 1. This resolves the "counterintuitive"
result the thesis flagged for future work in section 5.2. Details are in the
[README](../README.md#erratum).
