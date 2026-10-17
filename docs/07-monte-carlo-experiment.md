# 7. The preliminary Monte Carlo experiment

*Thesis Appendix A. Example: [`R/07_monte_carlo_experiment.R`](../R/07_monte_carlo_experiment.R). Original code: [`code/original/appendix_a_monte_carlo_2020.R`](../code/original/appendix_a_monte_carlo_2020.R).*

## What the thesis did

Before the main study, a one-predictor experiment checked that simulating
from an assumed model, then refitting, behaves as expected.

1. **Fix a population model.** The probability of finding maths not
   interesting is 0.25 for girls and 0.18 for boys. Solving the logit
   equations (with `uniroot` in the original code) gives
   **β₀ = −1.0986** and **β₁ = −0.4177**.
2. **Simulate.** Draw a sample of 500 students, generate responses from the
   population model, and fit a logistic regression. Repeat 1,000 times.
3. **Summarise.** Average the estimates. Count how often β₁ is significant at
   5%: this is the **power** to detect the gender effect.

**Thesis results:**
- Average estimates: β₀ = −1.0920 and β₁ = −0.4337, close to the population
  values.
- Power: **50.1%**.

## The idea

The population values are known, so the experiment shows two things:
- **No bias:** the estimates are centred on the truth.
- **How often a real effect is missed:** at n = 500, a real difference
  between 25% and 18% is detected only about half the time.

That second point is what motivated the main study. If one true predictor is
missed half the time, a selection method working on many predictors can
easily miss some.

## The example

The script reproduces the experiment. The only difference is gender: the
original drew it from the survey sample, while here it is 50/50.

| | Thesis | This example |
|---|---|---|
| Average β₀ | −1.0920 | −1.0984 |
| Average β₁ | −0.4337 | −0.4289 |
| Power | 50.1% | 49.3% |

The [chart](../R/output/07_monte_carlo.png) shows the 1,000 estimates of each coefficient, with the population
value marked.
