# 1. Logistic regression

*Thesis section 1.3. Example: [`R/01_logistic_regression.R`](../R/01_logistic_regression.R).*

## What the thesis did

The response came from the Growing Up in Ireland study: whether a secondary
school student found maths **"Not interesting"** (1) or not (0). It was
modelled with logistic regression on eight categorical predictors chosen from
Smyth's (2017) study of the move from primary to secondary school:

| Predictor | Meaning | Levels |
|---|---|---|
| `trans_diff_qrt` | Transition difficulty (quartiles of a scale) | 4 |
| `acce14` | Attitude to primary school | 3 |
| `acce16a` | Attitude to maths at primary level | 3 |
| `tposscale` | Positive interaction with teachers | 4 |
| `tnegscale` | Negative interaction with teachers | 4 |
| `bphc02a` | Gender | 2 |
| `bcce37` | Year in secondary school | 2 |
| `bcce50` | Highest qualification the student hopes to attain | 4 |

With a set-to-zero constraint (one reference level per factor, Searle 1987),
the 24 indicator variables reduce to **18**. Together with the intercept they
form the design matrix for 6,359 students. The fitted coefficients were then
used as the "true" model in the simulation study (chapter 3). The largest
effect was attitude to maths at primary level.

## The idea

A probability must stay between 0 and 1, so it cannot be modelled as a
straight line in the predictors. Logistic regression models the **log-odds**
instead, which can take any value. This is the logit link of a generalised
linear model (Nelder & Wedderburn, 1972):

```
logit(p_i) = log( p_i / (1 - p_i) ) = β0 + β1 x_1i + ... + βk x_ki          (eq 1.5)
p_i        = exp(η_i) / (1 + exp(η_i))                                      (eq 1.6)
```

A coefficient is a change in log-odds. Its exponential is an odds ratio.

## Estimation

The coefficients maximise the log-likelihood (eq 1.12):

```
ℓ(β) = Σ [ y_i η_i − log(1 + exp(η_i)) ]        (plus a constant)
```

There is no closed-form solution, so the thesis describes **Newton-Raphson**
(Agresti, 2002). It starts from a guess and repeatedly moves along the score
vector (first derivatives, eq 1.14), scaled by the inverse Hessian (second
derivatives, eq 1.15), until the estimates stop changing.

## The example

The script simulates 6,359 students from the thesis coefficients and fits the
model in two ways: Newton-Raphson written out by hand, and R's `glm()`.

- **Newton-Raphson converges in 6 iterations** and agrees with `glm()` to within
  about 10⁻⁹.
- With this many students, the estimates sit close to the values that
  generated the data.
- **Odds ratio:** students who never liked primary maths have about 2.7 times
  the odds of finding secondary maths not interesting, compared with students
  who always liked it.

## In the code

The thesis model is the `fit = glm(...)` line near the top of
[`code/original/thesis_simulation_study_2020.R`](../code/original/thesis_simulation_study_2020.R).
In every simulation run, the code uses `predict(fit, newdata = ...)` and
`invlogit` to turn this model into simulated responses.
