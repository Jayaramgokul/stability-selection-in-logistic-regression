# 9. Looking back: what the thesis left open, and what I would test now

*Written after the thesis, from later reading on model selection. The thesis,
its code and its results are unchanged; this is how I read them now. Nothing
here has been run as a new study. The last section lists what I would test.*

The thesis produced more questions than answers. Most of all: why was
stability selection so inclusive of noise predictors, and was that a property
of stability selection or of the methods underneath it? At the time I put it
down to multicollinearity and went no further. Reading the work of Yannick
Baraud and his co-authors on model and estimator selection, and going back to
the stability-selection literature itself, changed how I see four things.

## 1. What each method optimises is not what the thesis measured

The thesis scored every method on one question: **did it find the true
predictors?** None of the methods was built for exactly that question.

- **Stepwise with AIC:** AIC is built to choose the model that predicts best,
  in Kullback–Leibler terms, not to recover the true set of predictors.
- **The lasso:** its λ was chosen by cross-validated prediction error.
- **The LRT:** applied predictor by predictor at 5%, it tests each predictor
  separately, not the selected set as a whole.

Baraud (2019) makes a related point sharply. A selection rule can be good in
one sense and poor in another: being close to the truth in parameter
(Euclidean) distance need not mean being close in distribution. The general
lesson I take is that a study comparing methods should state what each method
optimises, and expect differences whenever that is not what the study scores.

## 2. AIC does not pay for the size of the candidate pool

Baraud, Giraud & Huet (2009) analyse classical criteria (FPE, AIC, BIC, AMDL)
in Gaussian regression with unknown variance:

- **When AIC works:** it behaves well when there is about one candidate model
  per size, as in nested models.
- **When it fails:** in *complete* variable selection, where any subset of the
  predictors is a candidate, the number of candidate models grows quickly with
  their size. AIC's fixed penalty of 2 per parameter does not account for
  this. In their words, AIC and FPE "should not be used for model selection
  purposes" with collections of that kind.
- **Their alternative:** the penalties they propose grow with the log of the
  pool size. Because the variance is estimated, they also adjust for the
  degrees of freedom left, n − |m|.

**For the thesis:**
- **The stepwise search was complete variable selection.** It ran over 14
  predictors (27 indicator columns once factor levels are expanded), so a
  pool-size penalty is the relevant correction.
- **The small-sample adjustment hardly matters.** At n = 500 or 5,000 with at
  most 27 columns, n − |m| is close to n, so only the pool-size part changes
  anything.
- **The theory is Gaussian.** It does not carry over to logistic regression
  directly. The closest logistic-ready analogue I know is the extended BIC,
  which adds a penalty for the number of candidate models of each size (Chen &
  Chen, 2008), and has a version for generalised linear models (Chen & Chen,
  2012).

## 3. Why stability selection was "overly inclusive"

Meinshausen & Bühlmann's error control (2010, Theorem 1) bounds the expected
number of falsely selected variables:

```
E(V) ≤ q² / ((2·π_thr − 1) · p)
```

- `q` is the average number of variables the lasso selects per subsample;
- `p` is the number of candidate variables;
- `π_thr` is the selection threshold.

It holds under two assumptions: the noise variables are **exchangeable**, and
the base procedure is no worse than random guessing.

The thesis setting breaks the bound in two ways:

- **The bound is too loose to protect anything.**
  - **Setting:** with the noise predictors added there are p = 27 columns: 18
    from the true predictors and 9 from the noise predictors (counted in this
    repository's simulated design, which mirrors the thesis's factor levels).
    The threshold was π_thr = 0.7.
  - **Even a lean selection is unprotected:** a lasso that kept only 8
    columns per subsample, one per true predictor, would give a bound of
    about 6 false selections out of 9 possible.
  - **The thesis's lasso kept far more:** at the smallest λ, which the thesis
    found always produced the highest selection counts, it keeps about 16
    columns per subsample in the simulated design. The bound becomes about
    23, more than the number of noise columns.
  - **Control and recovery cannot coexist here:** to guarantee at most 1
    false selection, the lasso could keep only about 3 columns per subsample,
    too few to find 8 predictors.

  In a problem this small, the guarantee and the goal pull against each other.
  Stability selection's error control is designed for many more candidates
  than selected variables (p much larger than q). The thesis's observation
  that the smallest λ "always wins" is the other face of the same fact: the
  smallest λ gives the largest q, and so the weakest control.
- **The noise was deliberately correlated with the signal.** Some noise
  predictors were chosen *because* they are related to true predictors, so
  the noise is not exchangeable, and Theorem 1 does not apply even in
  principle.

So my multicollinearity explanation was half right. Correlation does break
the assumption behind the guarantee. But even without it, the bound could not
have protected a selection large enough to find the true model.

Shah & Samworth (2013) offer an alternative: Complementary Pairs Stability
Selection, whose bounds need no exchangeability assumption. It is the version
I would use now.

## 4. Choosing λ, and the scenario with random noise

**Choosing λ.** The thesis left better ways of choosing λ as future work.
Baraud, Giraud & Huet (2014) address that question directly for Gaussian
regression. They select among an arbitrary collection of estimators, such as
lasso fits across a grid of λ, by estimated risk rather than by
cross-validation, and compare the two. Two caveats apply:
- their target is estimation risk, not finding the true predictors;
- the setting is Gaussian, not logistic.

**Scenario 3 (random noise).** Here N(0, 4²) noise was added to the linear
predictor, and every method lost the true predictors. Two separate things
happen in that scenario:
- **The signal weakens.** Averaging a logistic model over normal noise in the
  linear predictor shrinks the effective coefficients by roughly
  1/√(1 + 0.346·σ²) (Zeger, Liang & Albert, 1988). For σ = 4 that is to about
  40% of their size, which alone explains much of the loss.
- **The fitted model is misspecified.** The data no longer follow the
  logistic model the methods fit.

On the second point, ρ-estimation is relevant:
- **What it does:** it replaces the unbounded logarithm in the likelihood
  with a bounded comparison between candidate densities. The estimator stays
  reliable when the model is only approximately right (Baraud & Birgé,
  2018).
- **It now covers logistic regression:** Baraud & Chen (2020) extend it to
  regression in exponential families, including logistic regression.
- **What it does not do:** it is a remedy for misspecification and
  contamination, not for correlated predictors. It would not by itself
  separate a noise predictor from the true predictor it is correlated with.

## What I would test now

These are hypotheses, not results. Each could be tested by rerunning the
thesis simulation with one change.

| Change | Expected effect, if the reasoning above is right |
|---|---|
| Stepwise with extended BIC instead of AIC | Fewer noise predictors selected in scenario 2, at little cost to true ones at n = 5,000 |
| Stability selection with Λ chosen for a target E(V), and with complementary pairs | Fewer noise predictors, but also fewer true ones at n = 500: the trade-off made explicit |
| Scenario 2 with noise predictors independent of the true ones | Noise inclusion falls; the remaining excess is the q-versus-p effect, not correlation |
| Scenario 3 with the logistic-normal attenuation accounted for | Separates the weaker signal from the misspecification; only the latter is a case for a robust (ρ-) estimator |

## References

- Baraud, Y. (2019). Can we trust L2-criteria and L2-losses? Discussion of Arlot's survey. *Journal de la Société Française de Statistique* 160(3), 107–110. [Numdam](https://www.numdam.org/item/JSFS_2019__160_3_107_0/)
- Baraud, Y. & Birgé, L. (2018). Rho-estimators revisited: general theory and applications. *Annals of Statistics* 46(6B), 3767–3804. [arXiv:1605.05051](https://arxiv.org/abs/1605.05051)
- Baraud, Y. & Chen, J. (2020). Robust estimation of a regression function in exponential families. [arXiv:2011.01657](https://arxiv.org/abs/2011.01657)
- Baraud, Y., Giraud, C. & Huet, S. (2009). Gaussian model selection with an unknown variance. *Annals of Statistics* 37(2), 630–672. [arXiv:math/0701250](https://arxiv.org/abs/math/0701250)
- Baraud, Y., Giraud, C. & Huet, S. (2014). Estimator selection in the Gaussian setting. *Annales de l'IHP Probabilités et Statistiques* 50(3), 1092–1119. [arXiv:1007.2096](https://arxiv.org/abs/1007.2096)
- Chen, J. & Chen, Z. (2008). Extended Bayesian information criteria for model selection with large model spaces. *Biometrika* 95(3), 759–771.
- Chen, J. & Chen, Z. (2012). Extended BIC for small-n-large-P sparse GLM. *Statistica Sinica* 22, 555–574.
- Meinshausen, N. & Bühlmann, P. (2010). Stability selection. *JRSS B* 72(4), 417–473. [arXiv:0809.2932](https://arxiv.org/abs/0809.2932)
- Shah, R. D. & Samworth, R. J. (2013). Variable selection with error control: another look at stability selection. *JRSS B* 75(1), 55–80. [arXiv:1105.5578](https://arxiv.org/abs/1105.5578)
- Zeger, S. L., Liang, K.-Y. & Albert, P. S. (1988). Models for longitudinal data: a generalized estimating equation approach. *Biometrics* 44(4), 1049–1060.
