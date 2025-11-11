# Interpretation of Bayes Factors

Bayes Factors (BFs) are a fundamental tool in Bayesian analysis for
comparing two hypotheses: typically the **null hypothesis (H₀)** and the
**alternative hypothesis (H₁)**. The Bayes Factor in favor of the null
over the alternative is written as **BF₀₁**, which tells you how many
times more likely the observed data are under H₀ than under H₁. For
example, a BF₀₁ of 5 means the data are 5 times more likely under the
null hypothesis. A BF₀₁ of 0.2 (i.e., 1/5) means the data are 5 times
more likely under the alternative hypothesis.

Importantly, Bayes Factors can support **either** hypothesis:

- **BF₀₁ \> 1** → evidence favors **H₀** (null hypothesis)
- **BF₀₁ \< 1** → evidence favors **H₁** (alternative hypothesis)
- **BF₀₁ = 1** → evidence is balanced; neither hypothesis is preferred

This symmetry makes Bayes Factors more flexible than traditional
p-values, which can only reject or fail to reject H₀.

## Why Use the Natural Log of Bayes Factors?

Instead of reporting the raw BF₀₁ values, it is often better to report
their **natural logarithm**, written as **ln(BF₀₁)** or **log(BF₀₁)**
with base *e*. Here’s why:

### **Compresses Wide Ranges for Simpler Interpretation**

Bayes Factors can range from extremely small values (e.g. 0.001) to very
large ones (e.g. 1000), which makes them hard to compare directly.
Taking the natural log compresses this wide range:

- **BF₀₁ = 100 → ln(BF₀₁) ≈ 4.6**
- **BF₀₁ = 1 → ln(BF₀₁) = 0**
- **BF₀₁ = 0.01 → ln(BF₀₁) ≈ –4.6**

This compression makes values easier to report and visualize, especially
when plotting results or summarizing across multiple studies (e.g.,
meta-analysis).

### **Symmetrical Evidence Scale**

Using the natural log also gives a **symmetric scale** centered around
0:

- **ln(BF₀₁) \> 0** → evidence favors **H₀**
- **ln(BF₀₁) \< 0** → evidence favors **H₁**
- **ln(BF₀₁) = 0** → evidence is neutral

So, a log BF₀₁ of +2 means data support H₀ about as strongly as –2 would
support H₁. This makes interpretation intuitive: zero means balanced
evidence, and distance from zero indicates strength regardless of
direction.

### **Numerical Stability**

When computing Bayes Factors from likelihoods or marginal probabilities,
the raw numbers can be extremely large or small, which can lead to
numerical issues. Taking logs avoids underflow or overflow, since
products of small probabilities become sums of manageable
log-probabilities (e.g., **log(ab) = log(a) + log(b)**). This makes
Bayesian computation more robust in practice.

### **Additivity of Evidence**

When combining evidence from multiple sources or experiments, **log
Bayes Factors add**:

- ln(BF₀₁ from study 1) + ln(BF₀₁ from study 2) = ln(total BF₀₁)

This makes it easy to combine studies without recalculating everything
in raw terms. For example:

- Study 1: BF₀₁ = 3 → ln(BF₀₁) ≈ 1.1
- Study 2: BF₀₁ = 5 → ln(BF₀₁) ≈ 1.6
- Combined: ln(BF₀₁ total) ≈ 2.7 → BF₀₁ ≈ 15

This is equivalent to the data being 15 times more likely under H₀
across both studies.

## Interpreting Negative Log Bayes Factors

If you always report **log BF₀₁**, then:

- **Positive ln(BF₀₁)** → Evidence in favor of the null hypothesis
- **Negative ln(BF₀₁)** → Evidence in favor of the alternative
  hypothesis
- **Zero ln(BF₀₁)** → No evidence either way (BF₀₁ = 1)

**Examples:**

| BF₀₁ | ln(BF₀₁) | Interpretation                  |
|------|----------|---------------------------------|
| 100  | 4.61     | Strong evidence **for H₀**      |
| 10   | 2.30     | Moderate evidence **for H₀**    |
| 1    | 0        | No preference between H₀ and H₁ |
| 0.1  | –2.30    | Moderate evidence **for H₁**    |
| 0.01 | –4.61    | Strong evidence **for H₁**      |

The further the value is from 0, the stronger the evidence—positive
values favor the **null**, negative values favor the **alternative**.

## Summary

Using the **natural log of Bayes Factors (ln BF₀₁)** makes Bayesian
inference easier to interpret, communicate, and compute:

- **Compresses wide numeric range**
- **Symmetric scale centered at 0**
- **Numerically stable for computation**
- **Additive across studies**
- **Sign tells you which hypothesis is favored**; **magnitude tells you
  how strongly**

## References

- Jeffreys, H. (1961). *Theory of Probability* (3rd ed.). Oxford
  University Press.  
- Kass, R. E., & Raftery, A. E. (1995). Bayes Factors. *Journal of the
  American Statistical Association*, 90(430), 773–795.
  <https://doi.org/10.2307/2291091>  
- Lee, M. D., & Wagenmakers, E.-J. (2014). *Bayesian Cognitive Modeling:
  A Practical Course*. Cambridge University Press.  
- Morey, R. D., & Wagenmakers, E.-J. (2014). Simple Relation Between
  Bayesian Order-Restricted and Point-Null Hypothesis Tests. *Statistics
  & Probability Letters*, 92, 121–124.
  <https://doi.org/10.1016/j.spl.2014.05.010>  
- van Doorn, J., van den Bergh, D., Bohm, U., et al. (2021). The JASP
  Guidelines for Conducting and Reporting a Bayesian Analysis.
  *Psychonomic Bulletin & Review*, 28(3), 813–826.
  <https://doi.org/10.3758/s13423-020-01798-5>
