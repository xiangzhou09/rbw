
<!-- README.md is generated from README.Rmd. Please edit that file -->


# rbw: Residual Balancing Weights for Marginal Structural Models

Residual balancing is a method of constructing weights for marginal
structural models, which can be used to estimate marginal effects of
time-varying treatments and controlled direct/mediator effects in causal
mediation analysis. Compared with inverse probability-of-treatment
weights (IPW), residual balancing weights tend to be more robust and
more efficient, and are easier to use with continuous exposures. This
package provides two main functions, `rbwPanel()` and `rbwMed()`, that
produce residual balancing weights for analyzing time-varying treatments
and causal mediation, respectively.

**Reference**

  - Zhou, Xiang and Geoffrey T Wodtke. 2020. “[Residual Balancing: A
    Method of Constructing Weights for Marginal Structural
    Models](https://doi.org/10.1017/pan.2020.2)” Political Analysis.

## Installation

You can install the released version of rbw from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rbw")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("xiangzhou09/rbw")
```

## Estimating Marginal Effects of Time-varying Treatments

The `rbwPanel()` function constructs residual balancing weights for
estimating marginal effects of time-varying treatments. The following
example illustrates its use by estimating the effect of negative
campaign advertising (`d.gone.neg`) on election outcomes (`demprcnt`)
for 113 Democratic candidates in US Senate and Gubernatorial elections.

``` r
library(rbw)
# install.packages("survey")
library(survey)
#> Warning: package 'survey' was built under R version 4.0.5

# models for time-varying confounders
m1 <- lm(dem.polls ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) * factor(week), data = campaign_long)
m2 <- lm(undother ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) * factor(week), data = campaign_long)
xmodels <- list(m1, m2)

# residual balancing weights
rbwPanel_fit <- rbwPanel(treatment = d.gone.neg, xmodels = xmodels, id = id, time = week, data = campaign_long)
#> Entropy minimization converged within tolerance level

# merge weights into wide-format data
campaign_wide2 <- merge(campaign_wide, rbwPanel_fit$weights, by = "id")

# fit a marginal structural model (adjusting for baseline confounders)
rbw_design <- svydesign(ids = ~ 1, weights = ~ rbw, data = campaign_wide2)
msm_rbw <- svyglm(demprcnt ~ cum_neg * deminc + camp.length + factor(year) + office, design = rbw_design)
summary(msm_rbw)
#> 
#> Call:
#> svyglm(formula = demprcnt ~ cum_neg * deminc + camp.length + 
#>     factor(year) + office, design = rbw_design)
#> 
#> Survey design:
#> svydesign(ids = ~1, weights = ~rbw, data = campaign_wide2)
#> 
#> Coefficients:
#>                  Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)      50.39769    2.52269  19.978  < 2e-16 ***
#> cum_neg           0.96579    0.45496   2.123 0.036143 *  
#> deminc           17.04229    2.66426   6.397 4.62e-09 ***
#> camp.length      -0.09085    0.06175  -1.471 0.144222    
#> factor(year)2002 -5.57359    1.53081  -3.641 0.000425 ***
#> factor(year)2004 -6.22630    1.67340  -3.721 0.000322 ***
#> factor(year)2006 -1.51220    1.93697  -0.781 0.436751    
#> office            0.02811    1.11988   0.025 0.980026    
#> cum_neg:deminc   -2.99678    0.65932  -4.545 1.49e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for gaussian family taken to be 25.21453)
#> 
#> Number of Fisher Scoring iterations: 2
```

## Estimating Controlled Direct Effects (CDE)

In causal mediation analysis, the `rbwMed()` function can be used to
construct residual balancing weights for estimating the controlled
direct effect or the controlled mediator effect with a marginal
structural model. The following example illustrates its use by
estimating the controlled direct effect of shared democracy (`democ`) on
public support for war (`strike`) at different levels of perceived
morality of war (`immoral`) for a sample of respondents in a survey
experiment.

``` r
# models for post-treatment confounders
m1 <- lm(threatc ~ ally + trade + h1 + i1 + p1 + e1 + r1 +
  male + white + age + ed4 + democ, data = peace)

m2 <- lm(cost ~ ally + trade + h1 + i1 + p1 + e1 + r1 +
  male + white + age + ed4 + democ, data = peace)

m3 <- lm(successc ~ ally + trade + h1 + i1 + p1 + e1 + r1 +
  male + white + age + ed4 + democ, data = peace)

# residual balancing weights
rbwMed_fit <- rbwMed(treatment = democ, mediator = immoral,
  zmodels = list(m1, m2, m3), interact = TRUE,
  baseline_x = c(ally, trade, h1, i1, p1, e1, r1, male, white, age, ed4),
  data = peace)
#> Entropy minimization converged within tolerance level

# attach residual balancing weights to data
peace$rbw_cde <- rbwMed_fit$weights

# fit marginal structural model
rbw_design <- svydesign(ids = ~ 1, weights = ~ rbw_cde, data = peace)
msm_rbwMed <- svyglm(strike ~ democ * immoral, design = rbw_design)
summary(msm_rbwMed)
#> 
#> Call:
#> svyglm(formula = strike ~ democ * immoral, design = rbw_design)
#> 
#> Survey design:
#> svydesign(ids = ~1, weights = ~rbw_cde, data = peace)
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)    2.74428    0.06255  43.875  < 2e-16 ***
#> democ         -0.37399    0.09893  -3.780 0.000164 ***
#> immoral       -1.36569    0.15082  -9.055  < 2e-16 ***
#> democ:immoral  0.09091    0.19782   0.460 0.645899    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for gaussian family taken to be 1.384994)
#> 
#> Number of Fisher Scoring iterations: 2
```
