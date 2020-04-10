
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

**Main Reference**

  - Zhou, Xiang and Geoffrey T Wodtke. 2020. “[Residual Balancing: A
    Method of Constructing Weights for Marginal Structural
    Models](https://doi.org/10.1017/pan.2020.2)” Political Analysis.

## Installation

You can install the development version of rbw from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("xiangzhou09/rbw")
```

## Estimating Marginal Effects of Time-varying Treatments

The `rbwpanel()` function constructs residual balancing weights for
estimating marginal effects of time-varying treatments. The following
example illustrates its use by estimating the effect of negative
campaign advertising (`d.gone.neg`) on election outcomes (`demprcnt`)
for 113 Democratic candidates in US Senate and Gubernatorial elections.

``` r
library(rbw)
# install.packages("survey")
library(survey)

# models for time-varying confounders
m1 <- lm(dem.polls ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) * factor(week), data = campaign_long)
m2 <- lm(undother ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) * factor(week), data = campaign_long)
xmodels <- list(m1, m2)

# residual balancing weights
rbwPanel_fit <- rbwPanel(exposure = d.gone.neg, xmodels = xmodels, id = id, time = week, data = campaign_long)
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
#> (Intercept)      49.18066    2.84225  17.303  < 2e-16 ***
#> cum_neg           0.98164    0.54222   1.810 0.073122 .  
#> deminc           16.23583    2.97437   5.459 3.28e-07 ***
#> camp.length      -0.05905    0.06775  -0.872 0.385451    
#> factor(year)2002 -5.48633    1.62291  -3.381 0.001020 ** 
#> factor(year)2004 -6.15855    1.72409  -3.572 0.000538 ***
#> factor(year)2006 -1.30567    2.11142  -0.618 0.537674    
#> office            0.60034    1.28520   0.467 0.641391    
#> cum_neg:deminc   -2.65044    0.77025  -3.441 0.000836 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for gaussian family taken to be 28.21637)
#> 
#> Number of Fisher Scoring iterations: 2
```

## Estimating Controlled Direct Effects (CDE)

In causal mediation analysis, the `rbwMed()` function can be used to
construct residual balancing weights for estimating the controlled
direct effect or the controlled mediator effect with a marginal
structural model. The following example illustrates its use by
estimating the controlled direct effect of college education (`college`)
on depression at age 40 (`cesd40`) at different levels of socioeconomic
status (`ses`) for a subsample of respondents in the National
Longitudinal Survey of Youth, 1979.

``` r
# models for post-treatment confounders
m1 <- lm(cesd92 ~ male + black + test_score + educ_exp +  father + hispanic + urban + educ_mom +
           num_sibs + college, weights = weights, data = education)
m2 <- lm(prmarr98 ~ male + black + test_score + educ_exp +  father + hispanic + urban + educ_mom +
           num_sibs + college, weights = weights, data = education)
m3 <- lm(transitions98 ~ male + black + test_score + educ_exp + father + hispanic +urban + educ_mom +
           num_sibs + college, weights = weights, data = education)

# residual balancing weights
rbwMed_fit <- rbwMed(treatment = college, mediator = ses, baseline_x = male:num_sibs,
                     zmodels = list(m1, m2, m3), base_weights = weights, data = education)
#> Entropy minimization converged within tolerance level

# attach residual balancing weights to data
education$rbw <- rbwMed_fit$weights

# fit a marginal structural model
rbw_design <- svydesign(ids = ~ 1, weights = ~ rbw, data = education)
msm_rbw <- svyglm(cesd40 ~ college * ses, design = rbw_design)
summary(msm_rbw)
#> 
#> Call:
#> svyglm(formula = cesd40 ~ college * ses, design = rbw_design)
#> 
#> Survey design:
#> svydesign(ids = ~1, weights = ~rbw, data = education)
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)   
#> (Intercept) -0.0007527  0.0275409  -0.027  0.97820   
#> college     -0.1055394  0.0810119  -1.303  0.19276   
#> ses         -0.3587749  0.1143384  -3.138  0.00172 **
#> college:ses  0.3939679  0.3662347   1.076  0.28214   
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for gaussian family taken to be 0.975618)
#> 
#> Number of Fisher Scoring iterations: 2
```
