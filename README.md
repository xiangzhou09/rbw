
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbw: Residual Balancing Weights for Marginal Structural Models

Residual balancing is a method of constructing weights for marginal
structural models, which can be used to estimate marginal effects of
time-varying treatments and controlled direct/mediator effects in causal
mediation analysis. Compared with inverse probability-of-treatment
weights (IPW), residual balancing weights tend to be more robust and
more efficient, and are easier to use with continuous exposures. This
package provides three main functions, `rbwPoint()`, `rbwPanel()` and
`rbwMed()`, that produce residual balancing weights for analyzing point
treatments, time-varying treatments, and causal mediation, respectively.

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

## Estimating the Average Effect of a Point Treatment

The `rbwPoint()` function constructs residual balancing weights for
estimating the average effect of a point treatment. The following
example illustrates its use by estimating the average effect of
televised political advertisements (`treat`) on campaign contributions
(`Cont`) among 16,265 zipcodes in the 2004 and 2008 US presidential
elections.

``` r
library(rbw)
# install.packages("survey")
library(survey)

# residual balancing weights
rbwPoint_fit <- rbwPoint(treat, baseline_x = c(log_TotalPop, PercentOver65, log_Inc, PercentHispanic, PercentBlack, density, per_collegegrads, CanCommute), data = advertisement)
#> Entropy minimization converged within tolerance level

# attach residual balancing weights to data
advertisement$rbw_point <- rbwPoint_fit$weights

# fit marginal structural model
rbw_design <- svydesign(ids = ~ 1, weights = ~ rbw_point, data = advertisement)

# the outcome model includes the treatment, the square of the treatment,
# and state-level fixed effects (Fong, Hazlett, and Imai 2018)
msm_rbwPoint <- svyglm(Cont ~ treat + I(treat^2) + factor(StFIPS), design = rbw_design)
summary(msm_rbwPoint)
#> 
#> Call:
#> svyglm(formula = Cont ~ treat + I(treat^2) + factor(StFIPS), 
#>     design = rbw_design)
#> 
#> Survey design:
#> svydesign(ids = ~1, weights = ~rbw_point, data = advertisement)
#> 
#> Coefficients:
#>                  Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)       3.57549    2.79045   1.281 0.200095    
#> treat             0.43986    1.70909   0.257 0.796901    
#> I(treat^2)        0.01552    0.16555   0.094 0.925332    
#> factor(StFIPS)5  -1.25821    1.07914  -1.166 0.243656    
#> factor(StFIPS)6  64.54555    5.42773  11.892  < 2e-16 ***
#> factor(StFIPS)10 12.25245    6.06566   2.020 0.043403 *  
#> factor(StFIPS)13 11.14059    2.98022   3.738 0.000186 ***
#> factor(StFIPS)17 20.98707    4.37924   4.792 1.66e-06 ***
#> factor(StFIPS)20 -1.89078    1.17473  -1.610 0.107516    
#> factor(StFIPS)21 -1.75188    1.21534  -1.441 0.149469    
#> factor(StFIPS)23 -2.07515    1.60297  -1.295 0.195489    
#> factor(StFIPS)24 36.79553    8.26927   4.450 8.66e-06 ***
#> factor(StFIPS)25 48.39716    7.34165   6.592 4.47e-11 ***
#> factor(StFIPS)27  2.31899    2.11116   1.098 0.272027    
#> factor(StFIPS)28 -0.11943    1.25105  -0.095 0.923948    
#> factor(StFIPS)30 -4.49525    1.58284  -2.840 0.004517 ** 
#> factor(StFIPS)31 -3.16796    1.00206  -3.161 0.001573 ** 
#> factor(StFIPS)34 23.32090    4.04985   5.758 8.64e-09 ***
#> factor(StFIPS)36 29.47346    4.29735   6.859 7.21e-12 ***
#> factor(StFIPS)40  0.58593    1.16360   0.504 0.614588    
#> factor(StFIPS)45  1.14183    1.46973   0.777 0.437230    
#> factor(StFIPS)46 -4.75496    1.99265  -2.386 0.017033 *  
#> factor(StFIPS)47  5.66276    1.84947   3.062 0.002203 ** 
#> factor(StFIPS)48 18.32801    2.37261   7.725 1.19e-14 ***
#> factor(StFIPS)50 -0.50451    1.93071  -0.261 0.793860    
#> factor(StFIPS)56  2.17016    3.10951   0.698 0.485244    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for gaussian family taken to be 13738.91)
#> 
#> Number of Fisher Scoring iterations: 2
```

## Estimating Marginal Effects of Time-varying Treatments

The `rbwPanel()` function constructs residual balancing weights for
estimating marginal effects of time-varying treatments. The following
example illustrates its use by estimating the effect of negative
campaign advertising (`d.gone.neg`) on election outcomes (`demprcnt`)
for 113 Democratic candidates in US Senate and Gubernatorial elections.

``` r

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
