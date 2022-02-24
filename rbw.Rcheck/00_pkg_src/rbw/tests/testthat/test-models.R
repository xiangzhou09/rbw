test_that("confounders and models are supplied", {
  # Point Treatment
  expect_error(rbwPoint(treat, data = advertisement),
               "Baseline confounders are not provided")
  # Mediation Analysis - zmodels not provided and user doesn't set it to NULL
  m1 <- lm(threatc ~ ally + trade + h1 + i1 + p1 + e1 + r1 +
             male + white + age + ed4 + democ, data = peace)
  m2 <- lm(cost ~ ally + trade + h1 + i1 + p1 + e1 + r1 +
             male + white + age + ed4 + democ, data = peace)
  m3 <- lm(successc ~ ally + trade + h1 + i1 + p1 + e1 + r1 +
             male + white + age + ed4 + democ, data = peace)
  zmodels <- list(m1, m2, m3)
  expect_error(rbwMed(treatment = democ, mediator = immoral, interact = TRUE,
                      baseline_x = c(ally, trade, h1, i1, p1, e1, r1, male, white, age, ed4),
                      data = peace),
               "'zmodels' must be provided")
  # Mediation Analysis - zmodels set to NULL but user doesn't supply any baseline confounders
  expect_error(rbwMed(treatment = democ, mediator = immoral, interact = TRUE,
                      zmodels = NULL,
                      data = peace),
               "Neither 'zmodels' nor baseline confounders are provided")
  # Time-Varying Treatment
  m1 <- lm(dem.polls ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) * factor(week),
           data = campaign_long)
  m2 <- lm(undother ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) * factor(week),
           data = campaign_long)
  xmodels <- list(m1, m2)
  expect_error(rbwPanel(treatment = d.gone.neg, id = id,
                        time = week, data = campaign_long),
               "'xmodels' must be provided")
})


