test_that("data is not empty", {
  # Point Treatment
  expect_error(rbwPoint(treatment = treat,
                      baseline_x = c(log_TotalPop, PercentOver65, log_Inc,PercentHispanic, PercentBlack, density,per_collegegrads, CanCommute)),
               "'data' must be provided")
  # Mediation Analysis
  m1 <- lm(threatc ~ ally + trade + h1 + i1 + p1 + e1 + r1 +
             male + white + age + ed4 + democ, data = peace)
  m2 <- lm(cost ~ ally + trade + h1 + i1 + p1 + e1 + r1 +
             male + white + age + ed4 + democ, data = peace)
  m3 <- lm(successc ~ ally + trade + h1 + i1 + p1 + e1 + r1 +
             male + white + age + ed4 + democ, data = peace)
  zmodels <- list(m1, m2, m3)
  expect_error(rbwMed(treatment = democ, mediator = immoral,
                      zmodels = zmodels, interact = TRUE,
                      baseline_x = c(ally, trade, h1, i1, p1, e1, r1, male, white, age, ed4)),
               "'data' must be provided")
  # Time-Varying Treatment
  m1 <- lm(dem.polls ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) * factor(week),
           data = campaign_long)
  m2 <- lm(undother ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) * factor(week),
           data = campaign_long)
  xmodels <- list(m1, m2)
  expect_error(rbwPanel(treatment = d.gone.neg, xmodels = xmodels, id = id,
                        time = week),
               "'data' must be provided")
})
