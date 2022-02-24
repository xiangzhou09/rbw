test_that("output has the correct structure", {
  # Point Treatment
  rbwPoint_fit <- rbwPoint(treat, baseline_x = c(log_TotalPop, PercentOver65, log_Inc,
                                             PercentHispanic, PercentBlack, density,
                                             per_collegegrads, CanCommute), data = advertisement)
  expect_output(str(rbwPoint_fit),
                "List of 4")
  # Mediation Analysis
  m1 <- lm(threatc ~ ally + trade + h1 + i1 + p1 + e1 + r1 +
             male + white + age + ed4 + democ, data = peace)
  m2 <- lm(cost ~ ally + trade + h1 + i1 + p1 + e1 + r1 +
             male + white + age + ed4 + democ, data = peace)
  m3 <- lm(successc ~ ally + trade + h1 + i1 + p1 + e1 + r1 +
             male + white + age + ed4 + democ, data = peace)
  zmodels <- list(m1, m2, m3)
  rbwMed_fit <- rbwMed(treatment = democ, mediator = immoral,
                       zmodels = list(m1, m2, m3), interact = TRUE,
                       baseline_x = c(ally, trade, h1, i1, p1, e1, r1, male, white, age, ed4),
                       data = peace)
  expect_output(str(rbwMed_fit),
                "List of 4")
  # Time-Varying Treatment
  m1 <- lm(dem.polls ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) * factor(week),
           data = campaign_long)
  m2 <- lm(undother ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) * factor(week),
           data = campaign_long)
  xmodels <- list(m1, m2)
  rbwPanel_fit <- rbwPanel(treatment = d.gone.neg, xmodels = xmodels, id = id,
                           time = week, data = campaign_long)
  expect_output(str(rbwPanel_fit),
                "List of 4")
})

