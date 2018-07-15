# library(dplyr)
# library(tidyr)
#
# load("C:/Users/xiz225/Dropbox/Residual Balancing/campaign/dem.panel.RData")
#
# dem.panel2 <- dem.panel %>%
#   dplyr::select(state, demName, week, year, camp.length,
#                 dem.polls, dem.polls.l1, dem.polls.l2,
#                 undother, undother.l1, undother.l2,
#                 # d.gone.neg, d.gone.neg.l1, d.gone.neg.l2,
#                 neg.dem, neg.dem.l1, neg.dem.l2,
#                 neg.rep, neg.rep.l1, neg.rep.l2)
#
# campaign_long <- Blackwell %>%
#   mutate(demName = as.character(demName), week = time - 5,
#          year = 2000 + 2 * year.2002 + 4 * year.2004 + 6 * year.2006) %>%
#   dplyr::select(-year.2002, -year.2004, -year.2006, -d.neg.frac.l3, -time) %>%
#   left_join(dem.panel2, by = c("demName", "week", "year", "camp.length")) %>%
#   mutate(week = week + 5) %>%
#   filter(demName != "Brady")
#
# campaign <- campaign_long %>%
#   dplyr::select(setdiff(names(.), grep(".l2", names(.), value = TRUE))) %>%
#   mutate(id = paste0(demName, year)) %>%
#   mutate_at(c("d.gone.neg.l1", "dem.polls.l1", "undother.l1", "neg.rep.l1"),
#             function(x) ifelse(.[["week"]]==1, 1, x))
#
# use_data(campaign, overwrite = TRUE)
