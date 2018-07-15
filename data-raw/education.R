# rm(list=ls(all=TRUE))
#
# library("Hmisc")
# library("readr")
# library("tidyr")
# library("dplyr")
# library("survey")
#
# tmp_fun <- function(x){ifelse(x %in% -seq(1, 5), NA, x)}
# tmp_fun_b <- pryr::compose(as.numeric, as.character)
# percentile_rank <- function(x){rank(x, na.last = "keep")/sum(!is.na(x))}
#
# Mode <- function(x) {
#   x <- x[complete.cases(x)]
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }
#
# wtd_rank <- function (x, weights = NULL, normwt = FALSE, na.rm = TRUE){
#   if (!length(weights))
#     return(rank(x, na.last = if (na.rm) NA else TRUE))
#   tab <- Hmisc::wtd.table(x, weights, normwt = normwt, na.rm = na.rm)
#   freqs <- tab$sum.of.weights
#   r <- cumsum(freqs) - 0.5 * (freqs - 1)
#   stats::approx(tab$x, r, xout = x, rule = 2)$y
# }
#
# load("C:/Users/Xiang/Dropbox/RWR/education-depression/nlsy79_ed.RData")
#
# nlsy79_brand <- haven::read_dta("C:/Users/Xiang/Dropbox/RWR/education-depression/edurose_mediation_20180319.dta") %>%
#   mutate(R0000100 = tmp_fun_b(R0000100))
#
# nlsy79_mental <- read_csv("C:/Users/Xiang/Dropbox/RWR/education-depression/mental.csv") %>%
#   `names<-`(c("id", "mental92", "mental94")) %>%
#   mutate_at(vars(mental92:mental94), tmp_fun)
#
# education <- left_join(nlsy79_brand, nlsy79_ed, by = c("R0000100" = "id")) %>%
#   left_join(nlsy79_mental, by = c("R0000100" = "id")) %>%
#   dplyr::select(id = R0000100, college = compcoll25, weights = weights, male,
#                 black = black.y, test_score:hispanic, urban:num_sibs,
#                 cesd92 = mental92, prmarr98, transitions98, tfinc_dest_b, cesd40) %>%
#   filter(complete.cases(.)) %>%
#   mutate(ses = wtd_rank(tfinc_dest_b, weights = weights, normwt=TRUE)/length(tfinc_dest_b)-0.5,
#          cesd40 = (cesd40 - weighted.mean(cesd40, weights))/sqrt(wtd.var(cesd40, weights))) %>%
#   mutate_at(vars(male:num_sibs), function(x) x - weighted.mean(x, .[["weights"]]))
#
# use_data(education, overwrite = TRUE)
