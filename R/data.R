#' Long-format Data on Negative Campaign Advertising in US Senate and Gubernatorial Elections
#'
#' A dataset containing 19 variables and 565 unit-week records on the campaign of 113 Democratic candidates
#' in US Senate and Gubernatorial Elections from 2000 to 2006 (Blackwell 2013).
#'
#' @format A data frame with 565 rows and 19 columns: \describe{
#'   \item{demName}{name of the Democratic candidate}
#'   \item{d.gone.neg}{whether the candidate went negative in a campaign-week,
#'    defined as whether more than 10\% of the candidate's political advertising was negative}
#'   \item{d.gone.neg.l1}{whether the candidate went negative in the previous campaign-week}
#'   \item{camp.length}{length of the candidate's campaign (in weeks)}
#'   \item{deminc}{whether the candidate was an incumbent}
#'   \item{base.poll}{Democratic share in the baseline polls}
#'   \item{base.und}{share of undecided voters in the baseline polls}
#'   \item{office}{type of office in contest. 0: governor; 1: senator}
#'   \item{demprcnt}{Democratic share of the two-party vote in the election}
#'   \item{week}{week in the campaign (in the final five weeks preceding the election)}
#'   \item{year}{year of the election}
#'   \item{state}{state of the election}
#'   \item{dem.polls}{Democratic share in the polls}
#'   \item{dem.polls.l1}{Democratic share in the polls in the previous campaign-week}
#'   \item{undother}{share of undecided voters in the polls}
#'   \item{undother.l1}{share of undecided voters in the polls in the previous campaign-week}
#'   \item{neg.dem}{the proportion of advertisements that were negative in a campaign-week}
#'   \item{neg.dem.l1}{the proportion of advertisements that were negative in the previous campaign-week}
#'   \item{id}{candidate id}
#'  }
#' @references Blackwell, Matthew. 2013. A Framework for Dynamic Causal Inference in
#'   Political Science. American Journal of Political Science 57(2): 504-619.
"campaign_long"

#' Wide-format Data on Negative Campaign Advertising in US Senate and Gubernatorial Elections
#'
#' A dataset containing 32 variables and 113 unit records from Blackwell (2013).
#'
#' @format A data frame with 113 rows and 26 columns: \describe{
#'   \item{demName}{name of the Democratic candidate}
#'   \item{camp.length}{length of the candidate's campaign (in weeks)}
#'   \item{deminc}{whether the candidate was an incumbent.}
#'   \item{base.poll}{Democratic share in the baseline polls}
#'   \item{base.und}{share of undecided voters in the baseline polls}
#'   \item{office}{type of office in contest. 0: governor; 1: senator}
#'   \item{demprcnt}{Democratic share of the two-party vote in the election}
#'   \item{year}{year of the election}
#'   \item{state}{state of the election}
#'   \item{id}{candidate id}
#'   \item{dem.polls_1}{Democratic share in week 1 polls}
#'   \item{dem.polls_2}{Democratic share in week 2 polls}
#'   \item{dem.polls_3}{Democratic share in week 3 polls}
#'   \item{dem.polls_4}{Democratic share in week 4 polls}
#'   \item{dem.polls_5}{Democratic share in week 5 polls}
#'   \item{d.gone.neg_1}{whether the candidate went negative in week 1}
#'   \item{d.gone.neg_2}{whether the candidate went negative in week 2}
#'   \item{d.gone.neg_3}{whether the candidate went negative in week 3}
#'   \item{d.gone.neg_4}{whether the candidate went negative in week 4}
#'   \item{d.gone.neg_5}{whether the candidate went negative in week 5}
#'   \item{neg.dem_1}{the proportion of advertisements that were negative in week 1 polls}
#'   \item{neg.dem_2}{the proportion of advertisements that were negative in week 2 polls}
#'   \item{neg.dem_3}{the proportion of advertisements that were negative in week 3 polls}
#'   \item{neg.dem_4}{the proportion of advertisements that were negative in week 4 polls}
#'   \item{neg.dem_5}{the proportion of advertisements that were negative in week 5 polls}
#'   \item{undother_1}{share of undecided voters in week 1 polls}
#'   \item{undother_2}{share of undecided voters in week 2 polls}
#'   \item{undother_3}{share of undecided voters in week 3 polls}
#'   \item{undother_4}{share of undecided voters in week 4 polls}
#'   \item{undother_5}{share of undecided voters in week 5 polls}
#'   \item{cum_neg}{the total number of campaign-weeks in which a candidate went negative}
#'   \item{ave_neg}{the average proportion of advertisements that were negative over the final five weeks of the campaign multiplied by ten}
#'   }
#' @references Blackwell, Matthew. 2013. A Framework for Dynamic Causal Inference in
#'   Political Science. American Journal of Political Science 57(2): 504-619.
"campaign_wide"

#' Data on Political Advertisement and Campaign Contributions in US Presidential Elections
#'
#' A dataset containing 15 variables on the campaign contributions of 16,265 zip codes to the
#' 2004 and 2008 US presidential elections in addition to the demographic characteristics of each area
#' (Urban and Niebler 2014; Fong, Hazlett, and Imai 2018).
#'
#' @format A data frame with 16,265 rows and 15 columns: \describe{
#'   \item{zip}{zip code}
#'   \item{treat}{the log transformed TotAds}
#'   \item{TotAds}{the total number of political advertisements aired in the zip code}
#'   \item{TotalPop}{population size}
#'   \item{PercentOver65}{percent of the population over 65}
#'   \item{Inc}{median household income}
#'   \item{PercentHispanic}{percent Hispanic}
#'   \item{PercentBlack}{percent black}
#'   \item{density}{population density (people per sq mile)}
#'   \item{per_collegegrads}{percent college graduates}
#'   \item{CanCommute}{a dummy variable indicating whether it is possible to commute to the zip code from a competitive state}
#'   \item{StFIPS}{state FIPS code}
#'   \item{Cont}{campaign contributions (in thousands of dollars)}
#'   \item{log_TotalPop}{log population}
#'   \item{log_Inc}{log median income}
#'   }
#' @references Fong, Christian, Chad Hazlett, and Kosuke Imai. 2018. Covariate Balancing Propensity Score for a Continuous
#'   Treatment: Application to The Efficacy of Political Advertisements. The Annals of Applied Statistics 12(1):156-77.
#'
#' Urban, Carly, and Sarah Niebler. 2014. Dollars on the Sidewalk: Should U.S. Presidential Candidates Advertise in Uncontested States?
#'   American Journal of Political Science 58(2):322-36.
#'
"advertisement"

#' Data on Public Support for War in a Sample of US Respondents
#'
#' A dataset containing 17 variables on the views of 1,273 US adults about their support
#' for war against countries that were hypothetically developing nuclear weapons. The data include
#' several variables on the country's features and respondents' demographic and attitudinal characteristics
#' (Tomz and Weeks 2013; Zhou and Wodtke 2020).
#'
#' @format A data frame with 1,273 rows and 17 columns: \describe{
#'   \item{threatc}{number of adverse events respondents considered probable if the US did not engage in war}
#'   \item{ally}{a dummy variable indicating whether the country had signed a military alliance with the US}
#'   \item{trade}{a dummy variable indicating whether the country had high levels of trade with the US}
#'   \item{h1}{an index measuring respondent's attitude toward militarism}
#'   \item{i1}{an index measuring respondent's attitude toward internationalism}
#'   \item{p1}{an index measuring respondent's identification with the Republican party}
#'   \item{e1}{an index measuring respondent's attitude toward ethnocentrism}
#'   \item{r1}{an index measuring respondent's attitude toward religiosity}
#'   \item{male}{a dummy variable indicating whether the respondent is male}
#'   \item{white}{a dummy variable indicating whether the respondent is white}
#'   \item{age}{respondent's age}
#'   \item{ed4}{respondent's education with categories ranging from high school or less to postgraduate degree}
#'   \item{democ}{a dummy variable indicating whether the country was a democracy}
#'   \item{strike}{a measure of support for war on a five-point scale}
#'   \item{cost}{number of negative consequences anticipated if the US engaged in war}
#'   \item{successc}{whether the respondent thought the operation would succeed. 0: less than 50-50 chance of working even in the short run; 1: efficacious only in the short run; 2: successful both in the short and long run}
#'   \item{immoral}{a dummy variable indicating whether respondents thought it would be morally wrong to strike the country}
#'   }
#' @references Tomz, Michael R., and Jessica L. P. Weeks. 2013. Public Opinion and the Democratic Peace.
#'   The American Political Science Review 107(4):849-65.
#'
#' Zhou, Xiang, and Geoffrey T. Wodtke. 2020. Residual Balancing:
#'   A Method of Constructing Weights for Marginal Structural Models. Political Analysis 28(4):487-506.
#'
#'
"peace"

