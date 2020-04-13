#' Long-format Data on Negative Campaign Advertising in US Senate and Gubernatorial Elections
#'
#' A dataset containing 17 variables and 565 unit-week records on the campaign of 113 Democratic candidates
#' in US Senate and Gubernatorial Elections from 2000 to 2006 (Blackwell 2013).
#'
#' @format A data frame with 565 rows and 17 columns: \describe{
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
#'   \item{id}{candidate id}
#'  }
#' @references Blackwell, Matthew. 2013. A Framework for Dynamic Causal Inference in
#'   Political Science. American Journal of Political Science 57(2): 504-619.
"campaign_long"

#' Wide-format Data on Negative Campaign Advertising in US Senate and Gubernatorial Elections
#'
#' A dataset containing 26 variables and 113 unit records from Blackwell (2013).
#'
#' @format A data frame with 565 rows and 26 columns: \describe{
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
#'   \item{d.gone.neg_1}{whether the candidate went negative in week 1}
#'   \item{d.gone.neg_2}{whether the candidate went negative in week 2}
#'   \item{d.gone.neg_3}{whether the candidate went negative in week 3}
#'   \item{d.gone.neg_4}{whether the candidate went negative in week 4}
#'   \item{d.gone.neg_5}{whether the candidate went negative in week 5}
#'   \item{dem.polls_1}{Democratic share in week 1 polls}
#'   \item{dem.polls_2}{Democratic share in week 2 polls}
#'   \item{dem.polls_3}{Democratic share in week 3 polls}
#'   \item{dem.polls_4}{Democratic share in week 4 polls}
#'   \item{dem.polls_5}{Democratic share in week 5 polls}
#'   \item{undother_1}{share of undecided voters in week 1 polls}
#'   \item{undother_2}{share of undecided voters in week 2 polls}
#'   \item{undother_3}{share of undecided voters in week 3 polls}
#'   \item{undother_4}{share of undecided voters in week 4 polls}
#'   \item{undother_5}{share of undecided voters in week 5 polls}
#'   \item{cum_neg}{the total number of campaign-weeks in which a candidate went negative}
#'   }
#' @references Blackwell, Matthew. 2013. A Framework for Dynamic Causal Inference in
#'   Political Science. American Journal of Political Science 57(2): 504-619.
"campaign_wide"

#' Data on Education Attainment and Mental Health in a Sample of US adults.
#'
#' A dataset containing 14 variables on education, socioeconomic status,
#' mental health, gender, race, and family background for 2,835 respondents from
#' the National Longitudinal Survey of Youth, 1979 (NLSY79)
#'
#' @format A data frame with 565 rows and 17 columns: \describe{
#'   \item{id}{respondent id}
#'   \item{weights}{NLSY79 sampling weight}
#'   \item{college}{whether the respondent obtained a Bachelor's degree by age 25}
#'   \item{ses}{socioeconomic status, measured by the percentile rank of the respondent's
#'   family income at age 40.}
#'   \item{cesd40}{depression score at age 40, measured by the Center for Epidemiologic Studies
#'   Depression Scale}
#'   \item{female}{a dummy variable indicating whether the respondent is female.}
#'   \item{race}{the race of the respondent, 1: Hispanic; 2: black; 3: other.}
#'   \item{momedu}{mother's education, measured by highest grade completed}
#'   \item{parinc}{the percentile rank of parent income in 1979-1983}
#'   \item{afqt3}{cognitive ability in 1981, measured by the Armed Forces Qualification Test}
#'   \item{educexp}{educational expectation in 1979}
#'   \item{cesd92}{depression score in 1992, measured by the Center for Epidemiologic Studies
#'   Depression Scale}
#'   \item{prmarr98}{proportion of time married between 1990 and 1998}
#'   \item{transitions98}{number of family transitions between 1990 and 1998}
#'   }
"education"
