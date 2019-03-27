#' Residual Balancing Weights for Analyzing Time-varying Treatments
#'
#' \code{rbwPanel} is a function that produces residual balancing weights for
#' estimating the marginal effects of time-varying treatments. The user supplies
#' a long format data frame (each row being a unit-period) and a list of
#' fitted model objects for time-varying confounders.
#'
#' @param exposure Expression for the exposure variable.
#' @param xmodels A list of fitted \code{lm} or \code{glm} objects for
#'   time-varying confounders.
#' @param id Expression for the unit id variable.
#' @param time Expression for the time variable.
#' @param base_weights Expression for base weights (optional).
#' @param data A data frame containing the variables in the model.
#' @inheritParams eb2
#'
#' @return A list containing the results.
#'  \item{weights}{A data frame containing id and residual balancing weights.}
#'  \item{constraints}{A matrix of (linearly independent) residual balancing constraints}
#'  \item{eb_out}{Results from calling \code{\link{eb2}} function}
#'  \item{call}{The matched call.}
#' @export
#'
#' @examples
#'
#' # models for time-varying confounders
#'
#' m1 <- lm(dem.polls ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) * factor(week),
#' data = campaign_long)
#' m2 <- lm(undother ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) * factor(week),
#' data = campaign_long)
#'
#' xmodels <- list(m1, m2)
#'
#' # residual balancing weights
#' fit <- rbwPanel(exposure = d.gone.neg, xmodels = xmodels, id = id,
#' time = week, data = campaign_long)
#'
#' summary(fit$weights)
#'
#' # merge weights into wide-format data
#' campaign_wide2 <- merge(campaign_wide, fit$weights, by = "id")
#'
#' # fit marginal structural models
#' rbw_design <- survey::svydesign(ids = ~ 1, weights = ~ rbw, data = campaign_wide2)
#'
#' msm_rbw <- survey::svyglm(demprcnt ~ cum_neg * deminc + camp.length + factor(year) + office,
#'  design = rbw_design)

rbwPanel <- function(exposure, xmodels, id, time, base_weights, data,
                  max_iter = 500, print_level = 1, tol = 1e-3) {

    # match call
    cl <- match.call()

    # check missing arguments
    if(missing(exposure)) stop("exposure must be provided.")
    if(missing(xmodels)) stop("xmodels must be provided.")
    if(missing(id)) stop("id must be provided.")
    if(missing(time)) stop("time must be provided.")
    if(missing(data)) stop("data must be provided.")

    # check xmodels and data type
    if(!is.list(xmodels)) stop("xmodels must be a list.")
    if(!all(unlist(lapply(xmodels, inherits, "lm")))){
      stop("Each element of xmodels must be an object of class `lm`")
    }
    if(!is.data.frame(data)) stop("data must be a data.frame.")
    n <- nrow(data)

    # exposure name
    aname <- deparse(substitute(exposure))

    # extract input variables
    exposure <- eval(substitute(exposure), data, parent.frame())
    id <- eval(substitute(id), data, parent.frame())
    time <- eval(substitute(time), data, parent.frame())

    # check lengths of exposure, id, and time
    stopifnot(length(exposure)==n, length(id)==n, length(time)==n)

    # check if number of units * number of times equals n
    if(length(unique(id)) * length(unique(time)) != n){
      stop("Data must be in long format where # rows equals # units times # periods.")
    }

    # unique id positions
    unique_pos <- !duplicated(id)

    # base weights
    if(missing(base_weights)) bweights <- rep(1, sum(unique_pos)) else{
      bweights <- eval(substitute(base_weights), data, parent.frame())
      if(length(bweights) != n) stop("base_weights must have the same length as data.")
      bweights <- bweights[unique_pos]
    }

    # balancing conditions long format
    res_prods <- Reduce(cbind, lapply(xmodels, rmat, d = exposure, dname = aname))

    # res prods in wide format
    res_prods <- data.frame(res_prods[order(id), ], check.names = FALSE)
    res_prods_wide <- Reduce(cbind, split(res_prods, time[order(id)]))

    # extract linearly independent columns
    res_prods_indep <- pivot(res_prods_wide)
    C <- as.matrix(res_prods_indep)

    # entropy balancing
    eb_out <- eb2(C = C, M = rep(0, ncol(C)), Q = bweights/sum(bweights),
                  max_iter = max_iter, print_level = print_level, tol = tol)

    # data frame consisting of id and weights
    weights <- data.frame(id = sort(id[unique_pos]), rbw = eb_out$W)

    list(weights = weights, constraints = res_prods_indep, eb_out = eb_out, call = cl)
}
