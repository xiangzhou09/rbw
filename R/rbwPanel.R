#' Residual Balancing Weights for Analyzing Time-varying Treatments
#'
#' \code{rbwPanel} is a function that produces residual balancing weights (rbw) for
#' estimating the marginal effects of time-varying treatments. The user supplies
#' a long format data frame (each row being a unit-period) and a list of
#' fitted model objects for time-varying confounders. In the current implementation,
#' the residuals of a time-varying covariate \eqn{X_t} are balanced across both current
#' treatment \eqn{D_t} and the regressors of \eqn{X_t}.
#'
#' @param exposure Symbol for the exposure/treatment variable.
#' @param xmodels A list of fitted \code{lm} or \code{glm} objects for
#'   time-varying confounders.
#' @param id Symbol for the unit id variable.
#' @param time Symbol for the time variable.
#' @param data A data frame containing all variables in the model.
#' @param base_weights (Optional) Symbol for base weights.
#' @inheritParams eb2
#'
#' @return A list containing the results.
#'  \item{weights}{A data frame containing id and residual balancing weights.}
#'  \item{constraints}{A matrix of (linearly independent) residual balancing constraints}
#'  \item{eb_out}{Results from calling \code{\link{eb2}} function}
#'  \item{call}{The matched call.}
#' @export
#' @import rlang
#' @examples
#' # models for time-varying confounders
#' m1 <- lm(dem.polls ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) * factor(week),
#' data = campaign_long)
#' m2 <- lm(undother ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) * factor(week),
#' data = campaign_long)
#'
#' xmodels <- list(m1, m2)
#'
#' # residual balancing weights
#' rbwPanel_fit <- rbwPanel(exposure = d.gone.neg, xmodels = xmodels, id = id,
#' time = week, data = campaign_long)
#'
#' summary(rbwPanel_fit$weights)
#'
#' # merge weights into wide-format data
#' campaign_wide2 <- merge(campaign_wide, rbwPanel_fit$weights, by = "id")
#'
#' # fit a marginal structural model (adjusting for baseline confounders)
#' if(require(survey)){
#'   rbw_design <- svydesign(ids = ~ 1, weights = ~ rbw, data = campaign_wide2)
#'   msm_rbw <- svyglm(demprcnt ~ cum_neg * deminc + camp.length + factor(year) + office,
#'   design = rbw_design)
#'   summary(msm_rbw)
#' }
rbwPanel <- function(exposure, xmodels, id, time, data,
                     base_weights, max_iter = 200,
                     print_level = 1, tol = 1e-4) {

    # match call
    cl <- match.call()

    # check missing arguments
    if(missing(exposure)) stop("'exposure' must be provided.")
    if(missing(xmodels)) stop("'xmodels' must be provided.")
    if(missing(id)) stop("'id' must be provided.")
    if(missing(time)) stop("'time' must be provided.")
    if(missing(data)) stop("'data' must be provided.")

    # check xmodels and data type
    if(!is.list(xmodels)) stop("xmodels must be a list.")
    if(!all(unlist(lapply(xmodels, inherits, "lm")))){
      stop("Each element of xmodels must inherit the class 'lm'")
      xnames <- vapply(xmodels, function(mod) names(model.frame(mod))[[1]],
                       character(1L))
    }
    if(!is.data.frame(data)) stop("'data' must be a data.frame.")
    n <- nrow(data)

    # extract id and time
    id <- eval_tidy(ensym(id), data)
    time <- eval_tidy(ensym(time), data)

    # unique id positions
    unique_pos <- !duplicated(id)

    # base weights
    if(missing(base_weights)){
      bweights <- rep(1, sum(unique_pos))
    } else{
      bweights <- eval_tidy(enquo(base_weights), data)
      if(length(bweights) != n) stop("'base_weights' must have the same length as 'data'.")
      bweights <- bweights[unique_pos]
    }

    # construct model matrix for treatment (without intercept)
    a_mat <- eval_tidy(expr(model.matrix(~ !!ensym(exposure), data)))
    a <- `colnames<-`(a_mat[, -1, drop = FALSE], colnames(a_mat)[-1])

    # balancing conditions long and wide formats

    res_prods <- Reduce(cbind, mapply(rmat, xmodels, xnames, MoreArgs = list(a = a),
                                      SIMPLIFY = FALSE))
    tmp <- split(data.frame(id, res_prods, check.names = FALSE), time)
    for(i in seq_along(tmp)){
      names(tmp[[i]])[-1] <- paste(names(tmp[[i]])[-1], i, sep = "_t")
    }
    res_prods_wide <- as.matrix(Reduce(full_merge, tmp)[-1])
    res_prods_wide[is.na(res_prods_wide)] <- 0

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
