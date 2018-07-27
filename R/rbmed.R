#' Residual Balancing Weights for Causal Mediation Analysis
#'
#' \code{rbmed} is a function that produces residual balancing weights for
#' causal mediation analysis. The weights can be used to fit marginal
#' structural models for the joint effects of the treatment and a mediator.
#'
#' @param treatment Expression for the treatment variable.
#' @param mediator Expression for the mediator variable.
#' @param zmodels A list of fitted \code{lm} or \code{glm} objects for
#'   post-treatment confounders of the mediator-outcome relationship.
#' @param baseline_x Expression for a set of baseline confounders. .
#' @inheritParams eb2
#' @inheritParams rbtvt
#'
#' @return A list containing the results.
#'  \item{weights}{A vector of residual balancing weights.}
#'  \item{constraints}{A matrix of (linearly independent) residual balancing constraints}
#'  \item{eb_out}{Results from calling \code{\link{eb2}} function}
#'  \item{call}{The matched call.}
#' @export
#' @examples
#' # models for post-treatment confounders
#' m1 <- lm(cesd92 ~ male + black + test_score + educ_exp +  father +
#'   hispanic + urban + educ_mom + num_sibs + college, data = education)
#' m2 <- lm(prmarr98 ~ male + black + test_score + educ_exp +  father +
#'   hispanic + urban + educ_mom + num_sibs + college, data = education)
#' m3 <- lm(transitions98 ~ male + black + test_score + educ_exp + father +
#'   hispanic +urban + educ_mom + num_sibs + college, data = education)
#'
#' # residual balancing weights
#' rbmed_fit <- rbmed(treatment = college, mediator = ses, baseline_x = male:num_sibs,
#'   zmodels = list(m1, m2, m3), base_weights = weights, data = education)
#' summary(rbmed_fit$weights)


rbmed <- function(treatment, mediator, zmodels, baseline_x, base_weights,
                  data, max_iter = 500, print_level = 1, tol = 1e-3) {

  # match call
  cl <- match.call()

  # check missing arguments
  if(missing(treatment)) stop("treatment must be provided.")
  if(missing(mediator)) stop("mediator must be provided.")
  if(missing(zmodels)) stop("zmodels must be provided.")
  if(missing(data)) stop("data must be provided.")

  # check xmodels and data type
  if(!is.list(zmodels)) stop("zmodels must be a list.")
  if(!all(unlist(lapply(zmodels, inherits, "lm")))){
    stop("Each element of zmodels must be an object of class `lm`")
  }
  if(!is.data.frame(data)) stop("data must be a data.frame.")
  n <- nrow(data)

  # treatment name
  aname <- deparse(substitute(treatment))
  mname <- deparse(substitute(mediator))

  # extract input variables
  a <- eval(substitute(treatment), data, parent.frame())
  m <- eval(substitute(mediator), data, parent.frame())

  # check lengths of treatment and mediator
  if(length(a) != n) stop("treatment must have the same length as data.")
  if(length(m) != n) stop("mediator must have the same length as data.")

  # base weights
  if(missing(base_weights)) bweights <- rep(1, n) else{
    bweights <- eval(substitute(base_weights), data, parent.frame())
    if(length(bweights) != n) stop("base_weights must have the same length as data.")
  }

  # construct res_prods for baseline confounders
  if(!missing(baseline_x)) {
    nl <- as.list(seq_along(data))
    names(nl) <- names(data)
    vars <- eval(substitute(baseline_x), nl, parent.frame())
    x <- data[, vars, drop = FALSE]
    xmodels <- lapply(x, function(y) lm(y ~ 1, weights = bweights))
    res_prods_xa <- Reduce(cbind, lapply(xmodels, rmat, d = a, dname = aname))
    res_prods_xm <- Reduce(cbind, lapply(xmodels, rmat, d = m, dname = mname))
  } else{
    res_prods_xa <- res_prods_xm <- NULL
  }

  # construct res_prods for posttreatment confounders
  res_prods_zm <- Reduce(cbind, lapply(zmodels, rmat, d = m, dname = mname))

  # construct res_prods for both x and z
  res_prods <- cbind(res_prods_xa, res_prods_xm, res_prods_zm)

  # extract linearly independent columns
  res_prods_indep <- pivot(res_prods)
  C <- as.matrix(res_prods_indep)

  # entropy balancing
  eb_out <- eb2(C = C, M = rep(0, ncol(C)), Q = bweights/sum(bweights),
                max_iter = max_iter, print_level = print_level, tol = tol)

  # data frame consisting of id and weights
  weights <- eb_out$W

  list(weights = weights, constraints = res_prods_indep, eb_out = eb_out, call = cl)
}
