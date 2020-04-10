#' Residual Balancing Weights for Causal Mediation Analysis
#'
#' \code{rbwMed} is a function that produces residual balancing weights for
#' causal mediation analysis. The weights can be used to fit marginal
#' structural models for the joint effects of the treatment and a mediator.
#'
#' @param treatment Symbol for the treatment variable.
#' @param mediator Symbol for the mediator variable.
#' @param zmodels A list of fitted \code{lm} or \code{glm} objects for
#'   post-treatment confounders of the mediator-outcome relationship. If there's no
#'   post-treatment confounder, set it to be \code{NULL}.
#' @param baseline_x (Optional) Expression for a set of numeric-valued baseline confounders.
#' @param interact A logical variable indicating whether covariates should also be balanced
#'   against the treatment-mediator interaction term(s).
#' @inheritParams eb2
#' @inheritParams rbwPanel
#'
#' @return A list containing the results.
#'  \item{weights}{A vector of residual balancing weights.}
#'  \item{constraints}{A matrix of (linearly independent) residual balancing constraints}
#'  \item{eb_out}{Results from calling \code{\link{eb2}} function}
#'  \item{call}{The matched call.}
#' @export
#'
#' @examples
#' # models for post-treatment confounders
#' m1 <- lm(cesd92 ~ male + black + test_score + educ_exp +  father +
#'   hispanic + urban + educ_mom + num_sibs + college, weights = weights, data = education)
#' m2 <- lm(prmarr98 ~ male + black + test_score + educ_exp +  father +
#'   hispanic + urban + educ_mom + num_sibs + college, weights = weights, data = education)
#' m3 <- lm(transitions98 ~ male + black + test_score + educ_exp + father +
#'   hispanic +urban + educ_mom + num_sibs + college, weights = weights, data = education)
#'
#' # residual balancing weights
#' rbwMed_fit <- rbwMed(treatment = college, mediator = ses, baseline_x = male:num_sibs,
#'   zmodels = list(m1, m2, m3), interact = TRUE, base_weights = weights, data = education)
#' summary(rbwMed_fit$weights)
#'
#' # attach residual balancing weights to data
#' education$rbw <- rbwMed_fit$weights
#'
#' # fit marginal structural model
#' if(require(survey)){
#'   rbw_design <- svydesign(ids = ~ 1, weights = ~ rbw, data = education)
#'   msm_rbw <- svyglm(cesd40 ~ college * ses, design = rbw_design)
#'   summary(msm_rbw)
#' }
rbwMed <- function(treatment, mediator, zmodels, data,
                   baseline_x, interact = FALSE, base_weights,
                   max_iter = 200, print_level = 1, tol = 1e-4) {

  # match call
  cl <- match.call()

  # check treatment and mediator
  if(missing(treatment)) stop("'treatment' must be provided.")
  if(missing(mediator)) stop("'mediator' must be provided.")

  # check zmodels and data type
  if(missing(zmodels)) stop("'zmodels' must be provided.")
  if(!is.null(zmodels)){
    if(!is.list(zmodels)) stop("'zmodels' must be a list.")
    if(!all(unlist(lapply(zmodels, inherits, "lm")))){
      stop("Each element of zmodels must inherit the class 'lm'")
    }
  }

  # check data
  if(missing(data)) stop("'data' must be provided.")
  if(!is.data.frame(data)) stop("'data' must be a data frame.")
  n <- nrow(data)

  # check base weights
  if(missing(base_weights)){
    bweights <- rep(1, n)
  } else{
    bweights <- eval_tidy(enquo(base_weights), data)
    if(length(bweights) != n) stop("'base_weights' must have the same length as 'data'.")
  }

  # construct model matrix for treatment (without intercept)
  a_mat <- eval_tidy(expr(model.matrix(~ !!ensym(treatment), data)))
  a <- `colnames<-`(a_mat[, -1, drop = FALSE], colnames(a_mat)[-1])

  # construct model matrix for mediator (without intercept)
  m_mat <- eval_tidy(expr(model.matrix(~ !!ensym(mediator), data)))
  m <- `colnames<-`(m_mat[, -1, drop = FALSE], colnames(m_mat)[-1])

  # construct xmodels for baseline confounders
  if(!missing(baseline_x)) {
    nl <- as.list(seq_along(data))
    names(nl) <- names(data)
    vars <- eval_tidy(enquo(baseline_x), nl, empty_env())
    x <- data[, vars, drop = FALSE]
    xmodels <- lapply(x, function(y) lm(y ~ 1, weights = bweights))
  } else xmodels <- NULL

  if(interact == TRUE){
    am_mat <- eval_tidy(expr(model.matrix(~ (!!ensym(treatment)) * (!!ensym(mediator)), data)))
    am <- `colnames<-`(am_mat[, -1, drop = FALSE], colnames(am_mat)[-1])
    res_prods_xam <- Reduce(cbind, lapply(xmodels, rmat, am))
    res_prods_zam <- Reduce(cbind, lapply(zmodels, rmat, am))
    res_prods <- cbind(res_prods_xam, res_prods_zam)
  } else {
    res_prods_xa <- Reduce(cbind, lapply(xmodels, rmat, a))
    res_prods_xm <- Reduce(cbind, lapply(xmodels, rmat, m))
    res_prods_zm <- Reduce(cbind, lapply(zmodels, rmat, m))
    res_prods <- cbind(res_prods_xa, res_prods_xm, res_prods_zm)
  }

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
