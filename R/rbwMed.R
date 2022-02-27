#' Residual Balancing Weights for Causal Mediation Analysis
#'
#' \code{rbwMed} is a function that produces residual balancing weights for estimating
#' controlled direct/mediator effects in causal mediation analysis. The user supplies
#' a (optional) set of baseline confounders and a list of model objects for the conditional
#' mean of each post-treatment confounder given the treatment and baseline confounders.
#' The weights can be used to fit marginal structural models for the joint effects of the
#' treatment and a mediator on an outcome of interest.
#'
#' @param treatment A symbol or character string for the treatment variable in \code{data}.
#' @param mediator A symbol or character string for the mediator variable in \code{data}.
#' @param zmodels A list of fitted \code{lm} or \code{glm} objects for
#'   post-treatment confounders of the mediator-outcome relationship. If there's no
#'   post-treatment confounder, set it to be \code{NULL}.
#' @param baseline_x (Optional) An expression for a set of baseline confounders stored in \code{data} or
#'  a character vector of the names of these variables.
#' @param interact A logical variable indicating whether baseline and post-treatment covariates
#'   should be balanced against the treatment-mediator interaction term(s).
#' @inheritParams rbwPanel
#' @inheritParams eb2
#'
#' @return A list containing the results.
#'  \item{weights}{A vector of residual balancing weights.}
#'  \item{constraints}{A matrix of (linearly independent) residual balancing constraints}
#'  \item{eb_out}{Results from calling the \code{\link{eb2}} function}
#'  \item{call}{The matched call.}
#' @export
#'
#' @examples
#' # models for post-treatment confounders
#' m1 <- lm(threatc ~ ally + trade + h1 + i1 + p1 + e1 + r1 +
#'   male + white + age + ed4 + democ, data = peace)
#'
#' m2 <- lm(cost ~ ally + trade + h1 + i1 + p1 + e1 + r1 +
#'   male + white + age + ed4 + democ, data = peace)
#'
#' m3 <- lm(successc ~ ally + trade + h1 + i1 + p1 + e1 + r1 +
#'   male + white + age + ed4 + democ, data = peace)
#'
#' # residual balancing weights
#' rbwMed_fit <- rbwMed(treatment = democ, mediator = immoral,
#'   zmodels = list(m1, m2, m3), interact = TRUE,
#'   baseline_x = c(ally, trade, h1, i1, p1, e1, r1, male, white, age, ed4),
#'   data = peace)
#'
#' # attach residual balancing weights to data
#' peace$rbw_cde <- rbwMed_fit$weights
#'
#' # fit marginal structural model
#' if(require(survey)){
#'   rbw_design <- svydesign(ids = ~ 1, weights = ~ rbw_cde, data = peace)
#'   msm_rbwMed <- svyglm(strike ~ democ * immoral, design = rbw_design)
#'   summary(msm_rbwMed)
#' }
rbwMed <- function(treatment, mediator, zmodels, data,
                   baseline_x, interact = FALSE, base_weights,
                   max_iter = 200, tol = 1e-4, print_level = 1) {

  # match call
  cl <- match.call()

  # check and quote treatment and mediator
  if(missing(treatment)) stop("'treatment' must be provided.")
  if(missing(mediator)) stop("'mediator' must be provided.")
  if(!(typeof(enexpr(treatment)) %in% c("symbol", "character"))){
    stop("'treatment' must be a symbol or character string")
  }
  if(!(typeof(enexpr(mediator)) %in% c("symbol", "character"))){
    stop("'mediator' must be a symbol or character string")
  }
  treatment <- ensym(treatment)
  mediator <- ensym(mediator)

  # check data
  if(missing(data)) stop("'data' must be provided.")
  if(!is.data.frame(data) || nrow(data) < 2) stop("'data' must be a data frame with at least 2 rows.")
  n <- nrow(data)

  # check if treatment and mediator are both in data
  treatment_name <- as_string(treatment)
  mediator_name <- as_string(mediator)
  if(!(treatment_name %in% names(data))) stop(paste0(treatment_name, " is not in 'data'"))
  if(!(mediator_name %in% names(data))) stop(paste0(mediator_name, " is not in 'data'"))

  # check interact
  if(!is.logical(interact) || length(interact) > 1 || is.na(interact)){
    stop("'interact' must be a logical scalar")
  }

  # check base weights
  if(missing(base_weights)){
    bweights <- rep(1, n)
  } else{
    bweights <- eval_tidy(enquo(base_weights), data)
    if(length(bweights) != n || !is.double(bweights)){
      stop("'base_weights' must be numeric and have the same length as 'data'.")
      }
  }

  # check zmodels
  if(missing(zmodels)) stop("'zmodels' must be provided.")
  if(!is.null(zmodels)){
    if(!is.list(zmodels)) stop("'zmodels' must be a list.")
    if(!all(unlist(lapply(zmodels, inherits, "lm")))){
      stop("Each element of zmodels must inherit the class 'lm'")
    }
  }
  znames <- vapply(zmodels, function(mod) names(model.frame(mod))[[1]], character(1L))

  # construct xmodels for baseline confounders
  if(!missing(baseline_x)) {
    baseline_x <- enquo(baseline_x)
    nl <- as.list(seq_along(data))
    names(nl) <- names(data)
    vars <- eval_tidy(baseline_x, nl)
    if(!(is.character(vars) || is.integer(vars))){
      stop("'baseline_x' should either be a character vector or an expression of baseline confounders in 'data'")
    }
    xnames <- if(is.character(vars)) vars else names(data)[vars]
    xform <- paste("~", paste(xnames, collapse = "+"))
    x <- model.matrix(eval_tidy(parse_expr(xform)), data)[, -1, drop = FALSE]
    xmodels <- apply(x, 2, function(y) lm(y ~ 1, weights = bweights))
    xnames <- names(xmodels)
  } else {
    xnames <- xmodels <- NULL
    if(is.null(xmodels) && is.null(zmodels)){
      stop("Neither 'zmodels' nor baseline confounders are provided.")
    }
  }

  if(interact == TRUE){
    # construct model matrix for treatment * mediator (without intercept)
    am_mat <- eval_tidy(quo(model.matrix(~ (!!treatment) * (!!mediator), data)))
    am <- `colnames<-`(am_mat[, -1, drop = FALSE], colnames(am_mat)[-1])
    if(nrow(am) != n) stop("'treatment * mediator' must have the same length as 'data'.")

    # construct balancing constraints
    res_prods_xam <- Reduce(cbind, mapply(rmat, xmodels, xnames, MoreArgs = list(a = am),
                                          SIMPLIFY = FALSE))
    res_prods_zam <- Reduce(cbind, mapply(rmat, zmodels, znames, MoreArgs = list(a = am),
                                          SIMPLIFY = FALSE))
    res_prods <- cbind(res_prods_xam, res_prods_zam)
  } else {
    # construct model matrix for treatment (without intercept)
    a_mat <- eval_tidy(quo(model.matrix(~ !!treatment, data)))
    a <- `colnames<-`(a_mat[, -1, drop = FALSE], colnames(a_mat)[-1])
    if(nrow(a) != n) stop("'treatment' must have the same length as 'data'.")

    # construct model matrix for mediator (without intercept)
    m_mat <- eval_tidy(quo(model.matrix(~ !!mediator, data)))
    m <- `colnames<-`(m_mat[, -1, drop = FALSE], colnames(m_mat)[-1])
    if(nrow(m) != n) stop("'mediator' must have the same length as 'data'.")

    # construct balancing constraints
    res_prods_xa <- Reduce(cbind, mapply(rmat, xmodels, xnames, MoreArgs = list(a = a),
                                         SIMPLIFY = FALSE))
    res_prods_xm <- Reduce(cbind, mapply(rmat, xmodels, xnames, MoreArgs = list(a = m),
                                         SIMPLIFY = FALSE))
    res_prods_zm <- Reduce(cbind, mapply(rmat, zmodels, znames, MoreArgs = list(a = m),
                                         SIMPLIFY = FALSE))
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
