#' Residual Balancing Weights for Estimating the Average Treatment Effect (ATE) in a Point Treatment Setting
#'
#' \code{rbwPoint} is a function that produces residual balancing weights in a point treatment setting. It takes
#' a set of baseline confounders and computes the residuals for each confounder by centering it around
#' its sample mean. The weights can be used to fit marginal structural models to estimate the average treatment
#' effect (ATE).
#'
#' @param treatment A symbol or character string for the treatment variable in \code{data}.
#' @param baseline_x An expression for a set of baseline confounders stored in \code{data} or a character
#'  vector of the names of these variables.
#' @inheritParams rbwMed
#'
#' @return A list containing the results.
#'  \item{weights}{A vector of residual balancing weights.}
#'  \item{constraints}{A matrix of (linearly independent) residual balancing constraints}
#'  \item{eb_out}{Results from calling the \code{\link{eb2}} function}
#'  \item{call}{The matched call.}
#' @export
#'
#' @examples
#' # residual balancing weights
#' rbwPoint_fit <- rbwPoint(treat, baseline_x = c(log_TotalPop, PercentOver65, log_Inc,
#'   PercentHispanic, PercentBlack, density,
#'   per_collegegrads, CanCommute), data = advertisement)
#'
#' # attach residual balancing weights to data
#' advertisement$rbw_point <- rbwPoint_fit$weights
#'
#' # fit marginal structural model
#' if(require(survey)){
#'   rbw_design <- svydesign(ids = ~ 1, weights = ~ rbw_point, data = advertisement)
#'   # the outcome model includes the treatment, the square of the treatment,
#'   # and state-level fixed effects (Fong, Hazlett, and Imai 2018)
#'   msm_rbwPoint <- svyglm(Cont ~ treat + I(treat^2) + factor(StFIPS), design = rbw_design)
#'   summary(msm_rbwPoint)
#' }
#'
rbwPoint <- function(treatment, data, baseline_x, base_weights,
                   max_iter = 200, tol = 1e-4, print_level = 1) {

  # match call
  cl <- match.call()

  # check and quote treatment
  if(missing(treatment)) stop("'treatment' must be provided.")
  if(!(typeof(enexpr(treatment)) %in% c("symbol", "character"))){
    stop("'treatment' must be a symbol or character string")
  }
  treatment <- ensym(treatment)

  # check data
  if(missing(data)) stop("'data' must be provided.")
  if(!is.data.frame(data) || nrow(data) < 2) stop("'data' must be a data frame with at least 2 rows.")
  n <- nrow(data)

  # check if treatment is in data
  treatment_name <- as_string(treatment)
  if(!(treatment_name %in% names(data))) stop(paste0(treatment_name, " is not in 'data'"))

  # check base weights
  if(missing(base_weights)){
    bweights <- rep(1, n)
  } else{
    bweights <- eval_tidy(enquo(base_weights), data)
    if(length(bweights) != n || !is.double(bweights)){
      stop("'base_weights' must be numeric and have the same length as 'data'.")
    }
  }

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
    stop("Baseline confounders are not provided.")
  }

  # construct model matrix for treatment (without intercept)
  a_mat <- eval_tidy(quo(model.matrix(~ !!treatment, data)))
  a <- `colnames<-`(a_mat[, -1, drop = FALSE], colnames(a_mat)[-1])
  if(nrow(a) != n) stop("'treatment' must have the same length as 'data'.")

  # construct balancing constraints
  res_prods_xa <- Reduce(cbind, mapply(rmat, xmodels, xnames, MoreArgs = list(a = a),
                                         SIMPLIFY = FALSE))
  res_prods <- res_prods_xa

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



