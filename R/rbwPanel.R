#' Residual Balancing Weights for Analyzing Time-varying Treatments
#'
#' \code{rbwPanel} is a function that produces residual balancing weights (rbw) for
#' estimating the marginal effects of time-varying treatments. The user supplies
#' a long format data frame (each row being a unit-period) and a list of
#' fitted model objects for the conditional mean of each post-treatment confounder given
#' past treatments and past confounders. The residuals of each time-varying confounder
#' are balanced across both the current treatment \eqn{A_t} and the regressors of the confounder
#' model. In addition, when \code{future > 0}, the residuals are also balanced across future
#' treatments \eqn{A_{t+1},\ldots A_{t + future}}.
#'
#' @param treatment A symbol or character string for the treatment variable in \code{data}.
#' @param xmodels A list of fitted \code{lm} or \code{glm} objects for
#'   time-varying confounders.
#' @param id A symbol or character string for the unit id variable in \code{data}.
#' @param time A symbol or character string for the time variable in \code{data}. The time variable should be numeric.
#' @param data A data frame containing all variables in the model.
#' @param future An integer indicating the number of future treatments in the balancing conditions. When
#'  \code{future > 0}, the residualized time-varying covariates are balanced not only with respect to
#'  current treatment \eqn{A_t}, but also with respect to future treatments \eqn{A_{t+1},\ldots A_{t + future}}.
#' @param base_weights (Optional) A vector of base weights (or its name).
#' @param tol Tolerance parameter used to determine convergence in entropy minimization.
#'  See documentation for \code{\link{eb2}}.
#' @param print_level The level of printing. See documentation for \code{\link{eb2}}.
#' @inheritParams eb2
#'
#' @return A list containing the results.
#'  \item{weights}{A data frame containing the unit id variable and residual balancing weights.}
#'  \item{constraints}{A matrix of (linearly independent) residual balancing constraints}
#'  \item{eb_out}{Results from calling the \code{\link{eb2}} function}
#'  \item{call}{The matched call.}
#' @export
#' @import rlang
#' @importFrom dplyr `%>%`
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
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
#' rbwPanel_fit <- rbwPanel(treatment = d.gone.neg, xmodels = xmodels, id = id,
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
#'   msm_rbwPanel <- svyglm(demprcnt ~ cum_neg * deminc + camp.length + factor(year) + office,
#'   design = rbw_design)
#'   summary(msm_rbwPanel)
#' }
rbwPanel <- function(treatment, xmodels, id, time, data,
                     base_weights, future = 1L,
                     max_iter = 200, tol = 1e-4, print_level = 1) {

    # match call
    cl <- match.call()

    # check missing arguments
    if(missing(treatment)) stop("'treatment' must be provided.")
    if(missing(xmodels)) stop("'xmodels' must be provided.")
    if(missing(id)) stop("'id' must be provided.")
    if(missing(time)) stop("'time' must be provided.")
    if(missing(data)) stop("'data' must be provided.")

    #  check and quote treatment, id, and time
    if(!(typeof(enexpr(treatment)) %in% c("symbol", "character"))){
      stop("'treatment' must be a symbol or character string")
    }
    if(!(typeof(enexpr(id)) %in% c("symbol", "character"))){
      stop("'id' must be a symbol or character string")
    }
    if(!(typeof(enexpr(time)) %in% c("symbol", "character"))){
      stop("'time' must be a symbol or character string")
    }
    treatment <- ensym(treatment)
    id_sym <- ensym(id)
    time_sym <- ensym(time)

    # check if all elements of xmodels inherit the lm class
    if(!is.list(xmodels)) stop("'xmodels' must be a list.")
    if(!all(unlist(lapply(xmodels, inherits, "lm")))){
      stop("Each element of 'xmodels' must inherit the class 'lm'")
    }
    xnames <- vapply(xmodels, function(mod) names(model.frame(mod))[[1]], character(1L))

    # check if data is a data.frame
    if(!is.data.frame(data) || nrow(data) < 2) stop("'data' must be a data frame with at least 2 rows.")
    n <- nrow(data)

    # check if treatment, id, and time are all in data
    treatment_name <- as_string(treatment)
    id_name <- as_string(id_sym)
    time_name <- as_string(time_sym)
    if(!(treatment_name %in% names(data))) stop(paste0(treatment_name, " is not in 'data'"))
    if(!(id_name %in% names(data))) stop(paste0(id_name, " is not in 'data'"))
    if(!(time_name %in% names(data))) stop(paste0(time_name, " is not in 'data'"))

    # extract id and time
    id <- eval_tidy(id_sym, data)
    time <- eval_tidy(time_sym, data)
    if(length(id) != n) stop("'id' must have the same length as 'data'.")
    if(length(time) != n) stop("'time' must have the same length as 'data'.")

    # number of time points
    nT <- length(unique(time))

    # unique id positions
    unique_pos <- !duplicated(id)

    # base weights
    if(missing(base_weights)){
      bweights <- rep(1, sum(unique_pos))
    } else{
      bweights <- eval_tidy(enquo(base_weights), data)
      if(length(bweights) != n || !is.double(bweights)){
        stop("'base_weights' must be numeric and have the same length as 'data'.")
      }
      bweights <- bweights[unique_pos]
    }

    # check if future is numeric
    if(!is.numeric(future)) stop("'future' must be a nonnegative integer.")

    if(future > 0){

      # number of future treatments to be balanced against
      nfuture <- min(future, nT - 1)

      # symbols for future treatments
      treatments <- c(treatment, syms(lapply(1:nfuture, function(t) paste0(as_string(treatment), "_f", t))))

      # create future treatments in data
      data <-  data %>%
        mutate(uniqueID = seq_len(n)) %>%
        arrange(!!id_sym, !!time_sym) %>%
        group_by(!!id_sym)
      for (t in seq(1, nfuture)){
        data <- data %>%
          mutate(!!treatments[[t+1]]:= dplyr::lead(!!treatment, t))
      }
      data <- data %>%
        ungroup() %>%
        arrange(uniqueID)

      aform <- Reduce(function(x, y) expr(!!x + !!y), treatments)

    } else{
      aform <- treatment
    }

    # construct model matrix for treatment (without intercept)
    amat <- eval_tidy(quo(model.matrix(~ !!aform, model.frame(~ !!aform, data, na.action=na.pass))))
    a <- `colnames<-`(amat[, -1, drop = FALSE], colnames(amat)[-1])
    if(nrow(a) != n) stop("'treatment' must have the same length as 'data'.")

    # balancing conditions long and wide formats
    res_prods <- Reduce(cbind, mapply(rmat, xmodels, xnames, MoreArgs = list(a = a),
                                      SIMPLIFY = FALSE))
    tmp <- split(data.frame(id, res_prods, check.names = FALSE), time)
    # remove columns with all NAs
    tmp <- lapply(tmp, function(df) df[,colSums(is.na(df)) < nrow(df)])
    for(i in seq_along(tmp)){
      names(tmp[[i]])[-1] <- paste(names(tmp[[i]])[-1], i, sep = "_t")
    }
    # full_merge sorts rows by id
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
