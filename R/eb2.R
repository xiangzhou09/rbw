#' Function for Generating Minimum Entropy Weights Subject to a Set of Balancing
#'   Constraints
#'
#' \code{eb2} is an adaptation of \code{\link[ebal]{eb}} that generates
#' minimum entropy weights subject to a set of balancing constraints. Using
#' the method of Lagrange multipliers, the dual problem is an unconstrained
#' optimization problem that can be solved using Newton's method. When a full
#' Newton step is excessive, an exact line search is used to find the best step
#' size.
#'
#' @param C A constraint matrix.
#' @param M A vector of moment conditions to be met in the reweighted sample.
#' @param Q A vector of base weights.
#' @param Z A vector of Lagrange multipliers to be initialized.
#' @param max_iter Maximum number of iterations for Newton's method.
#' @param tol Tolerance parameter used to determine convergence.
#' @param print_level The level of printing: \describe{
#'  \item{1}{normal: print whether the algorithm converges or not}
#'  \item{2}{detailed: print also the maximum absolute value of the deviation between the moments
#'   of the reweighted data and the target moments in each iteration}
#'  \item{3}{very detailed: print also the step length of the line searcher in iterations where
#'   a full Newton step is excessive.}
#'  }
#'
#' @return A list containing the results from the algorithm.
#'  \item{W}{A vector of normalized minimum entropy weights.}
#'  \item{Z}{A vector of Lagrange multipliers.}
#'  \item{converged}{A logical indicator for convergence.}
#'  \item{maxdiff}{A scalar that contains the maximum deviation between the
#'   moments of the reweighted data and the target moments.}
#' @import stats
#' @export

eb2 <- function(C, M, Q, Z = rep(0, ncol(C)), max_iter = 200, tol = 1e-4, print_level = 2) {

  converged <- FALSE

  for (iter in 1:max_iter) {

    W <- c(Q * exp(C %*% Z))/sum(Q * exp(C %*% Z))
    sumC <- t(C) %*% W
    gradient <- sumC - M

    if (max(abs(gradient)) < tol) {
      converged <- TRUE
      break
    }

    if (print_level >= 2) {
      cat("Iteration", iter, "maximum deviation is =", format(max(abs(gradient)), digits = 4), "\n")
    }

    hessian = t(C) %*% (W * C)
    Z_old <- Z
    newton <- solve(hessian, gradient)

    Z <- Z_old - newton

    # reset environments for line_searcher
    environment(line_searcher) <- environment()

    loss.new <- line_searcher(coefs = Z, newton = newton, ss = 1)
    loss.old <- line_searcher(coefs = Z_old, newton = newton, ss = 0)

    if (print_level >= 3) {
      cat("new loss", loss.new, "old loss=", loss.old, "\n")
    }

    if (loss.old <= loss.new) {

      ss.out <- optimize(line_searcher, lower = 0, upper = 1, maximum = FALSE, coefs = Z_old, newton = newton)

      if (print_level >= 3) {
        cat("LS Step Length is ", ss.out$minimum, "\n")
        cat("Loss is", ss.out$objective, "\n")
      }

      Z <- Z_old - ss.out$minimum * newton
    }
  }

  if (converged) cat("Entropy minimization converged within tolerance level \n") else {
      message("Entropy minimization did not converge within tolerance level;")
      message("Try increasing the number of iterations or reducing balancing constraints \n")
  }

  W <- nrow(C) * W

  list(W = W, Z = Z, sumC = sumC, converged = converged, maxdiff = max(abs(gradient)))
}
