utils::globalVariables(c("C", "M", "Q"))

# extract independent columns from a matrix
pivot <- function(mat, eps = 1e-10) {
  nonzero <- colSums(abs(mat) > eps) > 0
  mat <- mat[, nonzero, drop = FALSE]
  decomp <- qr(mat)
  indep <- decomp$pivot[seq_len(decomp$rank)]
  mat[, indep, drop = FALSE]
}

# generate residual * H matrices
rmat <- function(mod, a, aname) {
  x <- model.frame(mod)[[1L]]
  xname <- names(model.frame(mod))[1L]
  pred <- model.matrix(mod)
  resid <- x - mod[["fitted.values"]]
  if(is.factor(a)) a <- unclass(model.matrix(~a)[, -1])
  out <- cbind(resid * pred, resid * a)
  colnames(out) <- paste0(xname, "_res*", c(colnames(pred), aname))
  out
}

# line searcher for eb2()
line_searcher <- function(coefs, newton, ss) {
  Z <- coefs - (ss * newton)
  weights.temp <- c(Q * exp(C %*% Z))/sum(Q * exp(C %*% Z))
  C.agg <- c(weights.temp %*% C)
  maxdiff <- max(abs(C.agg - M))
  return(maxdiff)
}

full_merge <- function(x, y) merge(x, y, by = "id", all = TRUE)
