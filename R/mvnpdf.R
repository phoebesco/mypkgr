#' mvnpdf
#'
#' Compute the value of the density of a multivariate normal distribution
#' on R^p at n points.
#'
#' Details
#'
#' @param x data matrix
#' @param mean mean vector of the multivariate normal distribution
#' @param varcovM variance-covariance matrix
#' @param Log if TRUE (default) returns the log scale
#'
#' @returns a list of length 2 with the data matrix as the first argument
#' and a vector of length n with the multivariate normal distribution
#' density values at the points in x.
#' @export
#' @importFrom mvtnorm dmvnorm
#' @examples example
mvnpdf <- function(x, mean =  rep(0, nrow(x)),
                   varcovM = diag(nrow(x)), Log = TRUE) {
  n <- ncol(x)
  p <- nrow(x)
  x0 <- x - mean
  Rinv <- solve(varcovM)
  LogDetvarcovM <- log(det(varcovM))

  y <- NULL
  for (j in 1:n) {
    yj <- - p/2 * log(2*pi) - 0.5 * LogDetvarcovM -
      0.5 * t(x0[, j]) %*% Rinv %*% x0[, j]
    y <- c(y, yj)
  }

  if (!Log) {
    y <- exp(y)
  }

  res <- list(x = x, y = y)
  class(res) <- "mvnpdf"
  return(res)
}
