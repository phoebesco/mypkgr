#' Plot of the mvnpdf function
#'
#' R knows its an S3 method because we have '.' and are using 'plot' which is
#' recognized. use methods(plot) to see S3 methods - not sure what happens if
#' you don't write plot. S3method also noted in the namespace.
#'
#' @param x an object of class \code{mvnpdf} resulting from a call of the
#' \code{mnvpdf()} function.
#' @param ... graphical parameters passed to \code{plot()} function.
#'
#' @return Nothing is returned, only a plot is given.
#' @export
#'
#' @examples
#' pdfvalues <- mvnpdf(x=matrix(seq(-3, 3, by = 0.1), nrow = 1), Log=FALSE)
#' plot(pdfvalues)
plot.mvnpdf <- function(x, ...) {
  plot(x$x, x$y, type = "l", ...)
}
