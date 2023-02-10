#' @title Where - a subsetting operator
#'
#' @description Does the same as subset().
#'
#' @param x A data.frame or matrix
#'
#' @return A subset of the original data.frame or matrix.
#' @examples
#' iris %where% (Sepal.Length > 6)
#' @export
#'

`%where%` <- function(x,subset){
  mc <- match.call()
  mc[[1]] <- as.name('subset')
  eval(mc,parent.frame())
}
