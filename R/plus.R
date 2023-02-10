#' @title An operator to quickly compose data.frames from predefined variable groups
#'
#' @description Does the same as compose() in an intuitive manner.
#'
#' @return A data.frame.
#' @examples
#' clin <- g$phenotype + g$medication
#' head(clin)
#' @export
#'

`+.element.df` <- function(...){
  newdat <- do.call(cbind,list(...))
  class(newdat) <- c('element.df','data.frame')
  return(newdat)
}

