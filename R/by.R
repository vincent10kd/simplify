#' @title By - an operator to apply a function stratified by a variable
#'
#' @description Does the same as aggregate() in an intuitive manner.
#'
#' @param fun A function.
#' @param by A variable to perform the function stratified by.
#'
#' @return The result of the function.
#' @examples
#' mean(iris$Sepal.Length) %by% iris$Species
#' @export
#'

`%by%` <- function(fun, by){
  mc <- as.list(match.call())
  fname <- mc[[2]][[1]]
  x <- eval(mc[[2]][[2]])
  if(class(x)!='numeric'){
    stop('The first argument of the lefthand function should be of class numeric.')
  }
  by <- eval(mc[[3]])
  if(!class(by)[1]%in%c('factor','character','numeric','integer')){
    stop('The object on the right side of the %by% operator should be of class factor, character, numeric or integer.')
  }
  result <- aggregate(x ~ by, FUN=fname)
  colnames(result) <- c('subgroup',fname)
  return(result)
}

