#' @title Create a data.frame from pre-defined variable groups in a grouped.df object
#'
#' @description Returns a data.frame containing the variables of a given set of groups.
#'
#' @param grouped.df A grouped.df object.
#' @param groups A character vector containing the variable group names.
#' @param names If TRUE, appends the variable group name as a prefix to the individual variables in that group.

#' @return A data.frame.
#' @examples
#' # create a new data.frame from a grouped.df
#' clin <- compose(g, groups=c('phenotype','medication'))
#' head(clin)
#'
#' @export

compose <- function(grouped.df, groups=NULL, names=FALSE){
  if(class(grouped.df)!='grouped.df') stop('compose only accepts objects of class grouped.df.')
  if(class(groups)!='character') stop('groups should be a character vector.')
  newdat <- data.frame(grouped.df[groups])
  if(names==FALSE) colnames(newdat) <- unlist(lapply(grouped.df[groups],colnames))
  return(newdat)
}
