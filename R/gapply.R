#' @title An apply function for grouped.df objects
#'
#' @description Applies a function to a variable group in a grouped.df object.
#'
#' @param grouped.df A grouped.df object.
#' @param groupname A character string referring to a variable group in a grouped.df object.
#' @param fun The function to be applied to all variables in the specified group.

#' @return An element.df object after applying the function.
#' @examples
#' # convert all variables in the group 'proteome' to numeric
#' head(sapply(g$proteome,class))
#' g$proteome <- gapply(g,'proteome',as.numeric)
#' head(sapply(g$proteome,class))
#' @export

gapply <- function(grouped.df,groupname,fun){
  if(class(grouped.df)!='grouped.df') stop('grouped.df should be of class grouped.df.')
  if(!groupname%in%names(grouped.df)) stop('groupname not recognized as a group in grouped.df. Should be of class character.')
  grouped.df[[groupname]] <- data.frame(lapply(grouped.df[[groupname]],fun))
  class(grouped.df[[groupname]]) <- c('element.df','data.frame')
  return(grouped.df[[groupname]])
}

