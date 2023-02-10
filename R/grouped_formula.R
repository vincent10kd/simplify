#' @title A formula function for grouped.df objects
#'
#' @description Easily write a variable-level formula by using group-based shorthand notation.
#'
#' @param grouped.df A grouped.df object.
#' @param gform A formula using variable group names instead of individual variable names.
#' @param scale Scales all numeric variables in the formula.

#' @return A variable-level formula.
#' @examples
#' grouped_formula(g,outcome~phenotype+medication)
#' grouped_formula(g,outcome~phenotype+medication+protein_555+gene_2450)
#' @export

grouped_formula <- function(grouped.df,gform,scale=FALSE){
  if(class(grouped.df)!='grouped.df') stop('grouped.df should be of class grouped.df.')
  if(class(gform)!='formula') stop('gform should be of class formula.')
  # take apart formula
  form <- as.character(gform)
  form.y <- form[2]
  form.X <- trimws(unlist(strsplit(form[3],'\\+')))
  condition <- FALSE
  # separate terms that come after | operator
  if(length(grep('\\|',form.X)>0)){
    condition <- TRUE
    form.alt <- strsplit(form.X,'\\|')
    cond <- which(unlist(lapply(form.alt,length))>1)
    keep <- trimws(form.alt[[cond]][1])
    sep <- trimws(form.alt[[cond]][2])
    form.X[cond] <- keep
  }
  # put formula back together
  form.X2 <- classes <- character()
  for(x in form.X){
    x2 <- if(x%in%names(grouped.df)) colnames(grouped.df[[x]])
    else x
    if(x2[1]%in%colnames(grouped.df[[x]])){
      classes <- c(classes,sapply(grouped.df[[x]],class))
    }
    form.X2 <- c(form.X2,x2)
  }
  if(scale==TRUE){
    numInd <- which(form.X2%in%names(classes))
    numInd <- numInd[which(classes=='numeric')]
    if(length(numInd)>0) form.X2[numInd] <- sprintf('scale(%s)',form.X2[numInd])
  }
  gform <- reformulate(form.X2,form.y)
  if(condition==TRUE){
    gform <- update(gform,paste0('~.|',sep))
    gform[[3]] <- gform[[3]][[2]]
  }
  return(gform)
}

