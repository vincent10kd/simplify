#' @title A plotting function for grouped.df objects
#'
#' @description Creates a barchart of the variable groups in a grouped.df.

#' @return A barchart.

#' @export
#'

plot.grouped.df <- function(grouped.df){
  varNo <- unlist(lapply(grouped.df,ncol))[-length(grouped.df)]
  groups <- names(grouped.df)[-length(grouped.df)]
  groups <- paste0(groups,'\n[n=',varNo,']')
  gdf <- data.frame(groups,varNo)
  barplot(varNo~groups,ylab='Variables per group',
          col=as.numeric(factor(groups)),
          main=paste0('Visual summary of ',substitute(grouped.df)),
          log='y')
}

