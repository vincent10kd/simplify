#' @title A summary function for grouped.df objects
#'
#' @description Returns an overview of the variable groups in a grouped.df object.

#' @export
#'

summary.grouped.df <- function(grouped.df){
  gdf <- substitute(grouped.df)
  cat(gdf,'is a grouped.df containing',length(grouped.df)-1,'groups:\n',
      paste(names(grouped.df)[-length(grouped.df)],collapse=', '),'\n')
  cat('===============================\n')
  for(g in 1:(length(grouped.df)-1)){
    varNo <- ncol(grouped.df[[g]])
    cat(names(grouped.df)[g],'comprises',varNo,ifelse(varNo>1,'variables','variable'),
        'of class(es)',paste0(unique(sapply(grouped.df[[g]],class)),collapse=', '),'\n')
  }
}
