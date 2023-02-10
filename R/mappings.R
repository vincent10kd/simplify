#' @title Show all grouped mappings for a data.frame in the global environment
#'
#' @description Identifies all grouped.dfs corresponding to a given data.frame in the global environment.
#'
#' @param originaldf A data.frame in the global environment.

#' @examples
#' mappings(highdim_df)
#'
#' @export

mappings <- function(originaldf){
  if(class(originaldf)!='data.frame') stop('originaldf must be a data.frame')
  originalnm <- substitute(originaldf)
  envObjClass <- unlist(sapply(ls(globalenv()),function(x) class(get(x))))
  groupedInd <- which(envObjClass=='grouped.df')
  groupings <- names(envObjClass[groupedInd])
  groupedDfs <- list()
  for(.item in seq_along(groupings)){
    groupedDfs[[.item]] <- get(groupings[.item])
  }
  groupnumber <- unlist(lapply(groupedDfs,length))-1
  originals <- unlist(lapply(groupedDfs, function(x) return(x['original'])))
  mapInd <- which(originals==originalnm)
  mappings <- list(original=originalnm,
                   mappings=groupings[mapInd],
                   groupNo=groupnumber)
  class(mappings) <- c('mappings','list')
  cat('The data.frame',mappings[[1]],'is associated with',length(mappings[[2]]),
      'grouped.df',ifelse(length(mappings[[2]])==1,'object.','objects.'),'\n')
  cat('===============================\n')
  cat('Mappings:',paste0(mappings[[2]],collapse=', '),'\n')
  for(m in seq_along(mappings[[2]])){
    cat(mappings[[2]][m],'contains',mappings[[3]][m],'groups.\n')
  }
  return(invisible(mappings))
}
