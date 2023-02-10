#' @title Consolidate several data.frames together
#'
#' @description A chained merge command of data.frames that share an ID variable.
#'
#' @param ... Multiple data.frames.
#' @param by The variable by which to merge.
#' @param all.x If TRUE, merges such that all IDs in the lefthand side data.frame are kept.
#' @param all.y If TRUE, merges such that all IDs across all data.frames are kept.

#' @return A data.frame.
#' @examples
#' pheno <- data.frame(id=id,
#' age=rnorm(100,65,5.8),
#' male=rbinom(100,1,0.5),
#' bmi=rnorm(100,33,5),
#' cancer=rbinom(100,1,0.15),
#' afib=rbinom(100,1,0.25),
#' ckd=rbinom(100,1,0.35),
#' chf=rbinom(100,1,0.20),
#' copd=rbinom(100,1,0.3))
#'
#' genotype <- data.frame(cbind(id,matrix(sample(2,5e6,replace=TRUE),nrow=100,ncol=5e4)))
#' colnames(genotype) <- c('id',paste0('gene_',seq(1,5e4)))
#'
#' protein <- data.frame(cbind(id,matrix(rnorm(2e5,0,0.35),nrow=100,ncol=2e3)))
#' colnames(protein) <- c('id',paste0('protein_',seq(1,2e3)))
#' # now consolidate
#' highdim_df <- consolidate(pheno,medi,genotype,protein)
#' dim(highdim_df)
#' @export

consolidate <- function(...,by=NULL,all.x=TRUE,all.y=FALSE){
  dfList <- list(...)
  if(length(dfList)<2) stop('consolidate requires at least two data.frames or grouped.dfs.')
  if(isFALSE(all(lapply(dfList,class)=='data.frame')|
             all(lapply(dfList,class)=='grouped.df'))){
    stop('consolidate only accepts objects of class data.frame or grouped.df.')
  }
  if(class(dfList[[1]])=='data.frame'){
    consolidate.df <- dfList[[1]]
    for(d in 2:length(dfList)){
      df <- data.frame(dfList[[d]])
      if(is.null(by)) by <- do.call(intersect,lapply(list(consolidate.df,df),colnames))
      consolidate.df <- merge(consolidate.df,df,by=by,all.x=all.x,all.y=all.y)
    }
    return(consolidate.df)
  }
  if(class(dfList[[1]])=='grouped.df'){
    originals <- unlist(lapply(dfList,function(x) x[['original']]))
    if(length(unique(originals))>1) stop('consolidate can only be used on grouped.dfs that were based on the same original data.frame.')
    dfList <- lapply(dfList,function(x) x[-length(x)])
    consolidate.gdf <- do.call('c',dfList)
    consolidate.gdf <- c(consolidate.gdf,original=unique(originals))
    names(consolidate.gdf) <- make.names(names(consolidate.gdf),unique=TRUE)
    class(consolidate.gdf) <- c('grouped.df')
    for(e in 1:(length(consolidate.gdf)-1)){
      sapply(consolidate.gdf[[e]],function(x) class(x) <- c('element.df','data.frame'))
    }
    return(consolidate.gdf)
  }
}

