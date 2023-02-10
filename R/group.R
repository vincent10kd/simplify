#' @title Create a data.frame with pre-defined variable groups
#'
#' @description Creates a grouped.df, a class that contains data and pre-defined variable groups in it.
#'
#' @param data A data.frame or matrix.
#' @param groups A list of character vectors, each containing the variable names corresponding to a specific group.

#' @return A grouped.df.
#' @examples
#' # create a fake dataset
#' pheno <- data.frame(id=id,
#'                     age=rnorm(100,65,5.8),
#'                     male=rbinom(100,1,0.5),
#'                     bmi=rnorm(100,33,5),
#'                     cancer=rbinom(100,1,0.15),
#'                     afib=rbinom(100,1,0.25),
#'                     ckd=rbinom(100,1,0.35),
#'                     chf=rbinom(100,1,0.20),
#'                     copd=rbinom(100,1,0.3))
#'
#' genotype <- data.frame(cbind(id,matrix(sample(2,5e6,replace=TRUE),nrow=100,ncol=5e4)))
#' colnames(genotype) <- c('id',paste0('gene_',seq(1,5e4)))
#'
#' protein <- data.frame(cbind(id,matrix(rnorm(2e5,0,0.35),nrow=100,ncol=2e3)))
#' colnames(protein) <- c('id',paste0('protein_',seq(1,2e3)))
#'
#' # consolidate into a larger dataset
#' highdim_df <- consolidate(pheno,medi,genotype,protein)
#'
#' # now group variables within this dataset
#' phenoVars <- colnames(pheno)[-1]
#' mediVars <- colnames(medi)[-1]
#' genoVars <- colnames(genotype)[-1]
#' proteinVars <- colnames(protein)[-1]
#'
#' g <- group(data=highdim_df,
#'            groups=list(phenotype=phenoVars,
#'                        medication=mediVars,
#'                        genotype=genoVars,
#'                        proteome=proteinVars))
#'
#' summary(g)
#' plot(g)
#'
#' @export

group <- function(data, groups){
  ref <- list()
  for(g in seq_along(groups)){
    groupVars <- unlist(groups[g])
    ref[[g]] <- data[groupVars]
    class(ref[[g]]) <- c('element.df','data.frame')
  }
  names(ref) <- names(groups)
  ref <- c(ref,original=as.character(substitute(data)))
  class(ref) <- 'grouped.df'
  cat('grouped.df with',length(groups),'groups created.')
  return(ref)
}
