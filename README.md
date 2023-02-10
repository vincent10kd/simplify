---
title: "simplify"
author: "vincent10kd@gmail.com"
date: "2021-04-06"
output: github_document
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

`%where%` <- function(x,subset){
  mc <- match.call()
  mc[[1]] <- as.name('subset')
  eval(mc,parent.frame())
}

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

group <- function(data,groups){
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

compose <- function(grouped.df,groups=NULL,names=FALSE){
  if(class(grouped.df)!='grouped.df') stop('compose only accepts objects of class grouped.df.')
  if(class(groups)!='character') stop('groups should be a character vector.')
  newdat <- data.frame(grouped.df[groups])
  if(names==FALSE) colnames(newdat) <- unlist(lapply(grouped.df[groups],colnames))
  return(newdat)
}

`+.element.df` <- function(...){
  newdat <- do.call(cbind,list(...))
  class(newdat) <- c('element.df','data.frame')
  return(newdat)
}

gapply <- function(grouped.df,groupname,fun){
  if(class(grouped.df)!='grouped.df') stop('grouped.df should be of class grouped.df.')
  if(!groupname%in%names(grouped.df)) stop('groupname not recognized as a group in grouped.df. Should be of class character.')
  grouped.df[[groupname]] <- data.frame(lapply(grouped.df[[groupname]],fun))
  class(grouped.df[[groupname]]) <- c('element.df','data.frame')
  return(grouped.df[[groupname]])
}

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

retrieve <- function(data,charVar,idVar,pattern,patternNames=TRUE){
  mc <- as.list(match.call())
  mc[[1]] <- NULL
  charName <- mc[[2]]
  # evaluate charVar in data
  charVar <- str2expression(paste0(mc[[1]],'$',mc[[2]]))
  charVar <- eval(charVar,parent.frame())
  idName <- mc[[3]]
  # evaluate idVar in data
  idVar <- str2expression(paste0(mc[[1]],'$',mc[[3]]))
  idVar <- eval(idVar,parent.frame())
  df <- aggregate(charVar~idVar,FUN=function(x) grep(pattern[1],x))
  df[,2] <- ifelse(is.na(as.numeric(df[,2])),0,1)
  colnames(df) <- c(idName,paste0(charName,'_',1))
  if(length(pattern)>1){
    for(p in 2:length(pattern)){
      df2 <- aggregate(charVar~idVar,FUN=function(x) grep(pattern[p],x))
      df2[,2] <- ifelse(is.na(as.numeric(df2[,2])),0,1)
      colnames(df2) <- c(idName,paste0(charName,'_',p))
      df <- cbind(df,df2[,2])
    }
  }
  if(patternNames==TRUE) colnames(df)[2:ncol(df)] <- gsub('[^[:alnum:]]','',pattern)
  return(df)
}

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
```

The **simplify** package simplifies data preprocessing steps, especially for high-dimensional datasets. It introduces grouped data.frames (the `group.df` class), lower dimensional representations of data.frames with many columns. Being able to think in terms of variable groups, which can each represent hundreds or thousands of variables, reduces the complexity of the dataset. This makes working with high-dimensional data easier and more dynamic. Moreover, **simplify** contains functions to simplify several common data preprocessing steps, such as merging several data.frames, subsetting, and applying functions to subgroups. 

## The %where% operator for simple subsetting
Subsetting is an essential step in data preprocessing. The **simplify** package introduces the `%where%` operator to reduce the mental bandwidth necessary to create subsets. Its syntax requires a data.frame or matrix on the left side of the operator, and the logical condition on the right side, enclosed by parentheses. The operator is a wrapper for the `base::subset` function. To illustrate, we will use the `iris` dataset.

```{r}
# iris
dim(iris)
head(iris)

i1 <- iris %where% (Species=='virginica' & Sepal.Length>5)
head(i1)
i2 <- iris %where% (Sepal.Width<3)
head(i2)
rm(i1, i2)
```

## The %by% operator to apply functions by subgroup
Similarly, the **simplify** package features the `%by%` operator, which facilitates running commands by subgroups. The function is a wrapper for `stats::aggregate`, and only works when the function takes a numeric argument, and the subgroup is of classes `character`, `factor`, `numeric` or `integer`. The subgroup should be a vector of equal length to the argument to the function. Some examples are shown below.

```{r}
mean(iris$Sepal.Length) %by% iris$Species
summary(iris$Petal.Width) %by% iris$Species
petal_quantiles <- cut(iris$Petal.Length,quantile(iris$Petal.Length))
median(iris$Sepal.Length) %by% petal_quantiles 
```

## Converting long character data into binary columns
Sometimes data is stored in long, 'free-text' format, and cannot be used without first transforming it to match the wide format of your main dataset. Think, for instance, of medication data that is stored by Anatomical Therapeutic Chemical (ATC) code, where there are several rows per patient ID corresponding to as many drugs taken at a given time point. Another common situation is that patient diagnoses may be stored in a long form character format, e.g. a single variable capturing all ICD-10 diagnoses across several rows per ID. In these situations, it is useful to be able to ad hoc define ATC codes or ICD-10 codes of interest, and determine whether each patient ID has a 'hit' for these codes or not. For this, the `retrieve()` function can be used. The `retrieve` function should be passed the name of the original data.frame or matrix, the name of the variable storing the information of interest, the name of the 'ID' variable by which the data should be summarized, and one or several regular expressions (`pattern`) to define the patterns of interest. An example is given below. First, let's generate an example dataset containing several ATC codes per individual. 

```{r}
# create data
set.seed(1234)
id <- paste0('pat_',sample(100,100,replace=FALSE))
atc3_codes <- c('A01A', 'A02A', 'A02B', 'A02X', 'A03A', 'A03B', 'A03C', 'A03D', 'A03E', 'A03F', 'A04A', 'A05A', 'A05B', 'A05C', 'A06A', 'A07A', 'A07B', 'A07C', 'A07D', 'A07E', 'A07F', 'A07X', 'A08A', 'A09A', 'A10A', 'A10B', 'A10X', 'A11A', 'A11B', 'A11C', 'A11D', 'A11E', 'A11G', 'A11H', 'A11J', 'A12A', 'A12B', 'A12C', 'A13A', 'A14A', 'A14B', 'A16A', 'B01A', 'B02A', 'B02B', 'B03A', 'B03B', 'B03X', 'B05A', 'B05B', 'B05C', 'B05D', 'B05X', 'B05Z', 'B06A', 'C01A', 'C01B', 'C01C', 'C01D', 'C01E', 'C02A', 'C02B', 'C02C', 'C02D', 'C02K', 'C02L', 'C02N', 'C03A', 'C03B', 'C03C', 'C03D', 'C03E', 'C03X', 'C04A', 'C05A', 'C05B', 'C05C', 'C07A', 'C07B', 'C07C', 'C07D', 'C07E', 'C07F', 'C08C', 'C08D', 'C08E', 'C08G', 'C09A', 'C09B', 'C09C', 'C09D', 'C09X', 'C10A', 'C10B', 'D01A', 'D01B', 'D02A', 'D02B', 'D03A', 'D03B', 'D04A', 'D05A', 'D05B', 'D06A', 'D06B', 'D06C', 'D07A', 'D07B', 'D07C', 'D07X', 'D08A', 'D09A', 'D10A', 'D10B', 'D11A', 'G01A', 'G01B', 'G02A', 'G02B', 'G02C', 'G03A', 'G03B', 'G03C', 'G03D', 'G03E', 'G03F', 'G03G', 'G03H', 'G03X', 'G04B', 'G04C', 'H01A', 'H01B', 'H01C', 'H02A', 'H02B', 'H02C', 'H03A', 'H03B', 'H03C', 'H04A', 'H05A', 'H05B', 'J01A', 'J01B', 'J01C', 'J01D', 'J01E', 'J01F', 'J01G', 'J01M', 'J01R', 'J01X', 'J02A', 'J04A', 'J04B', 'J05A', 'J06A', 'J06B', 'J07A', 'J07B', 'J07C', 'J07X', 'L01A', 'L01B', 'L01C', 'L01D', 'L01X', 'L02A', 'L02B', 'L03A', 'L04A', 'M01A', 'M01B', 'M01C', 'M02A', 'M03A', 'M03B', 'M03C', 'M04A', 'M05B', 'M09A', 'N01A', 'N01B', 'N02A', 'N02B', 'N02C', 'N03A', 'N04A', 'N04B', 'N05A', 'N05B', 'N05C', 'N06A', 'N06B', 'N06C', 'N06D', 'N07A', 'N07B', 'N07C', 'N07X', 'P01A', 'P01B', 'P01C', 'P02B', 'P02C', 'P02D', 'P03A', 'P03B', 'R01A', 'R01B', 'R02A', 'R03A', 'R03B', 'R03C', 'R03D', 'R05C', 'R05D', 'R05F', 'R05X', 'R06A', 'R07A', 'S01A', 'S01B', 'S01C', 'S01E', 'S01F', 'S01G', 'S01H', 'S01J', 'S01K', 'S01L', 'S01X', 'S02A', 'S02B', 'S02C', 'S02D', 'S03A', 'S03B', 'S03C', 'S03D', 'V01A', 'V03A', 'V04B', 'V04C', 'V06A', 'V06B', 'V06C', 'V06D', 'V07A', 'V08A', 'V08B', 'V08C', 'V08D', 'V09A', 'V09B', 'V09C', 'V09D', 'V09E', 'V09F', 'V09G', 'V09H', 'V09I', 'V09X', 'V10A', 'V10B', 'V10X')

# randomly repeat IDs up to 7 times
patvec <- character()
for(i in seq_along(id)){
  patvec <- c(patvec,rep(id[i],sample(0:7,1)))
}

atc_data <- data.frame(id=patvec,
                       atc=sample(atc3_codes,length(patvec),replace=TRUE))

head(atc_data)
```

Now these data can be converted to columns of binary variables for specific ATC codes by using `retrieve()`. As an example we will randomly sample 13 level-3 ATC codes.

```{r}

codes_of_interest <- sample(atc3_codes,13)

medi <- retrieve(atc_data,
                 charVar=atc,
                 idVar=id,
                 pattern=codes_of_interest)

head(medi)
```

## Merging several data.frames at once
By using the `consolidate()` function, multiple data.frames can be merged at once. It is equivalent to applying the base function `merge(x,y,by=byvar,all.x=TRUE)` k-1 times, where k is the number of data.frames. By default, the function assumes that it should merge on the column names that are present in all data.frames to be merged (usually just one 'id' variable), but `byvar` can alternatively be set with `by` in the `consolidate` function. Hence, `consolidate()` requires that each of the data.frames to be merged have at least one column with the same name. First, let's create a set of data.frames to merge. 

```{r}
pheno <- data.frame(id=id,
                    age=rnorm(100,65,5.8),
                    male=rbinom(100,1,0.5),
                    bmi=rnorm(100,33,5),
                    cancer=rbinom(100,1,0.15),
                    afib=rbinom(100,1,0.25),
                    ckd=rbinom(100,1,0.35),
                    chf=rbinom(100,1,0.20),
                    copd=rbinom(100,1,0.3))

genotype <- data.frame(cbind(id,matrix(sample(2,5e6,replace=TRUE),nrow=100,ncol=5e4)))
colnames(genotype) <- c('id',paste0('gene_',seq(1,5e4)))

protein <- data.frame(cbind(id,matrix(rnorm(2e5,0,0.35),nrow=100,ncol=2e3)))
colnames(protein) <- c('id',paste0('protein_',seq(1,2e3)))
```

Now let's consolidate these data.frames into one larger data.frame.

```{r}
highdim_df <- consolidate(pheno,medi,genotype,protein)
dim(highdim_df)
```

By chaining consolidate with a `%where%` operator, subsets across several data.frames can quickly be created.

```{r}

phenmed_oldfemale <- consolidate(pheno,medi) %where% (age>60 & copd==1)
head(phenmed_oldfemale)
```

## Grouping variables within data.frames
The `group` function can be used to group variables within an existing data.frame. This creates an S3 object of class `grouped.df`, which is essentially a named list of data.frames created from the larger data.frame within which the grouping structure was imagined. The elements of `grouped.df` objects are of class `element.df` and `data.frame`. The `grouped.df` class has several properties that facilitates working with groups of variables. For instance, the data can be summarized in a succinct way by using `summary`. Of course, different group mappings can be imagined for one dataset. You may want to create several `grouped.df` objects for a single data.frame, depending on the type of descriptive summary or analysis that is needed.

```{r}
# variable names, by group
phenoVars <- colnames(pheno)[-1]
mediVars <- colnames(medi)[-1]
genoVars <- colnames(genotype)[-1]
proteinVars <- colnames(protein)[-1]

g <- group(data=highdim_df,
           groups=list(phenotype=phenoVars,
                       medication=mediVars,
                       genotype=genoVars,
                       proteome=proteinVars))

summary(g)

g2 <- group(highdim_df,
            groups=list(panel1=proteinVars[1:500],
                        panel2=proteinVars[501:1000],
                        panel3=proteinVars[1001:1500],
                        panel4=proteinVars[1501:2000]))
summary(g2)
  
```

A simple visual summary of grouped.dfs can be produced with `plot`.

```{r, fig.width=6, fig.height=3}
plot(g)
plot(g2)
```

## Consolidating variable groups
Grouped.dfs with the same underlying data can also be merged using `consolidate()`.

```{r, fig.width=6, fig.height=3}
g3 <- consolidate(g,g2)
summary(g3)
plot(g3)
```

## Applying transformations to all variables within a group at once
You may want to apply the same transformations to several variables at once. For instance, you might want to convert all variables of a certain group to `numeric`, or transform several dates in a similar way. The `gapply` function can be used for this purpose. This function is the analogous to the base R `apply` family of functions, with the g in gapply referring to the `grouped.df` class. To use it, you specify the `grouped.df`, the relevant variable group, and the function that is to be applied. 

```{r}
# before
head(sapply(g$proteome,class))
g$proteome <- gapply(g,'proteome',as.numeric)
# after
head(sapply(g$proteome,class))

# before
head(sapply(g$genotype,class))
g$genotype <- gapply(g,'genotype',factor)
# after
head(sapply(g$genotype,class))
```

## Composing new data.frames using variable groups
Variable group names in `grouped.df` objects can be used to easily create subsets from the original data.frame. The `compose` function allows you to pass just the name of the `grouped.df` object and the variable group names to obtain a new data.frame containing just those groups. If the `names` argument is set to `TRUE`, the relevant group name will be prefixed to the names of the individual variables. By default this is set to `FALSE`.

```{r}
clin <- compose(g, groups=c('phenotype','medication'))
head(clin)
```

Alternatively, the `+` operator can also be used on objects of class `element.df` to achieve the same more quickly.

```{r}
clin <- g$phenotype + g$medication
head(clin)

panels_1_and_2 <- g2$panel1 + g2$panel2
head(panels_1_and_2[,1:10])
```

If you want to use the `%where%` operator while composing subsets based on groups, parentheses are necessary. Without parentheses, the subset command would only be performed on the rightmost group. 

```{r}
clin_young <- (g$phenotype + g$medication) %where% (age<50)
head(clin_young)
```

## Creating a long formula using group-based notation
For certain functions you will need `formula` notation. Creating long formulae is tedious -- using group-based notation can be useful here. A long formula can easily be constructed using the `grouped_formula` function. This function takes the name of the `grouped.df` and a formula incorporating variable groups instead of single variables, and returns a complete formula with all variable names written out. Single variables can still also be included.    

```{r}
grouped_formula(g,outcome~phenotype+medication)
grouped_formula(g,outcome~phenotype+medication+protein_555+gene_2450)
```

This function also has a `scale` argument, which applies scale to all independent variables of class numeric in the formula. This only works for variables that are part of variable groups in the named group.df.

```{r}
g$medication <- gapply(g,'medication',as.character)
grouped_formula(g,outcome~phenotype+medication, scale=TRUE)
```

## Example applications of grouped formulae
Let's say you want to quickly generate an overview of patient characteristics, stratified by sex. The `table1::table1()` function accepts a formula as first argument, and data.frame as second argument. In combination with `grouped_formula()` this becomes trivial to do.

```{r}
# use character version of g$phenotype 
bt <- gapply(g,'phenotype',as.character)
# transform only age and BMI to numeric
bt[c('age','bmi')] <- sapply(bt[c('age','bmi')],as.numeric)
form <- grouped_formula(g, male~phenotype|male)
table1::table1(form,bt)
```

Fitting a regression model is an alternative application for `grouped_formula`. For this we will need to have an outcome as well. We will plot the resulting estimates using the `forplo` package. Let's pretend the outcome is a continuous biomarker for systemic inflammation. 

```{r, fig.width=7}
regdat <- g$phenotype + gapply(g,'medication',as.numeric)
outcome <- (0.3 * scale(regdat$age)) + 
           (0.6 * regdat$cancer) + 
           (0.2 * regdat$ckd) +
           (0.3 * regdat$copd) - 
           (0.7 * regdat$A02B) +
           rnorm(nrow(regdat),0,0.2)

form <- grouped_formula(g, outcome~phenotype+A02B+L02B, scale=TRUE)
linmod <- lm(form,regdat)
forplo::forplo(linmod, sort=TRUE)
```

In some cases, you may be interested in showing just one estimate, and how it is changed by sequentially adding additional covariates to the model. For this we can create an additional grouping structure with `group`.

```{r, fig.width=7}
g4 <- group(regdat,
            list(var_of_interest='copd',
                 covars1=c('age','male'),
                 covars2=c('cancer','ckd','chf'),
                 covars3=c('A02B','L02B','C08E')))

form1 <- grouped_formula(g4, outcome~var_of_interest, scale=TRUE)
form2 <- grouped_formula(g4, outcome~var_of_interest + covars1, scale=TRUE)
form3 <- grouped_formula(g4, outcome~var_of_interest + covars1 + covars2, scale=TRUE)
form4 <- grouped_formula(g4, outcome~var_of_interest + covars1 + covars2 + covars3, scale=TRUE)

# fit models
crude <- lm(form1,regdat)
mod2 <- lm(form2,regdat)
mod3 <- lm(form3,regdat)
mod4 <- lm(form4,regdat)

# store estimate + confidence intervals for the relevant estimate
results <- c(coef(crude)[2], confint(crude)[2,])
results <- rbind(results, c(coef(mod2)[2], confint(mod2)[2,]))
results <- rbind(results, c(coef(mod3)[2], confint(mod3)[2,]))
results <- rbind(results, c(coef(mod4)[2], confint(mod4)[2,]))

# plot results
forplo::forplo(results,
               linreg=TRUE,
               title='COPD and inflammation',
               row.labels=c('Crude',
                            '+ age, sex',
                            '+ clinical profile',
                            '+ medication'),
               groups=c(1,2,2,2),
               grouplabs=c('Crude estimate',
                           'Adjusted estimates'))

```

## Showing all group mappings for a given data.frame
If several `grouped.df` objects have been created for a single original data.frame, you may want to get an overview of all groupings that were created for this data.frame. This can be done using the function `mappings()`. This function invisibly returns an object of classes `mappings` and `list`. 

```{r}
mappings(highdim_df)
```

