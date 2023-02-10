simplify
================
<vincent10kd@gmail.com>
2021-04-06

The **simplify** package simplifies data preprocessing steps, especially for high-dimensional datasets. It introduces grouped data.frames (the `group.df` class), lower dimensional representations of data.frames with many columns. Being able to think in terms of variable groups, which can each represent hundreds or thousands of variables, reduces the complexity of the dataset. This makes working with high-dimensional data easier and more dynamic. Moreover, **simplify** contains functions to simplify several common data preprocessing steps, such as merging several data.frames, subsetting, and applying functions to subgroups.

## The %where% operator for simple subsetting

Subsetting is an essential step in data preprocessing. The **simplify** package introduces the `%where%` operator to reduce the mental bandwidth necessary to create subsets. Its syntax requires a data.frame or matrix on the left side of the operator, and the logical condition on the right side, enclosed by parentheses. The operator is a wrapper for the `base::subset` function. To illustrate, we will use the `iris` dataset.

``` r
# iris
dim(iris)
#> [1] 150   5
head(iris)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
#> 4          4.6         3.1          1.5         0.2  setosa
#> 5          5.0         3.6          1.4         0.2  setosa
#> 6          5.4         3.9          1.7         0.4  setosa

i1 <- iris %where% (Species=='virginica' & Sepal.Length>5)
head(i1)
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
#> 101          6.3         3.3          6.0         2.5 virginica
#> 102          5.8         2.7          5.1         1.9 virginica
#> 103          7.1         3.0          5.9         2.1 virginica
#> 104          6.3         2.9          5.6         1.8 virginica
#> 105          6.5         3.0          5.8         2.2 virginica
#> 106          7.6         3.0          6.6         2.1 virginica
i2 <- iris %where% (Sepal.Width<3)
head(i2)
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 9           4.4         2.9          1.4         0.2     setosa
#> 42          4.5         2.3          1.3         0.3     setosa
#> 54          5.5         2.3          4.0         1.3 versicolor
#> 55          6.5         2.8          4.6         1.5 versicolor
#> 56          5.7         2.8          4.5         1.3 versicolor
#> 58          4.9         2.4          3.3         1.0 versicolor
rm(i1, i2)
```

## The %by% operator to apply functions by subgroup

Similarly, the **simplify** package features the `%by%` operator, which facilitates running commands by subgroups. The function is a wrapper for `stats::aggregate`, and only works when the function takes a numeric argument, and the subgroup is of classes `character`, `factor`, `numeric` or `integer`. The subgroup should be a vector of equal length to the argument to the function. Some examples are shown below.

``` r
mean(iris$Sepal.Length) %by% iris$Species
#>     subgroup  mean
#> 1     setosa 5.006
#> 2 versicolor 5.936
#> 3  virginica 6.588
summary(iris$Petal.Width) %by% iris$Species
#>     subgroup summary.Min. summary.1st Qu. summary.Median summary.Mean
#> 1     setosa        0.100           0.200          0.200        0.246
#> 2 versicolor        1.000           1.200          1.300        1.326
#> 3  virginica        1.400           1.800          2.000        2.026
#>   summary.3rd Qu. summary.Max.
#> 1           0.300        0.600
#> 2           1.500        1.800
#> 3           2.300        2.500
petal_quantiles <- cut(iris$Petal.Length,quantile(iris$Petal.Length))
median(iris$Sepal.Length) %by% petal_quantiles 
#>     subgroup median
#> 1    (1,1.6]    5.0
#> 2 (1.6,4.35]    5.6
#> 3 (4.35,5.1]    6.1
#> 4  (5.1,6.9]    6.7
```

## Converting long character data into binary columns

Sometimes data is stored in long, 'free-text' format, and cannot be used without first transforming it to match the wide format of your main dataset. Think, for instance, of medication data that is stored by Anatomical Therapeutic Chemical (ATC) code, where there are several rows per patient ID corresponding to as many drugs taken at a given time point. Another common situation is that patient diagnoses may be stored in a long form character format, e.g. a single variable capturing all ICD-10 diagnoses across several rows per ID. In these situations, it is useful to be able to ad hoc define ATC codes or ICD-10 codes of interest, and determine whether each patient ID has a 'hit' for these codes or not. For this, the `retrieve()` function can be used. The `retrieve` function should be passed the name of the original data.frame or matrix, the name of the variable storing the information of interest, the name of the 'ID' variable by which the data should be summarized, and one or several regular expressions (`pattern`) to define the patterns of interest. An example is given below. First, let's generate an example dataset containing several ATC codes per individual.

``` r
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
#>       id  atc
#> 1 pat_28 L03A
#> 2 pat_28 A13A
#> 3 pat_80 R03D
#> 4 pat_80 S02B
#> 5  pat_9 C01C
#> 6  pat_9 N01B
```

Now these data can be converted to columns of binary variables for specific ATC codes by using `retrieve()`. As an example we will randomly sample 13 level-3 ATC codes.

``` r

codes_of_interest <- sample(atc3_codes,13)

medi <- retrieve(atc_data,
                 charVar=atc,
                 idVar=id,
                 pattern=codes_of_interest)

head(medi)
#>        id L02B N07X J01X A10A C07E A02B R03C L03A C08E G02B A03B D09A P01B
#> 1   pat_1    0    0    0    0    0    0    0    0    0    0    0    0    0
#> 2  pat_10    0    0    0    0    0    0    0    0    0    0    0    0    0
#> 3 pat_100    0    0    0    0    0    0    0    0    0    0    0    0    0
#> 4  pat_11    1    0    0    0    0    1    0    0    0    0    0    0    0
#> 5  pat_13    0    0    0    0    0    0    0    0    0    0    0    0    0
#> 6  pat_14    0    0    0    0    0    0    0    0    0    0    0    0    0
```

## Merging several data.frames at once

By using the `consolidate()` function, multiple data.frames can be merged at once. It is equivalent to applying the base function `merge(x,y,by=byvar,all.x=TRUE)` k-1 times, where k is the number of data.frames. By default, the function assumes that it should merge on the column names that are present in all data.frames to be merged (usually just one 'id' variable), but `byvar` can alternatively be set with `by` in the `consolidate` function. Hence, `consolidate()` requires that each of the data.frames to be merged have at least one column with the same name. First, let's create a set of data.frames to merge.

``` r
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

``` r
highdim_df <- consolidate(pheno,medi,genotype,protein)
dim(highdim_df)
#> [1]   100 52022
```

By chaining consolidate with a `%where%` operator, subsets across several data.frames can quickly be created.

``` r

phenmed_oldfemale <- consolidate(pheno,medi) %where% (age>60 & copd==1)
head(phenmed_oldfemale)
#>        id      age male      bmi cancer afib ckd chf copd L02B N07X J01X A10A
#> 1   pat_1 60.47613    0 29.89619      0    0   1   1    1    0    0    0    0
#> 4  pat_11 65.26758    1 25.86721      0    0   0   0    1    1    0    0    0
#> 5  pat_12 66.66705    1 37.40019      0    0   0   0    1   NA   NA   NA   NA
#> 15 pat_21 72.59331    0 26.93927      0    1   0   0    1    0    0    0    0
#> 17 pat_23 65.62702    0 33.70034      0    0   0   0    1    0    0    0    0
#> 18 pat_24 64.37928    1 31.16415      0    0   1   0    1    0    0    0    0
#>    C07E A02B R03C L03A C08E G02B A03B D09A P01B
#> 1     0    0    0    0    0    0    0    0    0
#> 4     0    1    0    0    0    0    0    0    0
#> 5    NA   NA   NA   NA   NA   NA   NA   NA   NA
#> 15    0    0    0    0    1    0    0    0    0
#> 17    0    0    0    0    0    0    0    0    0
#> 18    0    0    0    0    0    0    0    0    0
```

## Grouping variables within data.frames

The `group` function can be used to group variables within an existing data.frame. This creates an S3 object of class `grouped.df`, which is essentially a named list of data.frames created from the larger data.frame within which the grouping structure was imagined. The elements of `grouped.df` objects are of class `element.df` and `data.frame`. The `grouped.df` class has several properties that facilitates working with groups of variables. For instance, the data can be summarized in a succinct way by using `summary`. Of course, different group mappings can be imagined for one dataset. You may want to create several `grouped.df` objects for a single data.frame, depending on the type of descriptive summary or analysis that is needed.

``` r
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
#> grouped.df with 4 groups created.

summary(g)
#> g is a grouped.df containing 4 groups:
#>  phenotype, medication, genotype, proteome 
#> ===============================
#> phenotype comprises 8 variables of class(es) numeric, integer 
#> medication comprises 13 variables of class(es) numeric 
#> genotype comprises 50000 variables of class(es) character 
#> proteome comprises 2000 variables of class(es) character

g2 <- group(highdim_df,
            groups=list(panel1=proteinVars[1:500],
                        panel2=proteinVars[501:1000],
                        panel3=proteinVars[1001:1500],
                        panel4=proteinVars[1501:2000]))
#> grouped.df with 4 groups created.
summary(g2)
#> g2 is a grouped.df containing 4 groups:
#>  panel1, panel2, panel3, panel4 
#> ===============================
#> panel1 comprises 500 variables of class(es) character 
#> panel2 comprises 500 variables of class(es) character 
#> panel3 comprises 500 variables of class(es) character 
#> panel4 comprises 500 variables of class(es) character
```

A simple visual summary of grouped.dfs can be produced with `plot`.

``` r
plot(g)
```

![](simplifyHD_vignette_md_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
plot(g2)
```

![](simplifyHD_vignette_md_files/figure-markdown_github/unnamed-chunk-9-2.png)

## Consolidating variable groups

Grouped.dfs with the same underlying data can also be merged using `consolidate()`.

``` r
g3 <- consolidate(g,g2)
summary(g3)
#> g3 is a grouped.df containing 8 groups:
#>  phenotype, medication, genotype, proteome, panel1, panel2, panel3, panel4 
#> ===============================
#> phenotype comprises 8 variables of class(es) numeric, integer 
#> medication comprises 13 variables of class(es) numeric 
#> genotype comprises 50000 variables of class(es) character 
#> proteome comprises 2000 variables of class(es) character 
#> panel1 comprises 500 variables of class(es) character 
#> panel2 comprises 500 variables of class(es) character 
#> panel3 comprises 500 variables of class(es) character 
#> panel4 comprises 500 variables of class(es) character
plot(g3)
```

![](simplifyHD_vignette_md_files/figure-markdown_github/unnamed-chunk-10-1.png)

## Applying transformations to all variables within a group at once

You may want to apply the same transformations to several variables at once. For instance, you might want to convert all variables of a certain group to `numeric`, or transform several dates in a similar way. The `gapply` function can be used for this purpose. This function is the analogous to the base R `apply` family of functions, with the g in gapply referring to the `grouped.df` class. To use it, you specify the `grouped.df`, the relevant variable group, and the function that is to be applied.

``` r
# before
head(sapply(g$proteome,class))
#>   protein_1   protein_2   protein_3   protein_4   protein_5   protein_6 
#> "character" "character" "character" "character" "character" "character"
g$proteome <- gapply(g,'proteome',as.numeric)
# after
head(sapply(g$proteome,class))
#> protein_1 protein_2 protein_3 protein_4 protein_5 protein_6 
#> "numeric" "numeric" "numeric" "numeric" "numeric" "numeric"

# before
head(sapply(g$genotype,class))
#>      gene_1      gene_2      gene_3      gene_4      gene_5      gene_6 
#> "character" "character" "character" "character" "character" "character"
g$genotype <- gapply(g,'genotype',factor)
# after
head(sapply(g$genotype,class))
#>   gene_1   gene_2   gene_3   gene_4   gene_5   gene_6 
#> "factor" "factor" "factor" "factor" "factor" "factor"
```

## Composing new data.frames using variable groups

Variable group names in `grouped.df` objects can be used to easily create subsets from the original data.frame. The `compose` function allows you to pass just the name of the `grouped.df` object and the variable group names to obtain a new data.frame containing just those groups. If the `names` argument is set to `TRUE`, the relevant group name will be prefixed to the names of the individual variables. By default this is set to `FALSE`.

``` r
clin <- compose(g, groups=c('phenotype','medication'))
head(clin)
#>        age male      bmi cancer afib ckd chf copd L02B N07X J01X A10A C07E A02B
#> 1 60.47613    0 29.89619      0    0   1   1    1    0    0    0    0    0    0
#> 2 48.38679    0 28.33350      0    0   0   0    0    0    0    0    0    0    0
#> 3 59.46906    0 37.31601      0    0   0   0    0    0    0    0    0    0    0
#> 4 65.26758    1 25.86721      0    0   0   0    1    1    0    0    0    0    1
#> 5 66.66705    1 37.40019      0    0   0   0    1   NA   NA   NA   NA   NA   NA
#> 6 68.11760    0 31.47832      1    0   0   0    0    0    0    0    0    0    0
#>   R03C L03A C08E G02B A03B D09A P01B
#> 1    0    0    0    0    0    0    0
#> 2    0    0    0    0    0    0    0
#> 3    0    0    0    0    0    0    0
#> 4    0    0    0    0    0    0    0
#> 5   NA   NA   NA   NA   NA   NA   NA
#> 6    0    0    0    0    0    0    0
```

Alternatively, the `+` operator can also be used on objects of class `element.df` to achieve the same more quickly.

``` r
clin <- g$phenotype + g$medication
head(clin)
#>        age male      bmi cancer afib ckd chf copd L02B N07X J01X A10A C07E A02B
#> 1 60.47613    0 29.89619      0    0   1   1    1    0    0    0    0    0    0
#> 2 48.38679    0 28.33350      0    0   0   0    0    0    0    0    0    0    0
#> 3 59.46906    0 37.31601      0    0   0   0    0    0    0    0    0    0    0
#> 4 65.26758    1 25.86721      0    0   0   0    1    1    0    0    0    0    1
#> 5 66.66705    1 37.40019      0    0   0   0    1   NA   NA   NA   NA   NA   NA
#> 6 68.11760    0 31.47832      1    0   0   0    0    0    0    0    0    0    0
#>   R03C L03A C08E G02B A03B D09A P01B
#> 1    0    0    0    0    0    0    0
#> 2    0    0    0    0    0    0    0
#> 3    0    0    0    0    0    0    0
#> 4    0    0    0    0    0    0    0
#> 5   NA   NA   NA   NA   NA   NA   NA
#> 6    0    0    0    0    0    0    0

panels_1_and_2 <- g2$panel1 + g2$panel2
head(panels_1_and_2[,1:10])
#>            protein_1          protein_2           protein_3          protein_4
#> 1 0.0814799714458665 -0.535271840694766   0.329944020463369  0.154548218353402
#> 2   0.20811112719061 -0.090223775667288 -0.0540489918411367  -0.42655619196785
#> 3  0.588879804046545 -0.151993925942915   0.289998604721531  0.142244917950693
#> 4 -0.170383156790551  0.718336766898188   0.199011776545635   0.15458155511649
#> 5 -0.306971231744767  0.130443072099664   0.272413885056997 0.0632302262480106
#> 6  0.273399658149463 -0.592028343165428   -0.21101268595017 -0.640692201011266
#>            protein_5            protein_6           protein_7
#> 1 -0.425434090827647   0.0286912315192129 -0.0721352917769525
#> 2 -0.306972334280287  -0.0791378910920376  -0.325533752199294
#> 3  0.299871931630149   -0.674321096817853   0.382770051974496
#> 4  0.148434862585141   -0.399693384673132  -0.287476742815269
#> 5  0.428431528452827 -0.00328080778382108 -0.0774792806601226
#> 6  0.229932824086722    0.518276747674479 0.00153867639031499
#>              protein_8           protein_9            protein_10
#> 1  -0.0392527521208741   -0.24622826515604    -0.202103882557328
#> 2   -0.135501138924854   0.293158443214172     0.258890770665324
#> 3   -0.105334281389375 -0.0142844623875511      0.42583093788044
#> 4   -0.269565838083027   0.343279900445675    -0.203700286836566
#> 5 -0.00585429647072851  -0.673231625017415 -0.000680011507464816
#> 6    0.773655659794311   0.505944214398052      0.29404516813554
```

If you want to use the `%where%` operator while composing subsets based on groups, parentheses are necessary. Without parentheses, the subset command would only be performed on the rightmost group.

``` r
clin_young <- (g$phenotype + g$medication) %where% (age<50)
head(clin_young)
#>         age male      bmi cancer afib ckd chf copd L02B N07X J01X A10A C07E
#> 2  48.38679    0 28.33350      0    0   0   0    0    0    0    0    0    0
#> 60 49.31562    1 27.14899      0    0   0   1    0    0    0    0    0    0
#>    A02B R03C L03A C08E G02B A03B D09A P01B
#> 2     0    0    0    0    0    0    0    0
#> 60    0    0    0    0    0    0    0    0
```

## Creating a long formula using group-based notation

For certain functions you will need `formula` notation. Creating long formulae is tedious -- using group-based notation can be useful here. A long formula can easily be constructed using the `grouped_formula` function. This function takes the name of the `grouped.df` and a formula incorporating variable groups instead of single variables, and returns a complete formula with all variable names written out. Single variables can still also be included.

``` r
grouped_formula(g,outcome~phenotype+medication)
#> outcome ~ age + male + bmi + cancer + afib + ckd + chf + copd + 
#>     L02B + N07X + J01X + A10A + C07E + A02B + R03C + L03A + C08E + 
#>     G02B + A03B + D09A + P01B
#> <environment: 0x7fdfc9bca100>
grouped_formula(g,outcome~phenotype+medication+protein_555+gene_2450)
#> outcome ~ age + male + bmi + cancer + afib + ckd + chf + copd + 
#>     L02B + N07X + J01X + A10A + C07E + A02B + R03C + L03A + C08E + 
#>     G02B + A03B + D09A + P01B + protein_555 + gene_2450
#> <environment: 0x7fdfbea54448>
```

This function also has a `scale` argument, which applies scale to all independent variables of class numeric in the formula. This only works for variables that are part of variable groups in the named group.df.

``` r
g$medication <- gapply(g,'medication',as.character)
grouped_formula(g,outcome~phenotype+medication, scale=TRUE)
#> outcome ~ scale(age) + male + scale(bmi) + cancer + afib + ckd + 
#>     chf + copd + L02B + N07X + J01X + A10A + C07E + A02B + R03C + 
#>     L03A + C08E + G02B + A03B + D09A + P01B
#> <environment: 0x7fdfc5ac5968>
```

## Example applications of grouped formulae

Let's say you want to quickly generate an overview of patient characteristics, stratified by sex. The `table1::table1()` function accepts a formula as first argument, and data.frame as second argument. In combination with `grouped_formula()` this becomes trivial to do.

``` r
# use character version of g$phenotype 
bt <- gapply(g,'phenotype',as.character)
# transform only age and BMI to numeric
bt[c('age','bmi')] <- sapply(bt[c('age','bmi')],as.numeric)
form <- grouped_formula(g, male~phenotype|male)
table1::table1(form,bt)
#> [1] "<table class=\"Rtable1\">\n<thead>\n<tr>\n<th class='rowlabel firstrow lastrow'></th>\n<th class='firstrow lastrow'><span class='stratlabel'>0<br><span class='stratn'>(N=44)</span></span></th>\n<th class='firstrow lastrow'><span class='stratlabel'>1<br><span class='stratn'>(N=56)</span></span></th>\n<th class='firstrow lastrow'><span class='stratlabel'>Overall<br><span class='stratn'>(N=100)</span></span></th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>male</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>0</td>\n<td>44 (100%)</td>\n<td>0 (0%)</td>\n<td>44 (44.0%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>1</td>\n<td class='lastrow'>0 (0%)</td>\n<td class='lastrow'>56 (100%)</td>\n<td class='lastrow'>56 (56.0%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>age</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>63.3 (4.94)</td>\n<td>64.8 (5.62)</td>\n<td>64.2 (5.35)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>64.0 [48.4, 72.6]</td>\n<td class='lastrow'>64.9 [49.3, 75.3]</td>\n<td class='lastrow'>64.6 [48.4, 75.3]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>bmi</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>33.7 (4.93)</td>\n<td>32.0 (4.78)</td>\n<td>32.8 (4.90)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>33.3 [22.2, 43.8]</td>\n<td class='lastrow'>31.9 [19.7, 44.7]</td>\n<td class='lastrow'>32.5 [19.7, 44.7]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>cancer</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>0</td>\n<td>40 (90.9%)</td>\n<td>49 (87.5%)</td>\n<td>89 (89.0%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>1</td>\n<td class='lastrow'>4 (9.1%)</td>\n<td class='lastrow'>7 (12.5%)</td>\n<td class='lastrow'>11 (11.0%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>afib</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>0</td>\n<td>33 (75.0%)</td>\n<td>47 (83.9%)</td>\n<td>80 (80.0%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>1</td>\n<td class='lastrow'>11 (25.0%)</td>\n<td class='lastrow'>9 (16.1%)</td>\n<td class='lastrow'>20 (20.0%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>ckd</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>0</td>\n<td>30 (68.2%)</td>\n<td>37 (66.1%)</td>\n<td>67 (67.0%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>1</td>\n<td class='lastrow'>14 (31.8%)</td>\n<td class='lastrow'>19 (33.9%)</td>\n<td class='lastrow'>33 (33.0%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>chf</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>0</td>\n<td>40 (90.9%)</td>\n<td>44 (78.6%)</td>\n<td>84 (84.0%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>1</td>\n<td class='lastrow'>4 (9.1%)</td>\n<td class='lastrow'>12 (21.4%)</td>\n<td class='lastrow'>16 (16.0%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>copd</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>0</td>\n<td>31 (70.5%)</td>\n<td>36 (64.3%)</td>\n<td>67 (67.0%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>1</td>\n<td class='lastrow'>13 (29.5%)</td>\n<td class='lastrow'>20 (35.7%)</td>\n<td class='lastrow'>33 (33.0%)</td>\n</tr>\n</tbody>\n</table>\n"
```

Fitting a regression model is an alternative application for `grouped_formula`. For this we will need to have an outcome as well. We will plot the resulting estimates using the `forplo` package. Let's pretend the outcome is a continuous biomarker for systemic inflammation.

``` r
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

![](simplifyHD_vignette_md_files/figure-markdown_github/unnamed-chunk-18-1.png)

In some cases, you may be interested in showing just one estimate, and how it is changed by sequentially adding additional covariates to the model. For this we can create an additional grouping structure with `group`.

``` r
g4 <- group(regdat,
            list(var_of_interest='copd',
                 covars1=c('age','male'),
                 covars2=c('cancer','ckd','chf'),
                 covars3=c('A02B','L02B','C08E')))
#> grouped.df with 4 groups created.

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

![](simplifyHD_vignette_md_files/figure-markdown_github/unnamed-chunk-19-1.png)

## Showing all group mappings for a given data.frame

If several `grouped.df` objects have been created for a single original data.frame, you may want to get an overview of all groupings that were created for this data.frame. This can be done using the function `mappings()`. This function invisibly returns an object of classes `mappings` and `list`.

``` r
mappings(highdim_df)
#> The data.frame highdim_df is associated with 3 grouped.df objects. 
#> ===============================
#> Mappings: g, g2, g3 
#> g contains 4 groups.
#> g2 contains 4 groups.
#> g3 contains 8 groups.
```
