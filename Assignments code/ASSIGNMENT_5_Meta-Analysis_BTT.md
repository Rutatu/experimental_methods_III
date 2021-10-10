Building on the shoulders of giants: meta-analysis
==================================================

Questions to be answered
------------------------

1.  What is the current evidence for distinctive vocal patterns in
    schizophrenia? Report how many papers report quantitative estimates,
    comment on what percentage of the overall studies reviewed they
    represent (see PRISMA chart) your method to analyze them, the
    estimated effect size of the difference (mean effect size and
    standard error) and forest plots representing it. N.B. Only measures
    of pitch mean and pitch sd are required for the assignment. Feel
    free to ignore the rest (although pause behavior looks interesting,
    if you check my article).

``` r
# load libraries
pacman::p_load(pacman,tidyverse,metafor,lmerTest, dplyr)

# read file into a dataframe
crazyData <- readxl::read_excel("Matrix_MetaAnalysis_Diagnosis_updated290719.xlsx")
```

    ## New names:
    ## * frequency -> frequency...68
    ## * frequency -> frequency...73
    ## * frequency -> frequency...78
    ## * frequency -> frequency...83
    ## * frequency -> frequency...88
    ## * ... and 7 more problems

``` r
# subsetting control data
behavedData <- crazyData %>% select("StudyID",TYPE_OF_TASK,DIAGNOSIS,SAMPLE_SIZE_HC,SAMPLE_SIZE_SZ,PITCH_F0_HC_M,PITCH_F0_HC_SD,PITCH_F0_SZ_M, PITCH_F0_SZ_SD)

# subsetting schizophrenia data
behavedDataSD <- crazyData %>% select("StudyID",TYPE_OF_TASK,DIAGNOSIS,SAMPLE_SIZE_HC,SAMPLE_SIZE_SZ,PITCH_F0SD_HC_M,PITCH_F0SD_HC_SD,PITCH_F0SD_SZ_M,PITCH_F0SD_SZ_SD)

# deleting NA's (which downsamples our dataset for metaanalysis, because studies use different measurements)
behavedData <- na.omit(behavedData)
behavedDataSD <- na.omit(behavedDataSD)

# set variables as a factor, introducing levels
behavedData <- behavedData %>% 
mutate_at(c("TYPE_OF_TASK","StudyID"),as.factor)
```

``` r
# calculating effect size for mean meassures dataframe
behavedData <- metafor::escalc("SMD",
n1i = SAMPLE_SIZE_SZ, n2i = SAMPLE_SIZE_HC,
m1i = PITCH_F0_SZ_M, m2i = PITCH_F0_HC_M,
sd1i = PITCH_F0_SZ_SD, sd2i = PITCH_F0_HC_SD,data=behavedData)

# set and examine factor levels
behavedData$TYPE_OF_TASK <- behavedData$TYPE_OF_TASK %>% as.factor()
behavedData$TYPE_OF_TASK %>% levels()
```

    ## [1] "CONSTR" "FREE"   "SOCIAL"

``` r
# defining a simple model predicting effect size with studies weighted by their squared standard deviation
simpleModel <- lmer(yi ~ 1 + (1|StudyID), behavedData, weights = 1/vi, REML=F,
control = lmerControl(
check.nobs.vs.nlev="ignore",
check.nobs.vs.nRE="ignore"))

# defining a model predicting effect size from type of task with studies weighted by their squared standard deviation
intermediateModel <- lmer(yi ~ 1 + TYPE_OF_TASK + (1|StudyID), behavedData, weights = 1/vi, REML=F,
control = lmerControl(
check.nobs.vs.nlev="ignore",
check.nobs.vs.nRE="ignore"))

# summarize statistical analysis of healthy controls
summary(simpleModel)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: yi ~ 1 + (1 | StudyID)
    ##    Data: behavedData
    ## Weights: 1/vi
    ## Control: 
    ## lmerControl(check.nobs.vs.nlev = "ignore", check.nobs.vs.nRE = "ignore")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##      9.7      9.0     -1.8      3.7        3 
    ## 
    ## Scaled residuals: 
    ##       Min        1Q    Median        3Q       Max 
    ## -0.006661 -0.002930  0.001998  0.003651  0.007026 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.
    ##  StudyID  (Intercept) 0.1078829 0.328455
    ##  Residual             0.0000362 0.006017
    ## Number of obs: 6, groups:  StudyID, 6
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)
    ## (Intercept)   0.2065     0.1341 6.0000    1.54    0.174

``` r
summary(intermediateModel)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: yi ~ 1 + TYPE_OF_TASK + (1 | StudyID)
    ##    Data: behavedData
    ## Weights: 1/vi
    ## Control: 
    ## lmerControl(check.nobs.vs.nlev = "ignore", check.nobs.vs.nRE = "ignore")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##     12.4     11.3     -1.2      2.4        1 
    ## 
    ## Scaled residuals: 
    ##       Min        1Q    Median        3Q       Max 
    ## -0.008045  0.000000  0.001126  0.002397  0.006298 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.
    ##  StudyID  (Intercept) 8.669e-02 0.294440
    ##  Residual             2.654e-05 0.005152
    ## Number of obs: 6, groups:  StudyID, 6
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error       df t value Pr(>|t|)
    ## (Intercept)         0.27677    0.14722  5.99993   1.880    0.109
    ## TYPE_OF_TASKFREE   -0.02659    0.32920  6.00000  -0.081    0.938
    ## TYPE_OF_TASKSOCIAL -0.39503    0.32920  6.00000  -1.200    0.275
    ## 
    ## Correlation of Fixed Effects:
    ##               (Intr) TYPE_OF_TASKF
    ## TYPE_OF_TASKF -0.447              
    ## TYPE_OF_TASKS -0.447  0.200

``` r
# defining a model using the rma (regression meta analysis) function that fits the meta-analytic models via linear mixed effects models
metaModel <- rma(yi, vi, data = behavedData, slab=StudyID)


#adding fixed effects
metaFixedModel <- rma(yi, vi, data = behavedData,mods = cbind(TYPE_OF_TASK), slab=StudyID,weighted=T)



# summarize models
summary(metaModel)
```

    ## 
    ## Random-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ##  -2.1749    4.3497    8.3497    7.5686   14.3497   
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.0712 (SE = 0.0908)
    ## tau (square root of estimated tau^2 value):      0.2668
    ## I^2 (total heterogeneity / total variability):   50.29%
    ## H^2 (total variability / sampling variability):  2.01
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 5) = 9.8472, p-val = 0.0797
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.1628  0.1554  1.0476  0.2948  -0.1417  0.4672    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(metaFixedModel)
```

    ## 
    ## Mixed-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ##  -1.9572    3.9143    9.9143    8.0732   33.9143   
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0924 (SE = 0.1201)
    ## tau (square root of estimated tau^2 value):             0.3040
    ## I^2 (residual heterogeneity / unaccounted variability): 55.34%
    ## H^2 (unaccounted variability / sampling variability):   2.24
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 4) = 9.0272, p-val = 0.0604
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.5154, p-val = 0.4728
    ## 
    ## Model Results:
    ## 
    ##               estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt         0.4067  0.3718   1.0940  0.2740  -0.3220  1.1354    
    ## TYPE_OF_TASK   -0.1507  0.2099  -0.7179  0.4728  -0.5621  0.2607    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# creating forest plots to visualize the standardized mean difference of the studies in the meta analysis

forest(metaModel, main = "Mean Model")
```

![](A5-Meta-Analysis-BTT_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
forest(metaFixedModel, main="Mean Model with Fixed Effect = Task")
```

![](A5-Meta-Analysis-BTT_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
# calculating effect size for standard deviation 
behavedDataSD <- metafor::escalc("SMD",
n1i = SAMPLE_SIZE_SZ, n2i = SAMPLE_SIZE_HC,
m1i = PITCH_F0SD_SZ_M, m2i = PITCH_F0SD_HC_M,
sd1i = PITCH_F0SD_HC_SD, sd2i = PITCH_F0SD_SZ_SD,data=behavedDataSD)

# set and examine factor levels
behavedDataSD$TYPE_OF_TASK <- behavedDataSD$TYPE_OF_TASK %>% as.factor()
behavedDataSD$TYPE_OF_TASK %>% levels()
```

    ## [1] "CONSTR" "FREE"   "SOCIAL"

``` r
# defining a simple model predicting effect size with studies weighted by their squared standard deviation
simpleModelSD <- lmer(yi ~ 1 + (1|StudyID), behavedDataSD, weights = 1/vi, REML=F,
control = lmerControl(
check.nobs.vs.nlev="ignore",
check.nobs.vs.nRE="ignore"))

# defining a model predicting effect size from type of task with studies weighted by their squared standard deviation
intermediateModelSD <- lmer(yi ~ 1 + TYPE_OF_TASK + (1|StudyID), behavedDataSD, weights = 1/vi, REML=F,
control = lmerControl(
check.nobs.vs.nlev="ignore",
check.nobs.vs.nRE="ignore"))

# summarize statistical analysis of healthy controls
summary(simpleModelSD)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: yi ~ 1 + (1 | StudyID)
    ##    Data: behavedDataSD
    ## Weights: 1/vi
    ## Control: 
    ## lmerControl(check.nobs.vs.nlev = "ignore", check.nobs.vs.nRE = "ignore")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##     44.1     46.2    -19.0     38.1       12 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.22670 -0.06686 -0.02009  0.09364  0.98817 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  StudyID  (Intercept) 1.4660   1.2108  
    ##  Residual             0.4061   0.6372  
    ## Number of obs: 15, groups:  StudyID, 12
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)
    ## (Intercept)  -0.1913     0.3532 11.6913  -0.542    0.598

``` r
summary(intermediateModelSD)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: yi ~ 1 + TYPE_OF_TASK + (1 | StudyID)
    ##    Data: behavedDataSD
    ## Weights: 1/vi
    ## Control: 
    ## lmerControl(check.nobs.vs.nlev = "ignore", check.nobs.vs.nRE = "ignore")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##     37.1     40.6    -13.5     27.1       10 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.31719 -0.06473  0.01124  0.02547  1.04957 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  StudyID  (Intercept) 0.98863  0.9943  
    ##  Residual             0.05375  0.2318  
    ## Number of obs: 15, groups:  StudyID, 12
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error       df t value Pr(>|t|)  
    ## (Intercept)        -0.55888    0.37980 12.35347  -1.472   0.1662  
    ## TYPE_OF_TASKFREE    1.19188    0.58552 12.12624   2.036   0.0642 .
    ## TYPE_OF_TASKSOCIAL -0.43762    0.09974  3.04713  -4.388   0.0212 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##               (Intr) TYPE_OF_TASKF
    ## TYPE_OF_TASKF -0.649              
    ## TYPE_OF_TASKS -0.130  0.084

``` r
# defining a model using the regression meta analysis function 
metaModelSD <- rma(yi, vi, data = behavedDataSD, slab=StudyID)

#adding Fixed effects
metaFixedModelSD <- rma(yi, vi, data = behavedDataSD,mods = cbind(TYPE_OF_TASK), slab=StudyID,weighted=T)


# summarize models
summary(metaModelSD)
```

    ## 
    ## Random-Effects Model (k = 15; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ## -22.3246   44.6493   48.6493   49.9274   49.7402   
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 1.2986 (SE = 0.5206)
    ## tau (square root of estimated tau^2 value):      1.1395
    ## I^2 (total heterogeneity / total variability):   95.37%
    ## H^2 (total variability / sampling variability):  21.58
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 14) = 174.2428, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se     zval    pval    ci.lb   ci.ub 
    ##  -0.2495  0.3031  -0.8233  0.4103  -0.8435  0.3445    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(metaFixedModelSD)
```

    ## 
    ## Mixed-Effects Model (k = 15; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ## -20.9607   41.9215   47.9215   49.6163   50.5882   
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     1.3517 (SE = 0.5610)
    ## tau (square root of estimated tau^2 value):             1.1626
    ## I^2 (residual heterogeneity / unaccounted variability): 95.57%
    ## H^2 (unaccounted variability / sampling variability):   22.56
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 13) = 169.6524, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.5388, p-val = 0.4629
    ## 
    ## Model Results:
    ## 
    ##               estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt         0.3711  0.9001   0.4124  0.6801  -1.3929  2.1352    
    ## TYPE_OF_TASK   -0.3108  0.4234  -0.7340  0.4629  -1.1405  0.5190    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# creating a forest plot to visualize the standardized mean difference of the studies in the meta analysis

forest(metaModelSD, main = "SD Model")
```

![](A5-Meta-Analysis-BTT_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
forest(metaFixedModelSD, main = "SD Model, Fixed effect = Task")
```

![](A5-Meta-Analysis-BTT_files/figure-markdown_github/unnamed-chunk-3-2.png)

1.  Do the results match your own analysis from Assignment 3? If you add
    your results to the meta-analysis, do the estimated effect sizes
    change? Report the new estimates and the new forest plots.

``` r
#### Create separate calculations for our own study assignment 3 ####
# Read data from previous study into data frame
ownData <- read.csv("assignment3.csv")

# Subset dataframe by selecting diagnosis, mean, sd, and subject id.
ownData <- ownData %>% select(Diagnosis, mean, sd, Subject)

# Convert Diagnosis from levels to characters
ownData$Diagnosis <- as.character(ownData$Diagnosis)



# Create a dataframe of means including our study assignment
#SZ
# create schizophrenia data frame
szData <- filter(ownData, Diagnosis == "Schizophrenia")

# calculating number of participants
szNumber <- n_distinct(szData$Subject)
# n = 59

# mean pitch and mean of mean pitch of schizophrenic participants
szMeanVec <- szData %>% group_by(Subject) %>%
  summarize(mean=mean(mean))
szMeanMean <- mean(szMeanVec$mean)

# standard deviation of the mean pitch
szSDMean <- sd(szMeanVec$mean)

# HC
# create a healthy control data frame
hcData <- filter(ownData, Diagnosis == "Control")

 # calculate number of participants
hcNumber <- n_distinct(hcData$Subject)
# n = 59

# find mean pitch and mean of mean pitch of healthy controls
hcMeanVec <- hcData %>% group_by(Subject) %>%
  summarize(mean=mean(mean))
hcMeanMean <- mean(hcMeanVec$mean)

# standard deviation of the mean pitch
hcSDMean <- sd(hcMeanVec$mean)

# create dataframe
ourOwnData <- data.frame(StudyID = "4", TYPE_OF_TASK = "FREE", DIAGNOSIS = "SZ", SAMPLE_SIZE_HC = hcNumber, SAMPLE_SIZE_SZ = szNumber, PITCH_F0_HC_M = hcMeanMean, PITCH_F0_HC_SD = hcSDMean, PITCH_F0_SZ_M = szMeanMean, PITCH_F0_SZ_SD = szSDMean,yi=NA,vi=NA)

# binding data frame into one with previous data
mergedData <- rbind.data.frame(behavedData, ourOwnData)

# creating effectsize and measures of uncertainty
mergedData  <- metafor::escalc("SMD",
n1i = SAMPLE_SIZE_SZ, n2i = SAMPLE_SIZE_HC,
m1i = PITCH_F0_SZ_M, m2i = PITCH_F0_HC_M,
sd1i = PITCH_F0_SZ_SD, sd2i = PITCH_F0_HC_SD,data=mergedData )


# create a model using effect size and an uncertainty measure
metaModelOwn <- rma(yi, vi, data = mergedData, slab=StudyID)

#adding fixed effects
metaFixedModelOwn <- rma(yi, vi, data = mergedData,cbind("TYPE_OF_TASK"),weighted=T, slab=StudyID)


#looking at models
summary(metaModelOwn)
```

    ## 
    ## Random-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ##  -2.1231    4.2461    8.2461    7.8296   12.2461   
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.0487 (SE = 0.0645)
    ## tau (square root of estimated tau^2 value):      0.2206
    ## I^2 (total heterogeneity / total variability):   44.11%
    ## H^2 (total variability / sampling variability):  1.79
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 6) = 10.4977, p-val = 0.1052
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.1150  0.1274  0.9022  0.3669  -0.1348  0.3647    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(metaFixedModelOwn)
```

    ## 
    ## Random-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ##  -2.1231    4.2461    8.2461    7.8296   12.2461   
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.0487 (SE = 0.0645)
    ## tau (square root of estimated tau^2 value):      0.2206
    ## I^2 (total heterogeneity / total variability):   44.11%
    ## H^2 (total variability / sampling variability):  1.79
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 6) = 10.4977, p-val = 0.1052
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.1150  0.1274  0.9022  0.3669  -0.1348  0.3647    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# create forest plots displaying effectsizes
forest(metaModelOwn, main= "Mean Model")
```

![](A5-Meta-Analysis-BTT_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
forest(metaFixedModelOwn, main = "Mean Model, Fixed effect = Task")
```

![](A5-Meta-Analysis-BTT_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
# create a data frame of SD's including our study assignment
#SZ
# mean pitch and mean of mean pitch of schizophrenic participants
szSDVec <- szData %>% group_by(Subject) %>%
  summarize(meanSD=mean(sd)) 

# mean and sd of standard deviation vector
szMeanSD <- mean(szSDVec$meanSD)
szSDSD <- sd(szSDVec$meanSD)

# HC
# find mean pitch and mean of mean pitch of healthy controls
hcSDVec <- hcData %>% group_by(Subject) %>%
  summarize(meanSD=mean(sd))

# mean and sd of standard deviation vector
hcMeanSD <- mean(hcSDVec$meanSD)
hcSDSD <- sd(hcSDVec$meanSD)

# create row in dataframe following earlier example
ourOwnDataSD <- data.frame(StudyID = "4", TYPE_OF_TASK = "FREE", DIAGNOSIS = "SZ", SAMPLE_SIZE_HC = hcNumber, SAMPLE_SIZE_SZ = szNumber, PITCH_F0SD_HC_M = hcMeanSD, PITCH_F0SD_HC_SD = hcSDSD, PITCH_F0SD_SZ_M = szMeanSD, PITCH_F0SD_SZ_SD = szSDSD,yi=NA,vi=NA)


# binding data frame into one with previous data
mergedDataSD <- rbind.data.frame(behavedDataSD, ourOwnDataSD)

# creating effectsize and measures of uncertainty
mergedDataSD <- metafor::escalc("SMD",
n1i = SAMPLE_SIZE_SZ, n2i = SAMPLE_SIZE_HC,
m1i = PITCH_F0SD_SZ_M, m2i = PITCH_F0SD_HC_M,
sd1i = PITCH_F0SD_HC_SD, sd2i = PITCH_F0SD_SZ_SD,data=mergedDataSD)


# create a model using effect size and an uncertainty measure
metaModelOwnSD <- rma(yi, vi, data = mergedDataSD, slab=StudyID)

#addign fixed effects
metaFixedModelOwnSD <- rma(yi, vi, data = mergedDataSD,mods = cbind(TYPE_OF_TASK), slab=StudyID,weighted=T)

# summarize models
summary(metaModelOwnSD)
```

    ## 
    ## Random-Effects Model (k = 16; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ## -23.3688   46.7377   50.7377   52.1538   51.7377   
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 1.1979 (SE = 0.4651)
    ## tau (square root of estimated tau^2 value):      1.0945
    ## I^2 (total heterogeneity / total variability):   95.24%
    ## H^2 (total variability / sampling variability):  21.01
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 15) = 174.2515, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se     zval    pval    ci.lb   ci.ub 
    ##  -0.2475  0.2822  -0.8771  0.3804  -0.8006  0.3056    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(metaFixedModelOwnSD)
```

    ## 
    ## Mixed-Effects Model (k = 16; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ## -22.0231   44.0463   50.0463   51.9634   52.4463   
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     1.2403 (SE = 0.4973)
    ## tau (square root of estimated tau^2 value):             1.1137
    ## I^2 (residual heterogeneity / unaccounted variability): 95.44%
    ## H^2 (unaccounted variability / sampling variability):   21.91
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 14) = 169.6673, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.5815, p-val = 0.4457
    ## 
    ## Model Results:
    ## 
    ##               estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt         0.3716  0.8611   0.4316  0.6660  -1.3160  2.0593    
    ## TYPE_OF_TASK   -0.3100  0.4066  -0.7625  0.4457  -1.1069  0.4869    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# create forest plots displaying effectsizes
forest(metaModelOwnSD, main = "SD Model")
```

![](A5-Meta-Analysis-BTT_files/figure-markdown_github/unnamed-chunk-4-3.png)

``` r
forest(metaFixedModelOwnSD, main = "SD Model, Fixed effect = Task")
```

![](A5-Meta-Analysis-BTT_files/figure-markdown_github/unnamed-chunk-4-4.png)

1.  Assess the quality of the literature: report and comment on
    heterogeneity of the studies (tau, I2), on publication bias (funnel
    plot), and on influential studies.

``` r
# create two funnel plots in order to investigate publication bias
funnel(metaModel, main = "Mean Model")
```

![](A5-Meta-Analysis-BTT_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
funnel(metaModelSD, main = "SD Model")
```

![](A5-Meta-Analysis-BTT_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
# fixed effects
funnel(metaFixedModel, main = "Mean Model, Fixed Effect = Task")
```

![](A5-Meta-Analysis-BTT_files/figure-markdown_github/unnamed-chunk-5-3.png)

``` r
funnel(metaFixedModelSD, main = "SD Model, Fixed Effect = Task")
```

![](A5-Meta-Analysis-BTT_files/figure-markdown_github/unnamed-chunk-5-4.png)

``` r
# testing for influential studies using influence plots
# meta model with means
influence <- influence(metaModel)
plot(influence)
```

![](A5-Meta-Analysis-BTT_files/figure-markdown_github/unnamed-chunk-5-5.png)

``` r
# meta model with sdandard deviation
influenceSD <- influence(metaModelSD)
plot(influenceSD)
```

![](A5-Meta-Analysis-BTT_files/figure-markdown_github/unnamed-chunk-5-6.png)

``` r
#fixed effects
influenceFixed <- influence(metaFixedModel)
plot(influenceFixed)
```

![](A5-Meta-Analysis-BTT_files/figure-markdown_github/unnamed-chunk-5-7.png)

``` r
# meta model with sdandard deviation
influenceSDFixed <- influence(metaFixedModelSD)
plot(influenceSDFixed)
```

![](A5-Meta-Analysis-BTT_files/figure-markdown_github/unnamed-chunk-5-8.png)

``` r
# Extra tests
# funnel
regtest(metaModel) # test for funnel plot asymmetry: z = 1.5798, p = 0.1142
```

    ## 
    ## Regression Test for Funnel Plot Asymmetry
    ## 
    ## model:     mixed-effects meta-regression model
    ## predictor: standard error
    ## 
    ## test for funnel plot asymmetry: z = 1.5798, p = 0.1142

``` r
regtest(metaModelSD) # test for funnel plot asymmetry: z = 0.2004, p = 0.8412
```

    ## 
    ## Regression Test for Funnel Plot Asymmetry
    ## 
    ## model:     mixed-effects meta-regression model
    ## predictor: standard error
    ## 
    ## test for funnel plot asymmetry: z = 0.2004, p = 0.8412

``` r
# kendall's tau
ranktest(metaModel) # k tau = 0.20, p = 0.72
```

    ## 
    ## Rank Correlation Test for Funnel Plot Asymmetry
    ## 
    ## Kendall's tau = 0.2000, p = 0.7194

``` r
ranktest(metaModelSD) # k tau = -0.28, p = 0.69
```

    ## 
    ## Rank Correlation Test for Funnel Plot Asymmetry
    ## 
    ## Kendall's tau = -0.2762, p = 0.1686

``` r
# tau 2
metaModel$tau2 # 0.071
```

    ## [1] 0.07116138

``` r
metaModelSD$tau2 # 1.30
```

    ## [1] 1.29856

``` r
# calculate I square from the two meta models
metaModel$I2
```

    ## [1] 50.2908

``` r
# 50.29
metaModelSD$I2
```

    ## [1] 95.36627

``` r
# 95.37


# Extra tests
# funnel
regtest(metaFixedModel) # test for funnel plot asymmetry: z = 1.1308, p = 0.2581
```

    ## 
    ## Regression Test for Funnel Plot Asymmetry
    ## 
    ## model:     mixed-effects meta-regression model
    ## predictor: standard error
    ## 
    ## test for funnel plot asymmetry: z = 1.1308, p = 0.2581

``` r
regtest(metaFixedModelSD) # test for funnel plot asymmetry: z = 0.3027, p = 0.7621
```

    ## 
    ## Regression Test for Funnel Plot Asymmetry
    ## 
    ## model:     mixed-effects meta-regression model
    ## predictor: standard error
    ## 
    ## test for funnel plot asymmetry: z = 0.3027, p = 0.7621

``` r
# kendall's tau
ranktest(metaFixedModel) #Kendall's tau = 0.2000, p = 0.7194
```

    ## 
    ## Rank Correlation Test for Funnel Plot Asymmetry
    ## 
    ## Kendall's tau = 0.2000, p = 0.7194

``` r
ranktest(metaFixedModelSD) #Kendall's tau = -0.2762, p = 0.1686
```

    ## 
    ## Rank Correlation Test for Funnel Plot Asymmetry
    ## 
    ## Kendall's tau = -0.2762, p = 0.1686

``` r
# tau^2
metaFixedModel$tau2 # 0.09239298
```

    ## [1] 0.09239298

``` r
metaFixedModelSD$tau2 #1.35172
```

    ## [1] 1.35172

``` r
# calculate I square from the two meta models
metaFixedModel$I2
```

    ## [1] 55.33512

``` r
# 55.33512
metaFixedModelSD$I2
```

    ## [1] 95.56817

``` r
# 95.56817
```

Tips on the process to follow:
------------------------------

-   Download the data on all published articles analyzing voice in
    schizophrenia and the prisma chart as reference of all articles
    found and reviewed

-   Look through the dataset to find out which columns to use, and if
    there is any additional information written as comments (real world
    data is always messy!).

    -   Hint: PITCH\_F0M and PITCH\_F0SD group of variables are what you
        need
    -   Hint: Make sure you read the comments in the columns:
        `pitch_f0_variability`, `frequency`, `Title`,
        `ACOUST_ANA_DESCR`, `DESCRIPTION`, and frma`COMMENTS`

-   Following the procedure in the slides calculate effect size and
    standard error of the effect size per each study. N.B. we focus on
    pitch mean and pitch standard deviation. . first try using lmer (to
    connect to what you know of mixed effects models) . then use rma()
    (to get some juicy additional statistics)

-   Build a forest plot of the results (forest(model))

-   Go back to Assignment 3, add your own study to the data table, and
    re-run meta-analysis. Do the results change?

-   Now look at the output of rma() and check tau and I2
