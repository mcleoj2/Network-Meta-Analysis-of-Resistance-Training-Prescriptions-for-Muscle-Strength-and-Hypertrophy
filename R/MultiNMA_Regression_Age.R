#This script will run a network meta regression with mean age as a covariate

#Author: J.Mcleod
rm(list=ls())

library (multinma)
library(tidyverse)

options(mc.cores= parallel::detectCores())

#Univariate Network Meta-Regression with mean age as covariate####
#Read in Contrast-Based Data
Data <- read.csv('TBR_Hypertrophy_ArmLevelContrastData_MultipleImputationDataSet_MultiNMA_Oct2022.csv',
                 header=TRUE)


#If running and excluding outlier studies and weak nodes, then run the code below
#Data3 <- Data[!(Data$treatment=="HM1" | Data$study =="Rodriguez-Lopez 2022" & Data$treatment== "HM2" | 
 #                 Data$study=="Tracy 2004" | Data$study =="Charette 1991" | 
  #                Data$study =="Stefanaki 2019" | Data$study =="Sooneste 2013"),]

#Your data should be in contrast-based format. Make sure you've accounted for multi-arm trials
Net <- 
set_agd_contrast(
  data = Data, #change this depending on if you have changed the list of studies to include
  study = study,
  trt = treatment,
  y = diff, 
  se = std.err,
  sample_size = sample.size
  )

#Random Effects NMA with Duration as a continuous covariate
NMA_Regression <- 
nma(
  Net,
  trt_effects = "random", regression = ~(Age_Mean):.trt,
  prior_intercept = normal(0, 100),
  prior_trt = normal(scale = 100),
  prior_het = half_normal (scale = 5),
  prior_reg = normal(scale = 100),
  warmup = 4000,
  iter = 10000,
  thin = 10
  )

#View the residual deviance, and DIC. 
NMA_Regression_dic <- dic(NMA_Regression)

#View Heterogeneity
summary(NMA_Regression, pars = "tau")


#Extract Individual Study contributions to Residual Deviance, Leverage, and DIC for regression
NMA_Regression_Residual <- 
  as.data.frame(NMA_Regression_dic[["pointwise"]][["agd_contrast"]])
file <- "NMA_Regression_Residual Deviance, Leverage, and DIC.csv"
write.csv(NMA_Regression_Residual, file = file) #can use this data to construct leverage plots

#plot the beta distributions for the covariate
plot(NMA_Regression, 
     pars = "beta", 
     ref_line = 0, 
     stat = "halfeye")

#produces estimates of relative effects at specified covariate intervals
#View the age range
Age_Range <- range(Data$Age_Mean)

#Produce all relative effects
Regression_Releff_all <- relative_effects(NMA_Regression,
                                      all_contrasts = TRUE,
                                      newdata = tibble::tibble(Age_Mean = seq(20, 80, by = 20),
                                                               label = paste0(Age_Mean, " years old")),
                                      study = label)
Regression_Releff_all

plot(Regression_Releff_all,
     ref_line = 0)

Regression_Releff_all <- 
  as.data.frame(Regression_Releff_all)
file <- "NMA_Regression_AllRelativeEffects.csv"
write.csv(Regression_Releff_all, file = file)