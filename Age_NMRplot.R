#Code below will run a NMR with age as a covariate and the create bubble plots for all possible comparisons

# Author: J. Mcleod
rm(list=ls())

library (multinma)
library(dplyr)
library(ggplot2)
library(tidyr)

options(mc.cores= parallel::detectCores())

#Univariate Network Meta-Regression with mean age as covariate####
#Read in Contrast-Based Data
Data <- read.csv('TBR_Hypertrophy_ArmLevelContrastData_MultipleImputationDataSet_MultiNMA_Oct2022.csv',
                 header=TRUE)
View(Data)
str(Data)
head(Data)

#If running and excluding outlier studies, then run the code below
#Data3 <- Data[!(Data$study =="Rodriguez-Lopez 2022" & Data$treatment== "HM2" |
 #                Data$study =="Barcelos 2015" & Data$treatment== "LS2" |
  #              Data$study=="Tracy 2004" | Data$study =="Charette 1991"),]



#Your data should be in contrast-based format. Make sure you've accounted for multi-arm trials
Net <- set_agd_contrast(data = Data #change this depending on if you have changed the list of studies to include
                        ,study = study,
                                        trt = treatment,
                                        y = diff, 
                                        se = std.err,
                                        sample_size = sample.size)
#Summarize the network and plot the NMA
Net
plot(Net)

#Random Effects NMA with Duration as a continuous covariate
NMA_Regression <- nma(Net,trt_effects = "random", regression = ~(Age_Mean):.trt,
prior_intercept = normal(0, 100),
prior_trt = normal(scale = 100),
prior_het = half_normal (scale = 5),
prior_reg = normal(scale = 100),
warmup = 4000,
iter = 10000,
thin = 10)

NMA_Regression

# #View the residual deviance, and DIC. 
# NMA_Regression_dic <- dic(NMA_Regression)
# NMA_Regression_dic

# #View Heterogeneity
# summary(NMA_Regression, pars = "tau")
# 
# 
# #Extract Individual Study contributions to Residual Deviance, Leverage, and DIC for regression
# NMA_Regression_Residual <- 
#   as.data.frame(NMA_Regression_dic[["pointwise"]][["agd_contrast"]])
# file <- "NMA_Regression_Residual Deviance, Leverage, and DIC.csv"
# write.csv(NMA_Regression_Residual, file = file) #can use this data to construct leverage plots
# 
# #plot the beta distributions for the covariate
# plot(NMA_Regression, 
#      pars = "beta", 
#      ref_line = 0, 
#      stat = "halfeye")

#produces estimates of relative effects at specified covariate intervals
# #Duration range
# Duration_range <- range(Data$Duration)
# 
# #produce relative effects vs CTRL
# Regression_Releff_CTRL <- relative_effects(NMA_Regression,
#                                            newdata = tibble::tibble(Duration = seq(6, 52, by = 1),
#                                                                     label = paste0(Duration, " weeks")),
#                                            study = label)
# 
# plot(Regression_Releff_CTRL,
#      ref_line = 0)
# 
# Regression_Releff_CTRL <- 
#   as.data.frame(Regression_Releff_CTRL)
# file <- "NMA_Regression_RelativeEffectsVsCTRL.csv"
# write.csv(Regression_Releff_CTRL, file = file)

#produce all relative effects
Regression_Releff_all <- relative_effects(NMA_Regression,
                                          all_contrasts = TRUE,
                                          newdata = tibble::tibble(Age_Mean = seq(20, 80, by = 20),
                                                                   label = paste0(Age_Mean, " years old")),
                                          study = label)

plot(Regression_Releff_all,
     ref_line = 0)

Regression_Releff_all <- 
  as.data.frame(Regression_Releff_all)
# file <- "NMA_Regression_AllRelativeEffects.csv"
# write.csv(Regression_Releff_all, file = file)


#Overlay regression line from NMA with effect sizes for each treatment comparison####
#Read in Pairwise Data - Filter studies if excluded from regression
PairwiseData <- read.csv('TBR_Hypertrophy_Pairwise_EffectSizes_Oct2022_WithCovariates_ForBubblePlotOnly.csv',
                         header=TRUE)

#Pull out all evidence with treatment comparisons
ListOfComparisons <- unique(PairwiseData$Treatment_Comparison) #24 unique comparisons with direct evidence

#view each plot individually in R, before coercing to the TIFF print function below
# ####HS3 vs HM3: Comparison 1310 ####
# Regression_Releff_all %>%
#   filter(grepl('HS3 vs. HM3', parameter)) -> Regression_HS3vsHM3
# 
# #Comparison 1310
# PairwiseData_HS3vsHM3 <- filter(PairwiseData, Treatment_Comparison == "1310")
# 
# age_dat <- tibble(Age_Mean= seq (20, 80, by = 20))
# 
# Regression_HS3vsHM3 %>%
#   bind_cols(age_dat) -> Regression_HS3vsHM3
# 
# #create plot
# plot(0,0, xlim=c(20,80), ylim=c(-2.0,2.0), type="n", axes=F, xlab="Mean Age (Years)", ylab="SMD", main="k.) HS3 vs HM3", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
# box("figure", lwd=2)
# 
# axis(2,at=c(-2.0,-1.5,-1.0,-0.5,0,0.5,1.0,1.5,2.0), c(-2.0,-1.5,-1.0,-0.5,0,0.5,1.0,1.5,2.0), pos=6, cex=1.5)
# axis(1,at=c(seq(20,80,by=20)), pos=-2.0, cex=1.5)
# 
# lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
# lines(x=age_dat$Age_Mean, y=Regression_HS3vsHM3$mean,lty=4, lwd=4, col=1)
# lines(x=age_dat$Age_Mean, y=Regression_HS3vsHM3$`2.5%`,lty=4, lwd=2, col=1)
# lines(x=age_dat$Age_Mean, y=Regression_HS3vsHM3$`97.5%`,lty=4, lwd=2, col=1)
# points(x=PairwiseData_HS3vsHM3$Duration,y=PairwiseData_HS3vsHM3$es,lty=4, lwd=4, col=1)


#Write off entire bubble plot
tiff(file="TBR_Hypertrophy_Age_BubblePlot.tiff", width=8862, height=4453, units="px", res=600, compression="lzw")

##par(mfrow=c(1,3))
par(mfrow=c(4,6), mar=c(3,3,3,3), mgp=c(1.5,0.5,0))

# a) LS2 vs CTRL: Comparison 31----
Regression_Releff_all %>%
  filter(grepl('LS2 vs. CTRL', parameter)) -> Regression_LS2vsCTRL

#Comparison 31
PairwiseData_LS2vsCTRL <- filter(PairwiseData, Treatment_Comparison == "31")

age_dat <- tibble(Age_Mean = seq (20, 80, by = 20))

Regression_LS2vsCTRL %>%
  bind_cols(age_dat) -> Regression_LS2vsCTRL

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-1.0,2.5), type="n", axes=F, xlab="Age (years)", ylab="SMD", main="LS2 vs CTRL", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(-1.0,-0.5,0,0.5,1.0,1.5,2.0,2.5), c(-1.0,-0.5,0,0.5,1.0,1.5,2.0,2.5), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-1.0, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS2vsCTRL$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS2vsCTRL$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS2vsCTRL$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_LS2vsCTRL$Age_Mean,y=PairwiseData_LS2vsCTRL$es,lty=4, lwd=4, col=1)

# b) LS3 vs CTRL: Comparison 41 ------
Regression_Releff_all %>%
  filter(grepl('LS3 vs. CTRL', parameter)) -> Regression_LS3vsCTRL

#Comparison 41
PairwiseData_LS3vsCTRL <- filter(PairwiseData, Treatment_Comparison == "41")

age_dat <- tibble(Age_Mean = seq (20, 80, by = 20))

Regression_LS3vsCTRL %>%
  bind_cols(age_dat) -> Regression_LS3vsCTRL

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-0.5,1), type="n", axes=F, xlab="Age (years)", ylab="SMD", main="LS3 vs CTRL", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(-0.5,0,0.5,1.0), c(-0.5,0,0.5,1.0), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-0.5, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS3vsCTRL$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS3vsCTRL$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS3vsCTRL$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_LS3vsCTRL$Age_Mean,y=PairwiseData_LS3vsCTRL$es,lty=4, lwd=4, col=1)

# c) LM2 vs CTRL: Comparison 61 ----
Regression_Releff_all %>%
  filter(grepl('LM2 vs. CTRL', parameter)) -> Regression_LM2vsCTRL

#Comparison 61
PairwiseData_LM2vsCTRL <- filter(PairwiseData, Treatment_Comparison == "61")

age_dat <- tibble(Age_Mean = seq (18, 95, by = 20))

Regression_LM2vsCTRL %>%
  bind_cols(age_dat) -> Regression_LM2vsCTRL

#create plot
plot(0,0, xlim=c(18,95), ylim=c(-0.5,2.0), type="n", axes=F, xlab="Age (years)", ylab="SMD", main="LM2 vs CTRL", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(-0.5,0,0.5,1.0,2.0), c(-0.5,0,0.5,1.0,2.0), pos=18, cex=1.5)
axis(1,at=c(seq(18,95,by=20)), pos=-0.5, cex=1.5)

lines(x=c(seq(18,95,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM2vsCTRL$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM2vsCTRL$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM2vsCTRL$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_LM2vsCTRL$Age_Mean,y=PairwiseData_LM2vsCTRL$es,lty=4, lwd=4, col=1)
# d) LM3 vs CTRL: Comparison 71 ----
Regression_Releff_all %>%
  filter(grepl('LM3 vs. CTRL', parameter)) -> Regression_LM3vsCTRL

#Comparison 71
PairwiseData_LM3vsCTRL <- filter(PairwiseData, Treatment_Comparison == "71")

age_dat <- tibble(Age_Mean = seq (20, 80, by = 20))

Regression_LM3vsCTRL %>%
  bind_cols(age_dat) -> Regression_LM3vsCTRL

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-0.5,1), type="n", axes=F, xlab="Age (years)", ylab="SMD", main="LM3 vs CTRL", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(-0.5,0,0.5,1.0), c(-0.5,0,0.5,1.0), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-0.5, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM3vsCTRL$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM3vsCTRL$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM3vsCTRL$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_LM3vsCTRL$Age_Mean,y=PairwiseData_LM3vsCTRL$es,lty=4, lwd=4, col=1)

# e) HS3 vs CTRL: Comparison 101 ----
Regression_Releff_all %>%
  filter(grepl('HS3 vs. CTRL', parameter)) -> Regression_HS3vsCTRL

#Comparison 101
PairwiseData_HS3vsCTRL <- filter(PairwiseData, Treatment_Comparison == "101")

age_dat <- tibble(Age_Mean= seq (20, 80, by = 20))

Regression_HS3vsCTRL %>%
  bind_cols(age_dat) -> Regression_HS3vsCTRL

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-0.5,1), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="HS3 vs CTRL", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(-0.5,0,0.5,1.0), c(-0.5,0,0.5,1.0), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-0.5, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HS3vsCTRL$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HS3vsCTRL$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HS3vsCTRL$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_HS3vsCTRL$Age_Mean,y=PairwiseData_HS3vsCTRL$es,lty=4, lwd=4, col=1)

# f) HM1 vs CTRL: Comparison 111 ----
Regression_Releff_all %>%
  filter(grepl('HM1 vs. CTRL', parameter)) -> Regression_HM1vsCTRL

#Comparison 111
PairwiseData_HM1vsCTRL <- filter(PairwiseData, Treatment_Comparison == "111")

age_dat <- tibble(Age_Mean= seq (20, 80, by = 20))

Regression_HM1vsCTRL %>%
  bind_cols(age_dat) -> Regression_HM1vsCTRL

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-10,10), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="HM1 vs CTRL", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(seq(-10,10, by=5)), c(seq(-10,10, by=5)), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-10.0, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HM1vsCTRL$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HM1vsCTRL$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HM1vsCTRL$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_HM1vsCTRL$Age_Mean,y=PairwiseData_HM1vsCTRL$es,lty=4, lwd=4, col=1)

# g) HM2 vs CTRL: Comparison 121----
Regression_Releff_all %>%
  filter(grepl('HM2 vs. CTRL', parameter)) -> Regression_HM2vsCTRL

#Comparison 121
PairwiseData_HM2vsCTRL <- filter(PairwiseData, Treatment_Comparison == "121")  
age_dat <- tibble(Age_Mean = seq (18, 80, by = 18))

Regression_HM2vsCTRL %>%
  bind_cols(age_dat) -> Regression_HM2vsCTRL

#create plot
PairwiseData_HM2vsCTRL <- filter(PairwiseData, Treatment_Comparison == "121")

plot(0,0, xlim=c(18,80), ylim=c(-0.5,2.0), type="n", axes=F, xlab="Age (years)", ylab="SMD", main="HM2 vs CTRL", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(-0.5,0,0.5,1.0,1.5,2.0), c(-0.5,0,0.5,1.0,1.5,2.0), pos=18, cex=1.5)
axis(1,at=c(seq(18,80,by=18)), pos=-0.5, cex=1.5)

lines(x=c(seq(18,80,by=18)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HM2vsCTRL$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HM2vsCTRL$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HM2vsCTRL$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_HM2vsCTRL$Age_Mean,y=PairwiseData_HM2vsCTRL$es,lty=4, lwd=4, col=1)

# h) HM3 vs CTRL: Comparison 131----
Regression_Releff_all %>%
  filter(grepl('HM3 vs. CTRL', parameter)) -> Regression_HM3vsCTRL

#Comparison 131
PairwiseData_HM3vsCTRL <- filter(PairwiseData, Treatment_Comparison == "131")

age_dat <- tibble(Age_Mean= seq (20, 80, by = 20))

Regression_HM3vsCTRL %>%
  bind_cols(age_dat) -> Regression_HM3vsCTRL

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-0.5,1.5), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="HM3 vs CTRL", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(-0.5,0,0.5,1.0,1.5), c(-0.5,0,0.5,1.0,1.5), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-0.5, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HM3vsCTRL$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HM3vsCTRL$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HM3vsCTRL$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_HM3vsCTRL$Age_Mean,y=PairwiseData_HM3vsCTRL$es,lty=4, lwd=4, col=1)

# I) LS3 vs LS2: Comparison 43 ####
Regression_Releff_all %>%
  filter(grepl('LS3 vs. LS2', parameter)) -> Regression_LS3vsLS2

#Comparison 43
PairwiseData_LS3vsLS2 <- filter(PairwiseData, Treatment_Comparison == "43")

age_dat <- tibble(Age_Mean= seq (20, 80, by = 20))

Regression_LS3vsLS2 %>%
  bind_cols(age_dat) -> Regression_LS3vsLS2

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-2.0,1.5), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="LS3 vs LS2", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(-2.0,-1.5,-1.0,-0.5,0,0.5,1.0,1.5), c(-2.0,-1.5,-1.0,-0.5,0,0.5,1.0,1.5), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-2.0, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS3vsLS2$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS3vsLS2$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS3vsLS2$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_LS3vsLS2$Age_Mean,y=PairwiseData_LS3vsLS2$es,lty=4, lwd=4, col=1)

# J) LS2 vs LM2: Comparison 63 ----
Regression_Releff_all %>%
  filter(grepl('LS2 vs. LM2', parameter)) -> Regression_LS2vsLM2

#Comparison 63
PairwiseData_LS2vsLM2 <- filter(PairwiseData, Treatment_Comparison == "63")

age_dat <- tibble(Age_Mean= seq (20, 80, by = 20))

Regression_LS2vsLM2 %>%
  bind_cols(age_dat) -> Regression_LS2vsLM2

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-1.0,1.5), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="LS2 vs LM2", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(-1.0,-0.5,0,0.5,1.0,1.5), c(-1.0,-0.5,0,0.5,1.0,1.5), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-1.0, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS2vsLM2$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS2vsLM2$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS2vsLM2$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_LS2vsLM2$Age_Mean,y=PairwiseData_LS2vsLM2$es,lty=4, lwd=4, col=1)

# K) LM2 vs LM1: Comparison 65 ####
Regression_Releff_all %>%
  filter(grepl('LM2 vs. LM1', parameter)) -> Regression_LM2vsLM1

#Comparison 65
PairwiseData_LM2vsLM1 <- filter(PairwiseData, Treatment_Comparison == "65")

age_dat <- tibble(Age_Mean= seq (20, 80, by = 20))

Regression_LM2vsLM1 %>%
  bind_cols(age_dat) -> Regression_LM2vsLM1

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-10,10), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="LM2 vs LM1", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(seq(-10,10, by=5)), c(seq(-10,10, by=5)), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-10, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM2vsLM1$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM2vsLM1$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM2vsLM1$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_LM2vsLM1$Age_Mean,y=PairwiseData_LM2vsLM1$es,lty=4, lwd=4, col=1)

# L) LS3 vs LM3: Comparison 74 ####
Regression_Releff_all %>%
  filter(grepl('LS3 vs. LM3', parameter)) -> Regression_LS3vsLM3

#Comparison 74
PairwiseData_LS3vsLM3 <- filter(PairwiseData, Treatment_Comparison == "74")

age_dat <- tibble(Age_Mean= seq (20, 80, by = 20))

Regression_LS3vsLM3 %>%
  bind_cols(age_dat) -> Regression_LS3vsLM3

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-1.0,1.0), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="LS3 vs LM3", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(-1.0,-0.5,0,0.5,1.0), c(-1.0,-0.5,0,0.5,1.0), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-1.0, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS3vsLM3$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS3vsLM3$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS3vsLM3$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_LS3vsLM3$Age_Mean,y=PairwiseData_LS3vsLM3$es,lty=4, lwd=4, col=1)

# M) LM3 vs LM1: Comparison 75 ----
Regression_Releff_all %>%
  filter(grepl('LM3 vs. LM1', parameter)) -> Regression_LM3vsLM1

#Comparison 75
PairwiseData_LM3vsLM1 <- filter(PairwiseData, Treatment_Comparison == "75")

age_dat <- tibble(Age_Mean= seq (20, 80, by = 20))

Regression_LM3vsLM1 %>%
  bind_cols(age_dat) -> Regression_LM3vsLM1

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-10,10), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="LM3 vs LM1", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(seq(-10,10, by=5)), c(seq(-10,10, by=5)), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-10, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM3vsLM1$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM3vsLM1$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM3vsLM1$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_LM3vsLM1$Age_Mean,y=PairwiseData_LM3vsLM1$es,lty=4, lwd=4, col=1)

# N) LM3 vs LM2: Comparison 76 ----
Regression_Releff_all %>%
  filter(grepl('LM3 vs. LM2', parameter)) -> Regression_LM3vsLM2

#Comparison 76
PairwiseData_LM3vsLM2 <- filter(PairwiseData, Treatment_Comparison == "76")

age_dat <- tibble(Age_Mean= seq (20, 80, by = 20))

Regression_LM3vsLM2 %>%
  bind_cols(age_dat) -> Regression_LM3vsLM2

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-1.0,1.0), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="LM3 vs LM2", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(seq(-1,1, by=0.5)), c(seq(-1,1, by=0.5)), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-1.0, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM3vsLM2$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM3vsLM2$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM3vsLM2$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_LM3vsLM2$Age_Mean,y=PairwiseData_LM3vsLM2$es,lty=4, lwd=4, col=1)

# O) LS2 vs HS2: Comparison 93 ----
Regression_Releff_all %>%
  filter(grepl('LS2 vs. HS2', parameter)) -> Regression_LS2vsHS2

#Comparison 93
PairwiseData_LS2vsHS2 <- filter(PairwiseData, Treatment_Comparison == "93")

age_dat <- tibble(Age_Mean= seq (20, 80, by = 20))

Regression_LS2vsHS2 %>%
  bind_cols(age_dat) -> Regression_LS2vsHS2

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-10,10), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="LS2 vs HS2", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(seq(-10,10, by=5)), c(seq(-10,10, by=5)), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-10.0, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS2vsHS2$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS2vsHS2$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS2vsHS2$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_LS2vsHS2$Age_Mean,y=PairwiseData_LS2vsHS2$es,lty=4, lwd=4, col=1)
# P) LS3 vs HS3: Comparison 104 ####
Regression_Releff_all %>%
  filter(grepl('LS3 vs. HS3', parameter)) -> Regression_LS3vsHS3

#Comparison 104
PairwiseData_LS3vsHS3 <- filter(PairwiseData, Treatment_Comparison == "104")

age_dat <- tibble(Age_Mean= seq (20, 80, by = 20))

Regression_LS3vsHS3 %>%
  bind_cols(age_dat) -> Regression_LS3vsHS3

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-1.0,1.0), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="LS3 vs HS3", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(seq(-1,1, by=0.5)), c(seq(-1,1, by=0.5)), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-1.0, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS3vsHS3$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS3vsHS3$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS3vsHS3$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_LS3vsHS3$Age_Mean,y=PairwiseData_LS3vsHS3$es,lty=4, lwd=4, col=1)
# Q) LM3 vs HS3: Comparison 107 ####
Regression_Releff_all %>%
  filter(grepl('LM3 vs. HS3', parameter)) -> Regression_LM3vsHS3

#Comparison 107
PairwiseData_LM3vsHS3 <- filter(PairwiseData, Treatment_Comparison == "107")

age_dat <- tibble(Age_Mean= seq (20, 80, by = 20))

Regression_LM3vsHS3 %>%
  bind_cols(age_dat) -> Regression_LM3vsHS3

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-1,1), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="LM3 vs HS3", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(seq(-1,1, by=0.5)), c(seq(-1,1, by=0.5)), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-1.0, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM3vsHS3$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM3vsHS3$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM3vsHS3$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_LM3vsHS3$Age_Mean,y=PairwiseData_LM3vsHS3$es,lty=4, lwd=4, col=1)
# R) LM2 vs HM2: Comparison 126 ----
Regression_Releff_all %>%
  filter(grepl('LM2 vs. HM2', parameter)) -> Regression_LM2vsHM2

#Comparison 126
PairwiseData_LM2vsHM2 <- filter(PairwiseData, Treatment_Comparison == "126")

age_dat <- tibble(Age_Mean= seq (19, 80, by = 20))

Regression_LM2vsHM2 %>%
  bind_cols(age_dat) -> Regression_LM2vsHM2

#create plot
plot(0,0, xlim=c(19,80), ylim=c(-1.0,1.0), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="LM2 vs HM2", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(seq(-1,1, by=0.5)), c(seq(-1,1, by=0.5)), pos=19, cex=1.5)
axis(1,at=c(seq(19,80,by=20)), pos=-1.0, cex=1.5)

lines(x=c(seq(19,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM2vsHM2$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM2vsHM2$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM2vsHM2$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_LM2vsHM2$Age_Mean,y=PairwiseData_LM2vsHM2$es,lty=4, lwd=4, col=1)
# S) HS2 vs HM2: Comparison 129 ----
Regression_Releff_all %>%
  filter(grepl('HS2 vs. HM2', parameter)) -> Regression_HS2vsHM2

#Comparison 129
PairwiseData_HS2vsHM2 <- filter(PairwiseData, Treatment_Comparison == "129")

age_dat <- tibble(Age_Mean= seq (20, 80, by = 20))

Regression_HS2vsHM2 %>%
  bind_cols(age_dat) -> Regression_HS2vsHM2

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-10.0,10.0), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="HS2 vs HM2", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(seq(-10,10, by=5)), c(seq(-10,10, by=5)), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-10.0, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HS2vsHM2$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HS2vsHM2$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HS2vsHM2$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_HS2vsHM2$Age_Mean,y=PairwiseData_HS2vsHM2$es,lty=4, lwd=4, col=1)
# T) LS3 vs HM3: Comparison 134 ----
Regression_Releff_all %>%
  filter(grepl('LS3 vs. HM3', parameter)) -> Regression_LS3vsHM3

#Comparison 134
PairwiseData_LS3vsHM3 <- filter(PairwiseData, Treatment_Comparison == "134")

age_dat <- tibble(Age_Mean= seq (20, 80, by = 20))

Regression_LS3vsHM3 %>%
  bind_cols(age_dat) -> Regression_LS3vsHM3

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-1.0,1.0), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="LS3 vs HM3", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(seq(-1,1, by=0.5)), c(seq(-1,1, by=0.5)), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-1.0, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS3vsHM3$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS3vsHM3$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LS3vsHM3$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_LS3vsHM3$Age_Mean,y=PairwiseData_LS3vsHM3$es,lty=4, lwd=4, col=1)

# U) LM3 vs HM3: Comparison 137 ----
Regression_Releff_all %>%
  filter(grepl('LM3 vs. HM3', parameter)) -> Regression_LM3vsHM3

#Comparison 137
PairwiseData_LM3vsHM3 <- filter(PairwiseData, Treatment_Comparison == "137")

age_dat <- tibble(Age_Mean= seq (20, 80, by = 20))

Regression_LM3vsHM3 %>%
  bind_cols(age_dat) -> Regression_LM3vsHM3

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-1.0,1.0), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="LM3 vs HM3", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(-1.0,-0.5,0,0.5,1.0), c(-1.0,-0.5,0,0.5,1.0), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-1.0, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM3vsHM3$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM3vsHM3$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_LM3vsHM3$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_LM3vsHM3$Age_Mean,y=PairwiseData_LM3vsHM3$es,lty=4, lwd=4, col=1)
# V) HM2 vs HM1: Comparison 1211 ----
Regression_Releff_all %>%
  filter(grepl('HM2 vs. HM1', parameter)) -> Regression_HM2vsHM1

#Comparison 1211
PairwiseData_HM2vsHM1 <- filter(PairwiseData, Treatment_Comparison == "1211")

age_dat <- tibble(Age_Mean= seq (20, 80, by = 20))

Regression_HM2vsHM1 %>%
  bind_cols(age_dat) -> Regression_HM2vsHM1

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-10.0,10.0), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="HM2 vs HM1", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(seq(-10,10, by=5)), c(seq(-10,10, by=5)), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-10.0, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HM2vsHM1$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HM2vsHM1$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HM2vsHM1$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_HM2vsHM1$Age_Mean,y=PairwiseData_HM2vsHM1$es,lty=4, lwd=4, col=1)

# W) HS3 vs HM3: Comparison 1310 ----
Regression_Releff_all %>%
  filter(grepl('HS3 vs. HM3', parameter)) -> Regression_HS3vsHM3

#Comparison 1310
PairwiseData_HS3vsHM3 <- filter(PairwiseData, Treatment_Comparison == "1310")

age_dat <- tibble(Age_Mean = seq(20,80,by=20))

Regression_HS3vsHM3 %>%
  bind_cols(age_dat) -> Regression_HS3vsHM3

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-1,1), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="HS3 vs HM3", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(seq (-1, 1, by = 0.5)), c(seq (-1, 1, by = 0.5)), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-1, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HS3vsHM3$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HS3vsHM3$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HS3vsHM3$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_HS3vsHM3$Age_Mean,y=PairwiseData_HS3vsHM3$es,lty=4, lwd=4, col=1)

# X) HM3 vs HM2: Comparison 1312 ----
Regression_Releff_all %>%
  filter(grepl('HM3 vs. HM2', parameter)) -> Regression_HM3vsHM2
#Comparison 1312
PairwiseData_HM3vsHM2 <- filter(PairwiseData, Treatment_Comparison == "1312")

age_dat <- tibble(Age_Mean = seq (20, 80, by = 20))

Regression_HM3vsHM2 %>%
  bind_cols(age_dat) -> Regression_HM3vsHM2

#create plot
plot(0,0, xlim=c(20,80), ylim=c(-1.0,1.0), type="n", axes=F, xlab="Age (Years)", ylab="SMD", main="HM3 vs HM2", cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
box("figure", lwd=2)

axis(2,at=c(-1.0,-0.5,0,0.5,1.0), c(-1.0,-0.5,0,0.5,1.0), pos=20, cex=1.5)
axis(1,at=c(seq(20,80,by=20)), pos=-1.0, cex=1.5)

lines(x=c(seq(20,80,by=20)), y=c(0,0,0,0), lty=4, lwd=1, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HM3vsHM2$mean,lty=4, lwd=4, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HM3vsHM2$`2.5%`,lty=4, lwd=2, col=1)
lines(x=age_dat$Age_Mean, y=Regression_HM3vsHM2$`97.5%`,lty=4, lwd=2, col=1)
points(x=PairwiseData_HM3vsHM2$Age_Mean,y=PairwiseData_HM3vsHM2$es,lty=4, lwd=4, col=1)

#Print graphs----
dev.off()

# #plot
# ggplot(aes(x = Duration), data = PairwiseData_HM2vsCTRL) +
#   geom_hline(yintercept = 0, colour = "grey60") +
#   geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), data = Regression_HM2vsCTRL,
#               fill = "darkred", alpha = 0.3) +
#   geom_line(aes(y = mean), data = Regression_HM2vsCTRL,
#             colour = "darkred") +
#   geom_point(aes(y = es, size = sample.size), alpha = 1.0) +
#   coord_cartesian(xlim = c(6, 24)) +
#   xlab("Mean Age (Years)") + ylab("SMD")