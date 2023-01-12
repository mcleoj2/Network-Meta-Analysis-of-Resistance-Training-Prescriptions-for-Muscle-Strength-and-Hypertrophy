#TBR Code used to assess influential cases and potential outliars
#Author: J.Mcleod

#From analysis of pairwise meta analyses, transfer over comparisons that you 
#believe are suspicious

rm(list=ls(all=T))
#If you dont have the following packages below, run install.packages("package name")
library(meta)
library(dplyr)
library(tidyverse)
library(dmetar)
library(gridExtra)

#Read in and view your data, With calculated effect sizes
Data <- read.csv ('TBR_Hypertrophy_Pairwise_EffectSizes_Oct2022.csv', sep=',',
                  header=TRUE)
View(Data)
str(Data)

# LM2 vs CTRL - Comparison 61####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 61)

#View Data
View(Data2)

#Create Pairwise Meta Analysis
Meta <- metagen(TE = es,
                seTE = se,
                studlab = Study,
                data = Data2,
                sm = "SMD",
                method.SMD = "Hedges",
                fixed = FALSE,
                random = TRUE,
                prediction = TRUE,
                method.tau = "REML",
                hakn = TRUE,
                title = "Pairwise Random-Effects Meta Analysis: LM2 VS CTRL")
#Assess heterogeneity
summary(Meta) #Moderate heterogeneity, significant cochrans Q

#Outlier analysis and influential cases

#detect outliers by using the "find.outliers" function
Meta_Outliers <- find.outliers(Meta)
Meta_Outliers
#Outlier studies are detected if their 95% confidence interval lies ouside the 95% confidence interval of the pooled effect.

#view differences in pooled effect 
Meta
Meta_Outliers

#View Influential Cases - Dont have to have large ES, but have a large impact on the ES or the heterogeneity
#generate Baujat plot
Meta_inf <- InfluenceAnalysis(Meta, random = TRUE)

plot(Meta_inf, "baujat")

plot(Meta_inf, "influence")


#Leave one out analysis
plot(Meta_inf, "I2") #plot sorted by heterogeneity 
plot(Meta_inf, "es") #plot sorted by effect sizes 


# LM2 vs LS2 - Comparison 71####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 63)

#View Data
View(Data2)

#Create Pairwise Meta Analysis
Meta <- metagen(TE = es,
                seTE = se,
                studlab = Study,
                data = Data2,
                sm = "SMD",
                method.SMD = "Hedges",
                fixed = FALSE,
                random = TRUE,
                prediction = TRUE,
                method.tau = "REML",
                hakn = TRUE,
                title = "Pairwise Random-Effects Meta Analysis: LM2 VS LS2")
#Assess heterogeneity
summary(Meta) #Moderate heterogeneity, significant cochrans Q

#Outlier analysis and influential cases

#detect outliers by using the "find.outliers" function
Meta_Outliers <- find.outliers(Meta)
Meta_Outliers
#Outlier studies are detected if their 95% confidence interval lies ouside the 95% confidence interval of the pooled effect.

#view differences in pooled effect 
Meta
Meta_Outliers

#View Influential Cases - Dont have to have large ES, but have a large impact on the ES or the heterogeneity
#generate Baujat plot
Meta_inf <- InfluenceAnalysis(Meta, random = TRUE)

plot(Meta_inf, "baujat")

plot(Meta_inf, "influence")


#Leave one out analysis
plot(Meta_inf, "I2") #plot sorted by heterogeneity 
plot(Meta_inf, "es") #plot sorted by effect sizes 

# HM2 vs CTRL - Comparison 121####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 121)

#View Data
View(Data2)

#Create Pairwise Meta Analysis
Meta <- metagen(TE = es,
                         seTE = se,
                         studlab = Study,
                         data = Data2,
                         sm = "SMD",
                         method.SMD = "Hedges",
                         fixed = FALSE,
                         random = TRUE,
                         prediction = TRUE,
                         method.tau = "REML",
                         hakn = TRUE,
                         title = "Pairwise Random-Effects Meta Analysis: HM2 VS CTRL")
#Assess heterogeneity
summary(Meta) #Moderate heterogeneity, significant cochrans Q

#Outlier analysis and influential cases

#detect outliers by using the "find.outliers" function
Meta_Outliers <- find.outliers(Meta)
Meta_Outliers
#Outlier studies are detected if their 95% confidence interval lies ouside the 95% confidence interval of the pooled effect.
#Result: Kubo 2021 is an outlier

#view differences in pooled effect 
Meta
Meta_Outliers

#View Influential Cases - Dont have to have large ES, but have a large impact on the ES or the heterogeneity
#generate Baujat plot
Meta_inf <- InfluenceAnalysis(Meta, random = TRUE)

plot(Meta_inf, "baujat")

plot(Meta_inf, "influence")


#Leave one out analysis
plot(Meta_inf, "I2") #plot sorted by heterogeneity 
plot(Meta_inf, "es") #plot sorted by effect sizes 

#Should consider excluding Rodriguez - Lopez 2022
# HM2 vs LM2 - Comparison 126####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 126)

#View Data
View(Data2)

#Create Pairwise Meta Analysis
Meta <- metagen(TE = es,
                seTE = se,
                studlab = Study,
                data = Data2,
                sm = "SMD",
                method.SMD = "Hedges",
                fixed = FALSE,
                random = TRUE,
                prediction = TRUE,
                method.tau = "REML",
                hakn = TRUE,
                title = "Pairwise Random-Effects Meta Analysis: HM2 VS LM2")
#Assess heterogeneity
summary(Meta) #Moderate heterogeneity, significant cochrans Q

#Outlier analysis and influential cases

#detect outliers by using the "find.outliers" function
Meta_Outliers <- find.outliers(Meta)
Meta_Outliers
#Outlier studies are detected if their 95% confidence interval lies ouside the 95% confidence interval of the pooled effect.
#Result: Kubo 2021 is an outlier

#view differences in pooled effect 
Meta
Meta_Outliers

#View Influential Cases - Dont have to have large ES, but have a large impact on the ES or the heterogeneity
#generate Baujat plot
Meta_inf <- InfluenceAnalysis(Meta, random = TRUE)

plot(Meta_inf, "baujat")

plot(Meta_inf, "influence")


#Leave one out analysis
plot(Meta_inf, "I2") #plot sorted by heterogeneity 
plot(Meta_inf, "es") #plot sorted by effect sizes 

# HM3 vs CTRL - Comparison 131####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 131)

#View Data
View(Data2)

#Create Pairwise Meta Analysis
Meta <- metagen(TE = es,
                seTE = se,
                studlab = Study,
                data = Data2,
                sm = "SMD",
                method.SMD = "Hedges",
                fixed = FALSE,
                random = TRUE,
                prediction = TRUE,
                method.tau = "REML",
                hakn = TRUE,
                title = "Pairwise Random-Effects Meta Analysis: HM3 VS CTRL")
#Assess heterogeneity
summary(Meta) #Moderate heterogeneity, significant cochrans Q

#Outlier analysis and influential cases

#detect outliers by using the "find.outliers" function
Meta_Outliers <- find.outliers(Meta)
Meta_Outliers
#Outlier studies are detected if their 95% confidence interval lies ouside the 95% confidence interval of the pooled effect.
#Result: Kubo 2021 is an outlier

#view differences in pooled effect 
Meta
Meta_Outliers

#View Influential Cases - Dont have to have large ES, but have a large impact on the ES or the heterogeneity
#generate Baujat plot
Meta_inf <- InfluenceAnalysis(Meta, random = TRUE)

plot(Meta_inf, "baujat")

plot(Meta_inf, "influence")


#Leave one out analysis
plot(Meta_inf, "I2") #plot sorted by heterogeneity 
plot(Meta_inf, "es") #plot sorted by effect sizes 

# LM2 vs CTRL - Comparison 61####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 61)

#View Data
View(Data2)

#Create Pairwise Meta Analysis
Meta <- metagen(TE = es,
                seTE = se,
                studlab = Study,
                data = Data2,
                sm = "SMD",
                method.SMD = "Hedges",
                fixed = FALSE,
                random = TRUE,
                prediction = TRUE,
                method.tau = "REML",
                hakn = TRUE,
                title = "Pairwise Random-Effects Meta Analysis: LM2 VS CTRL")
#Assess heterogeneity
summary(Meta) #Moderate heterogeneity, significant cochrans Q

#Outlier analysis and influential cases

#detect outliers by using the "find.outliers" function
Meta_Outliers <- find.outliers(Meta)
Meta_Outliers
#Outlier studies are detected if their 95% confidence interval lies ouside the 95% confidence interval of the pooled effect.

#view differences in pooled effect 
Meta
Meta_Outliers

#View Influential Cases - Dont have to have large ES, but have a large impact on the ES or the heterogeneity
#generate Baujat plot
Meta_inf <- InfluenceAnalysis(Meta, random = TRUE)

plot(Meta_inf, "baujat")

plot(Meta_inf, "influence")


#Leave one out analysis
plot(Meta_inf, "I2") #plot sorted by heterogeneity 
plot(Meta_inf, "es") #plot sorted by effect sizes 

# LM3 vs CTRL - Comparison 71####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 71)

#View Data
View(Data2)

#Create Pairwise Meta Analysis
Meta <- metagen(TE = es,
                seTE = se,
                studlab = Study,
                data = Data2,
                sm = "SMD",
                method.SMD = "Hedges",
                fixed = FALSE,
                random = TRUE,
                prediction = TRUE,
                method.tau = "REML",
                hakn = TRUE,
                title = "Pairwise Random-Effects Meta Analysis: LM3 VS CTRL")
#Assess heterogeneity
summary(Meta) #Moderate heterogeneity, significant cochrans Q

#Outlier analysis and influential cases

#detect outliers by using the "find.outliers" function
Meta_Outliers <- find.outliers(Meta)
Meta_Outliers
#Outlier studies are detected if their 95% confidence interval lies ouside the 95% confidence interval of the pooled effect.

#view differences in pooled effect 
Meta
Meta_Outliers

#View Influential Cases - Dont have to have large ES, but have a large impact on the ES or the heterogeneity
#generate Baujat plot
Meta_inf <- InfluenceAnalysis(Meta, random = TRUE)

plot(Meta_inf, "baujat")

plot(Meta_inf, "influence")


#Leave one out analysis
plot(Meta_inf, "I2") #plot sorted by heterogeneity 
plot(Meta_inf, "es") #plot sorted by effect sizes 

# LM3 vs LM2 - Comparison 71####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 76)

#View Data
View(Data2)

#Create Pairwise Meta Analysis
Meta <- metagen(TE = es,
                seTE = se,
                studlab = Study,
                data = Data2,
                sm = "SMD",
                method.SMD = "Hedges",
                fixed = FALSE,
                random = TRUE,
                prediction = TRUE,
                method.tau = "REML",
                hakn = TRUE,
                title = "Pairwise Random-Effects Meta Analysis: LM3 VS LM2")
#Assess heterogeneity
summary(Meta) #Moderate heterogeneity, significant cochrans Q

#Outlier analysis and influential cases

#detect outliers by using the "find.outliers" function
Meta_Outliers <- find.outliers(Meta)
Meta_Outliers
#Outlier studies are detected if their 95% confidence interval lies ouside the 95% confidence interval of the pooled effect.

#view differences in pooled effect 
Meta
Meta_Outliers

#View Influential Cases - Dont have to have large ES, but have a large impact on the ES or the heterogeneity
#generate Baujat plot
Meta_inf <- InfluenceAnalysis(Meta, random = TRUE)

plot(Meta_inf, "baujat")

plot(Meta_inf, "influence")


#Leave one out analysis
plot(Meta_inf, "I2") #plot sorted by heterogeneity 
plot(Meta_inf, "es") #plot sorted by effect sizes 
