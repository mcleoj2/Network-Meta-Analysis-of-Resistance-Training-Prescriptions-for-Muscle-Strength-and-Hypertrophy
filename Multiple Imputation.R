#Multiple Imputation for Hypertrophy NMA - October 4th, 2022
#Author: J Mcleod

#Clear environment, install packages, and set working directory
rm(list=ls())

#Load packages 
library(mice)
library(metafor)
library(VIM)
library(dplyr)
library(sjmisc)

#Load data set
Data <- read.csv('TBR_Hypertrophy_MissingCovariates.csv',
                 header=TRUE)

#Inspect the incomplete data set
Data
summary(Data)
str(Data)

# Visualizing data missing from covariates of interest Keep only the covariates from the data set
DataMissingViz <- Data[c("Sample_Size_Females_1", "Sample_Size_Males_1", 
                         "Sample_Size_Females_2", "Sample_Size_Males_2",
                         "Number_Exercises_1", "Perc1Rm_1", "Number_Exercises_2",
                         "Perc1Rm_2", "Duration", "Fatigue_0No", "Training_Status_0Untrained",
                         "Hyp_Tool_0Ultrasound", "Hyp_Region_0Lower")]

data.frame(k.NA=colSums(is.na(DataMissingViz))) #count missing values in each column

md.pattern(DataMissingViz)#Check the patterns of missigness 

#another way to view the data that is missing
aggrplot <- aggr(DataMissingViz, col=c('navyblue', 'yellow'),
                 numbers=TRUE, sortVars=TRUE,
                 labels=names("DataMissingViz"),
                 cex.axis=.7,
                 gap=1, ylab=c("Histogram of Missing Data", "Pattern"))

#View the flux pattern. Values wuth lower outflux has lower predictive power for imputation
fx <- fluxplot(Data)
fx #Do not include sample size for males or females (for either arms) due to low outflux, and maybe Perc1Rm?

#Convert all Categorical  variables to factors
#Training Status - 0 Untrained; 1 Trained
#Fatigue - 0 No; 1 Yes
#Hyp_Tool_0Ultrasound - 0 Ultrasound; 1 MRI; 2 BIA; 3 DXA; 4 Fibre CSA; 5 CT; 6 Bod Pod; 7 Hydro
#Hyp_Region_0Lower - 0 Lower; 1 Upper; 2 Whole Body
Data$Training_Status_0Untrained <- factor(Data$Training_Status_0Untrained)
Data$Fatigue_0No <- factor(Data$Fatigue_0No)
Data$Hyp_Tool_0Ultrasound <- factor(Data$Hyp_Tool_0Ultrasound)
Data$Hyp_Region_0Lower <- factor(Data$Hyp_Region_0Lower)

#View the structure of the data frame now
str(Data) #Ensure that the categorical factors have the correct number of levels. 

PredMatrix <- make.predictorMatrix(Data) #Generate prediction matrix
PredMatrix
#need to exclude variables that we do not want to used for imputing
PredMatrix[,c("Study", "Treatment1", "Treatment_Dummy1", "Treatment2",
              "Treatment_Dummy2", "Change_Mean1",
              "Change_SD1", "Change_SD2", "Change_Mean2",
              "Treatment_Comparison", "weight", "se", "var",
              "ci.lo", "ci.hi", "measure", "sample.size",
              "Sample_Size_Females_1", "Sample_Size_Females_2", 
              "Sample_Size_Males_1", "Sample_Size_Males_2")] <- 0 #dont use the following variables as predictors for imputing

#View new prediction matrix
PredMatrix


ImpMethod <- make.method(Data) #Generate Imp methods

ImpMethod #ensure that all methods are correct (pmm for continuous, logreg for categorical, 
#and "" for values that will not be imputed[either because we dont want to, such as study and diff, 
#or because we dont need to because we have completed data])

ImpMethod[c("Sample_Size_Females_1", "Sample_Size_Females_2")] <- "" #dont impute female data, we will calculate from male imputed data

#View new Imputation Method
ImpMethod

#Impute
imp <- mice(Data, m=20, maxit=35, predictorMatrix=PredMatrix, method=ImpMethod, seed = 1234)
imp

plot(imp) #plot convergence

#View if the imputed data (red) follows similar trends as the observed data (blue)
stripplot(imp, Sample_Size_Males_1~.imp)
stripplot(imp, Sample_Size_Males_2~.imp)
stripplot(imp, Number_Exercises_1~.imp)
stripplot(imp, Perc1Rm_1~.imp)
stripplot(imp, Reps_1~.imp)
stripplot(imp, Number_Exercises_2~.imp)
stripplot(imp, Perc1Rm_2~.imp)
stripplot(imp, Fatigue_0No~.imp)
stripplot(imp, Training_Status_0Untrained~.imp)

#plot the density plots of the observed and imputed values
densityplot(imp, ~Sample_Size_Males_1)
densityplot(imp, ~Sample_Size_Males_2)
densityplot(imp, ~Number_Exercises_1)
densityplot(imp, ~Perc1Rm_1)
densityplot(imp, ~Reps_1)
densityplot(imp, ~Number_Exercises_2)
densityplot(imp, ~Perc1Rm_2)
#Investigate Perc1Rm_2 more
densityplot(imp, ~Perc1Rm_2 | .imp)
hist(Data$Perc1Rm_2)
#This is the only density plot that is abnormal, but makes sense 
#because because the original distribution (blue) contains the CTRL groups(which are 0).
densityplot(imp, ~Fatigue_0No)
densityplot(imp, ~Training_Status_0Untrained)

#Compare the means of the imputed data set with the means of the incomplete data set
summary(Data) #Incomplete data set
summary(complete(imp)) #Imputed data set - the comparison should be relatively reasonable

#Run sample regression with complete case missing data vs imputed data
#Complete case missing data regression
fit_original <- rma(es, var, mods = ~ Sample_Size_Males_1 + Age_Mean_1 + 
                      Duration + Perc1Rm_1 + Fatigue_0No +
                      Training_Status_0Untrained + Hyp_Tool_0Ultrasound + Hyp_Region_0Lower, data=Data)
#Imputed Data Set
fit_imp <- with(imp, rma(es, var, mods = ~ Sample_Size_Males_1 + Age_Mean_1 + 
                           Duration + Perc1Rm_1 + Fatigue_0No +
                           Training_Status_0Untrained + Hyp_Tool_0Ultrasound + Hyp_Region_0Lower))

#complete case missing data regression results
summary(fit_original)

#imputed regression results
pool <- summary(pool(fit_imp))
pool[-1] <- round(pool[-1], digits=4)
pool


#Merge Imputations
Merged_Imp <- merge_imputations(Data,imp, ori = Data)

#Extract Imputation Data
file <- "TBR_Hypertrophy_MI_Original.csv"
write.csv(Merged_Imp, file=file)

#Open excel doc with Imputed data sets
#View your imputed data sets, and for unreasonable values, replace with plausible value from the imputed data sets
imp$imp$Sample_Size_Males_1
imp$imp$Sample_Size_Males_2
imp$imp$Perc1Rm_1
imp$imp$Perc1Rm_2
