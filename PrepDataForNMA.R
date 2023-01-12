#Reshaping Wide Contrast Level Data to Long-Contrast Level Data----
#Author: J.Mcleod

#Clear environment, Install and/or load packages
rm(list=ls(all=T))
#If you dont have the following packages below, run install.packages("package name")
library(dplyr)
library(tidyr)
library(magrittr)
library(tibble)

#Read in and view contrast-level EffectSize Data
Data <- read.csv ('TBR_Hypertrophy_Pairwise_EffectSizes_Oct2022.csv', sep=',',
                  header=TRUE)
View(Data)
str(Data)

#Keep columns that we need for MultiNMA
Data2 <- select(Data, Study, es, se, Treatment1, Treatment2, Sample_Size1, Sample_Size2)  

#Pivot the data to arm-level format
Data2 %>%
  dplyr::select(1:7) %>%
  pivot_longer(-Study,
               names_to = c(".value"),
               names_pattern = "(..)") %>% 
  set_colnames(c("study", "diff", 
                 "std.err", "treatment","Sample Size")) -> ArmLevelContrastedData
#Save this data to CSV (NOTE: this is still not ready for use in MultiNMA, 
#we must first account for correlation in multi-arm trials)
write.csv(ArmLevelContrastedData,
          file ="TBR_Hypertrophy_ArmLevelContrastData_MultiNMA.csv",
          row.names = FALSE)

#Pull out all multi-arm trials, and create a table. This will help to determine 
#which SE in the above CSV need to be modified (to account for correlations)
ArmLevelContrastedData %>%
  pull(study)%>% 
  table() %>% 
  {.[. > 2]} %>% as.data.frame() -> MultiArmTrials
MultiArmTrials
#Save this list to CSV
write.csv(MultiArmTrials,
          file = "TBR_Hypertrophy_ListOfMultiArmTrials.csv",
          row.names = FALSE)
#NEXT STEP: In excel, calculate a correlation between multiarm trials, and place in the "SE" col
#of the reference arm, remove redundant comparisons.


###BELOW IS FOR USE IN GEMTC TO CALL THE FULL TREATMENT NAMES. WE MAY NOT USE 
#FOR MULITNMA
#Prepare Treatment List
#unique(ArmLevelContrastedData$treatment)
#c("MMM" = "Moderate Load, Multi-Set, Multi-Frequency", 
#"CTRL" = "Control",
#"MSM" = "Moderate Load, Single-Set, Multi-Frequency",
#"HMM" = "High Load, Multi-Set, Multi-Frequency",
#"LMM" = "Low Load, Multi-Set, Multi-Frequency",
#"LSM" = "Low Load, Single-Set, Multi-Frequency",
#"MMS" = "Moderate Load, Multi-Set, Single-Frequency",
#"HSM" = "High Load, Single-Set, Multi-Frequency",
#"HMS" = "High Load, Multi-Set, Single-Frequency")%>% 
 # data.frame() %>% 
 # set_colnames("description") %>% 
  #rownames_to_column("id") -> TreatmentCodes
#TreatmentCodes

#Save Treatment codes CSV for GeMTC
#write.csv(TreatmentCodes,
 #         file = "TBR_Hypertrophy_TreatmentCodes_GeMTC.csv", 
  #        row.names = FALSE)
