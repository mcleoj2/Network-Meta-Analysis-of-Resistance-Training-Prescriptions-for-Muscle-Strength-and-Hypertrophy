#Calculate SMD, SE, and Hedges G -----

#If you dont have the following packages below, run install.packages("package name")
library(tidyverse)
library(esc)

#Load in contrast-based data sheet
#Read in and view contrast-level data ----
Data <- read_csv('TBR_Hypertrophy_Pairwise_Comparison.csv')

#Calculate SMD (specifically Hedges G), and SE
EffectSizes <- esc_mean_sd(grp1m = Data$Change_Mean1, grp2m = Data$Change_Mean2, 
                           grp1sd = Data$Change_SD1, grp2sd = Data$Change_SD2, 
                           grp1n = Data$Sample_Size1, 
                           grp2n = Data$Sample_Size2, es.type = "g")

#Convert EffectSize Object into a Data Frame
EffectSizesDataFrame <- as.data.frame(EffectSizes)

#Remove Study column from "EffectSizeDataFrame"
EffectSizesDataFrame2 <- select(EffectSizesDataFrame,es, weight,
                                sample.size, se, var, ci.lo, ci.hi, measure)

#Combine EffectSizeDataFrame2 with Original Data
EffectSizesData <- bind_cols(Data, EffectSizesDataFrame)

#Write to CSV - Will need this CSV afterwards
write_esc(EffectSizesData, path = 
            "TBR_Hypertrophy_Pairwise_EffectSizes.csv", sep = ",")
