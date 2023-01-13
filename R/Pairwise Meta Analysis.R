#CODE TO GENERATE PAIRWISE META RESULTS, FOREST PLOTS, AND ASSESS PUBLICATION BIAS
#Author: J.Mcleod 

#NOTE: this code will not assess outliers and influential cases, see, "Outliers and Influential Cases Script."
#If you dont have the following packages below, run install.packages("package name")
if (!require("meta", quietly = TRUE))
    install.packages("meta")
if (!require("tidyverse", quietly = TRUE))
    install.packages("tidyverse")
if (!require("dmetar", quietly = TRUE))
    install.packages("dmetar")
if (!require("gridExtra", quietly = TRUE))
    install.packages("gridExtra")

#Read in and view your data, With calculated effect sizes
Data <- read_csv ("TBR_Hypertrophy_Pairwise_EffectSizes_Oct2022.csv")
##### FOR EACH COMPARISON, COPY AND PASTE THE FOLLOWING CODE BELOW#
#### ADJUST THE OBJECT NAMES ACCORDING TO EACH COMPARISON##

###########################
##COMPARISON 31: LS2 VS CTRL ####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 31)

#Create Pairwise Meta Analysis
Meta <- 
       metagen(
              TE = es,
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
              title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: LS2 VS CTRL"
              )

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- 
       forest.meta(
              Meta, 
              sortvar = TE,
              leftcols = c("studlab"),
              leftlabs = c("Study"),
              xlim = c(-1,3.5),
              test.overall.random = TRUE,
              label.left = "Favors CTRL",
              label.right = "Favors LS2",
              colgap.forest = "3.5cm"
              )
#Save Image
save.image(Forest, "outputs/Forest.png")

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- 
       funnel.meta(
              x = Meta,
              contour= c(0.9,0.95,0.99),
              col.contour = col.contour,
              random= TRUE,
              studlab = TRUE,
              xlim = c(-1,3.5)
              )
#Generate Title and Add Legend
title("Funnel Plot (LS2 VS CTRL)")
legend(x = 0.75, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10


##COMPARISON 41: LS3 VS CTRL ####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 41)

#View Data
View(Data2)

#Create Pairwise Meta Analysis
Meta <- metagen(
       TE = es,
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
       title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: LS3 VS CTRL"
       )


#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest2 <- 
       forest.meta(
              Meta, 
              sortvar = TE,
              leftcols = c("studlab"),
              leftlabs = c("Study"),
              xlim = c(-1,1.8),
              test.overall.random = TRUE,
              label.left = "Favors CTRL",
              label.right = "Favors LS3",
              colgap.forest = "3.5cm"
              )
#Save Image
save.image(Forest2, "outputs/Forest2.png")

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-1,1.5))
#Generate Title and Add Legend
title("Funnel Plot (LS3 VS CTRL)")
legend(x = 0.75, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10



##COMPARISON 43: LS3 VS LS2 ####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 43)

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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: LS3 VS LS2")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1,1),
                      test.overall.random = TRUE,
                      label.left = "Favors LS2",
                      label.right = "Favors LS3",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-1,1.5))
#Generate Title and Add Legend
title("Funnel Plot (LS3 VS LS2)")
legend(x = 0.75, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10



##COMPARISON 61: LM2 VS CTRL ####
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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: LM2 VS CTRL")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1,2.80),
                      test.overall.random = TRUE,
                      label.left = "Favors CTRL",
                      label.right = "Favors LM2",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-1,1.5))
#Generate Title and Add Legend
title("Funnel Plot (LM2 VS CTRL)")
legend(x = 0.75, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10



##COMPARISON 63: LM2 VS LS2 ####
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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: LM2 VS LS2")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.25,1.25),
                      test.overall.random = TRUE,
                      label.left = "Favors LS2",
                      label.right = "Favors LM2",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-1,1.5))
#Generate Title and Add Legend
title("Funnel Plot (LM2 VS LS2)")
legend(x = 0.75, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10



##COMPARISON 65: LM2 VS LM1 ####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 65)

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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: LM2 VS LM1")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.25,1.25),
                      test.overall.random = TRUE,
                      label.left = "Favors LM1",
                      label.right = "Favors LM2",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-1,1.5))
#Generate Title and Add Legend
title("Funnel Plot (LM2 VS LM1)")
legend(x = 0.75, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10



##COMPARISON 71: LM3 VS CTRL ####
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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: LM3 VS CTRL")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-2,2),
                      test.overall.random = TRUE,
                      label.left = "Favors CTRL",
                      label.right = "Favors LM3",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (LM3 VS CTRL)")
legend(x = 1.0, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10



##COMPARISON 74: LM3 VS LS3 ####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 74)

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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: LM3 VS LS3")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1,2),
                      test.overall.random = TRUE,
                      label.left = "Favors LS3",
                      label.right = "Favors LM3",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (LM3 VS LS3)")
legend(x = 1.0, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10



##COMPARISON 75: LM3 VS LM1 ####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 75)

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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: LM3 VS LM1")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.50,1),
                      test.overall.random = TRUE,
                      label.left = "Favors LM1",
                      label.right = "Favors LM3",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (LM3 VS LM1)")
legend(x = 1.0, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10



##COMPARISON 76: LM3 VS LM2 ####
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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: LM3 VS LM2")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.50,1.50),
                      test.overall.random = TRUE,
                      label.left = "Favors LM2",
                      label.right = "Favors LM3",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (LM3 VS LM2)")
legend(x = 1.0, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10



##COMPARISON 93: HS2 VS LS2 ####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 93)

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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: HS2 VS LS2")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.50,1.50),
                      test.overall.random = TRUE,
                      label.left = "Favors LS2",
                      label.right = "Favors HS2",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (HS2 VS LS2)")
legend(x = 1.0, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10




##COMPARISON 101: HS3 VS CTRL ####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 101)

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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: HS3 VS CTRL")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.50,1.50),
                      test.overall.random = TRUE,
                      label.left = "Favors CTRL",
                      label.right = "Favors HS3",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (HS3 VS CTRL)")
legend(x = 1.0, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10




##COMPARISON 104: HS3 VS LS3 ####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 104)

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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: HS3 VS LS3")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.50,1.50),
                      test.overall.random = TRUE,
                      label.left = "Favors LS3",
                      label.right = "Favors HS3",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (HS3 VS LS3)")
legend(x = 1.0, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10




##COMPARISON 107: HS3 VS LM3 ####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 107)

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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: HS3 VS LM3")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.50,1.50),
                      test.overall.random = TRUE,
                      label.left = "Favors LM3",
                      label.right = "Favors HS3",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (HS3 VS LM3)")
legend(x = 1.0, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10




##COMPARISON 111: HM1 VS CTRL ####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 111)

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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: HM1 VS CTRL")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.50,1.50),
                      test.overall.random = TRUE,
                      label.left = "Favors CTRL",
                      label.right = "Favors HM1",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (HM1 VS CTRL)")
legend(x = 1.0, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10




##COMPARISON 121: HM2 VS CTRL ####
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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: HM2 VS CTRL")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.50,3.0),
                      test.overall.random = TRUE,
                      label.left = "Favors CTRL",
                      label.right = "Favors HM2",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (HM2 VS CTRL)")
legend(x = 1.0, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10




##COMPARISON 126: HM2 VS LM2 ####
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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: HM2 VS LM2")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.50,1.5),
                      test.overall.random = TRUE,
                      label.left = "Favors LM2",
                      label.right = "Favors HM2",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (HM2 VS LM2)")
legend(x = 1.0, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10




##COMPARISON 126: HM2 VS HS2 ####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 129)

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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: HM2 VS HS2")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.50,2.5),
                      test.overall.random = TRUE,
                      label.left = "Favors HS2",
                      label.right = "Favors HM2",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (HM2 VS HS2)")
legend(x = 1.0, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10




##COMPARISON 131: HM3 VS CTRL ####
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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: HM3 VS CTRL")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.50,3.1),
                      test.overall.random = TRUE,
                      label.left = "Favors CTRL",
                      label.right = "Favors HM3",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (HM3 VS CTRL)")
legend(x = 1.0, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10




##COMPARISON 134: HM3 VS LS3 ####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 134)

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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: HM3 VS LS3")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.0,1.0),
                      test.overall.random = TRUE,
                      label.left = "Favors LS3",
                      label.right = "Favors HM3",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (HM3 VS LS3)")
legend(x = 1.0, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10




##COMPARISON 137: HM3 VS LS3 ####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 137)

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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: HM3 VS LM3")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.6,2.0),
                      test.overall.random = TRUE,
                      label.left = "Favors LM3",
                      label.right = "Favors HM3",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (HM3 VS LM3)")
legend(x = 1.0, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10




##COMPARISON 1211: HM2 VS HM1 ####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 1211)

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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: HM2 VS HM1")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance. 

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot 
Forest <- forest.meta(Meta, 
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.6,2.0),
                      test.overall.random = TRUE,
                      label.left = "Favors HM1",
                      label.right = "Favors HM2",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (HM2 VS HM1)")
legend(x = 1.0, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias, 
# or some other factor. 

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results 
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10




##COMPARISON 1310: HM3 VS HS3 ####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 1310)

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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: HM3 VS HS3")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance.

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot
Forest <- forest.meta(Meta,
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.6,2.0),
                      test.overall.random = TRUE,
                      label.left = "Favors HS3",
                      label.right = "Favors HM3",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (HM3 VS HS3)")
legend(x = 1.0, y = 0.01,
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias,
# or some other factor.

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10




##COMPARISON 1312: HM3 VS HM2 ####
#Filter Data
Data2 <- filter(Data, Treatment_Comparison == 1312)

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
                title = "Pairwise Random-Effects Meta Analysis: Hypertrophy: HM3 VS HM2")
#Assess heterogeneity
summary(Meta)

#Cochrans Q:
#Interpretation: Significance suggests the presence of between-study heterogeneity
#Caveat: Sensitive to the number of studies included in the analysis

#I^2:
#Interpretation: Higher number suggests more heterogeneity
#Caveat: Not an absolute measure of heterogeneity, and still influenced by the precision of included studies

#Tau and Tau^2:
#Interpretation: Insensitive to the number of studies. Higher number means more variance. Tau^2 is the variance of the true effect sizes; Tau is the standard deviation of the true effect sizes.
#Caveat: Difficult to determine the practicality of the variance.

#Prediction Interval (PI): Give us the range into which we can "predict" the effects of future studies to fall based on current evidence

#Generate Forest Plot
Forest <- forest.meta(Meta,
                      sortvar = TE,
                      leftcols = c("studlab"),
                      leftlabs = c("Study"),
                      xlim = c(-1.6,2.0),
                      test.overall.random = TRUE,
                      label.left = "Favors HM2",
                      label.right = "Favors HM3",
                      colgap.forest = "3.5cm")
#Save Image

#Publication Bias
#Visual inspection of publication bias -  Contoured Enhanced Funnel Plot

#Create Colour Schema
col.contour = c("grey51", "grey80", "grey95")
#Generate Funnel Plot
Funnel <- funnel.meta(x = Meta,
                      contour= c(0.9,0.95,0.99),
                      col.contour = col.contour,
                      random= TRUE,
                      studlab = TRUE,
                      xlim = c(-2.5,2.5))
#Generate Title and Add Legend
title("Funnel Plot (HM3 VS HM2)")
legend(x = 1.0, y = 0.01,
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


#Inspect the plot visually, Are the studies symmetrical (i.e., follow the  dotted, inverted, funnel lines)
#The contour visual aids with supporting - or refuting - whether your asymmetry is due to publication bias,
# or some other factor.

#Save Image

#Statistical inspection of publication bias -  Eggers Regression Test

(Egger <- eggers.test(Meta))

#View the results
#Is the Intercept significantly different from 0? There is publication bias
#Is the Intercept not significantly different from 0? there is no publication bias
#Advisable to only run these tests when the number of studies is >= 10



