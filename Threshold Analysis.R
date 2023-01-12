#This code will run threshold analysis following an NMA to determine the robustness of your treatment rankings

#Author: J.Mcleod
rm(list=ls())
library(nmathresh)
library(Matrix)
library(multinma)
library(dplyr)
library(ggplot2)
library(tidyr)

# order of events to conduct study level threshold analysis

# - perform unadjusted NMA using multinma, will need this information to get summary data and posterior covariance
# - from Multinma, get summary data
# - from Multinma, extract parameters (d, and delta for RE)
# - create posterior covariance matrix
# - Read in dat.raww for design matrix
# - Perform Thresholding

# Perform Unadjusted NMA ####
options(mc.cores= parallel::detectCores())


#Read in Contrast-Based Data
Data <- read.csv('TBR_Hypertrophy_ArmLevelContrastData_MultipleImputationDataSet_MultiNMA_Oct2022.csv',
                 header=TRUE)
#View the data
View(Data)
str(Data)
head(Data)

#Your data should be in contrast-based format. Make sure you've accounted for multi-arm trials
TBR_Hypertrophy_Net <- set_agd_contrast(data = Data,
                                        study = study,
                                        trt = treatment,
                                        y = diff, 
                                        se = std.err,
                                        trt_ref = "CTRL",
                                        sample_size = sample.size)


#Summarize the network and plot the NMA
TBR_Hypertrophy_Net

#Visualize Network Geometry
plot(TBR_Hypertrophy_Net, weight_edges = TRUE, weight_nodes = TRUE) + 
  ggplot2::theme (legend.position = "bottom", legend.box = "vertical")

#Random Effect NMA
TBR_Hypertrophy_NMA_RE_unadj <- nma(TBR_Hypertrophy_Net,
                                    trt_effects = "random",
                                    chains = 4,
                                    prior_trt = normal(scale = 100),
                                    prior_het = half_normal (scale = 5),
                                    warmup = 4000, iter = 10000,
                                    thin = 10)
TBR_Hypertrophy_NMA_RE_unadj

#extract summary statistics
#only need basic parameters, in other words, extract mean estimates vs CTRL
#Relative effects compared with control 
relative_effects(TBR_Hypertrophy_NMA_RE_unadj, trt_ref = "CTRL",
                                             probs = c(0.025,0.975)) %>%
  as.data.frame() -> relative_effects

#extract d and delta sampling to construct posterior covariance
post <- as.data.frame(TBR_Hypertrophy_NMA_RE_unadj)


#get indices of parameters
vnames <- sub("(.*)\\[.*","\\1", colnames(post))
ind.d <- which(vnames == "d")
ind.delta <- which(vnames=="delta")

#create posterior covariance
post.cov <- cov(as.matrix(post[,c(ind.d, ind.delta)]))

# Read study data
dat.raww <- read.csv('NMAThresh.csv',
                             header=TRUE)

# Print first few rows
head(dat.raww)

n <- nrow(dat.raww) #Number of rows

# Turn wide study data into long with one row for each arm
dat.rawl <- reshape(dat.raww, varying=c("t.2","y.2","Var.2","t.3","y.3","Var.3",
                                        "t.4","y.4","Var.4","t.5","y.5","Var.5"),
                    timevar="arm", idvar="studyID", direction="long")

# Sort data by study and contrast, removing NA rows
dat.rawl <- dat.rawl[order(dat.rawl$studyID, dat.rawl$arm, dat.rawl$y, na.last=NA),]

K <- length(unique(c(dat.rawl$t.1, dat.rawl$t)))    # Number of treatments
N <- nrow(dat.rawl)    # Number of data points

# Create likelihood covariance matrix
V.diag <- as.list(rep(NA,n))
attach(dat.raww)
for (i in 1:n){
  if (n.arms[i] == 2){
    V.diag[[i]] <- Var.2[i]
  }
  else {
    V.diag[[i]] <- matrix(V[i], nrow=n.arms[i]-1, ncol=n.arms[i]-1)
    tempVar <- c(Var.2[i], Var.3[i], Var.4[i], Var.5[i])
    diag(V.diag[[i]]) <- tempVar[!is.na(tempVar)]
  }
}
detach(dat.raww)

lik.cov <- bdiag(V.diag)

trt.code <- c("CTRL","HM1", "HM2","HM3", "HS2", "HS3",  "LM1" ,"LM2" ,"LM3","LS2", "LS3")

#Conduct Thresholding
thresh <- nma_thresh(mean.dk = relative_effects[, "mean"], 
                     lhood = lik.cov, 
                     post = post.cov,
                     nmatype = "random",
                     trt.code = trt.code,
                     opt.max = TRUE)

#Construct forest plot
# 95% CIs
dat.rawl$CI2.5 <- with(dat.rawl, y + qnorm(0.025)*sqrt(Var))
dat.rawl$CI97.5 <- with(dat.rawl, y + qnorm(0.975)*sqrt(Var))

# Study labels
dat.rawl$lab <- with(dat.rawl, paste0(study," (",t," vs ",t.1,")"))

# Forest plot - all contrasts, very large
#thresh_forest(thresh, y, CI2.5, CI97.5, label = lab, data = dat.rawl,
 #              label.title = "Study (Contrast)", xlab = "Standardised Mean Difference", 
  #             xlim = c(-4, 3), y.title = "SMD", refline = 0, digits = 2)

 
 # Forest plot - only contrasts with thresholds <4 SMD
 cutoff <- 4
 absmin <- function(x) min(abs(x))
 dat.rawl$coverage <- apply(thresh$thresholds[, c("lo", "hi")] / 
                              (dat.rawl[, c("CI2.5", "CI97.5")] - dat.rawl$y), 
                            1, absmin)
 dat.rawl$ord <- ifelse(thresh$thresholds$lo > -cutoff | thresh$thresholds$hi < cutoff, 
                        dat.rawl$coverage, NA)
 
 thresh.forest <- thresh_forest(thresh, y, CI2.5, CI97.5, label = lab, 
               orderby = list(ord, na.last = NA), data = dat.rawl,
               label.title = "Study (Contrast)", xlab = "Standardised Mean Difference", 
               xlim = c(-4, 3), y.title = "SMD", refline = 0, digits = 2, calcdim = TRUE)
 
 #Combine threshold data with your data frame, and export data.
 Thresh.combine <- as.data.frame(c(dat.rawl, thresh$thresholds)) #still need to figure this out
file='StudyThresholdAnalysis_CombinedData.csv'
write.csv(Thresh.combine, file=file)
