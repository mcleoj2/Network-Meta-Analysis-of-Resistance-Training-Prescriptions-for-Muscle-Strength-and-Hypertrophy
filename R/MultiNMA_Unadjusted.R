#This script will run an unadjusted Bayesian NMA
#Author: J.Mcleod

#This run will include all of the hypertrophy studies
rm(list=ls())

library (multinma)
library(dplyr)
library(ggplot2)
library(tidyr)

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

#Fixed Effect NMA
TBR_Hypertrophy_NMA_FE_unadj <- nma(TBR_Hypertrophy_Net,
                                    trt_effects = "fixed",
                                    chains = 4,
                                    prior_trt = normal(scale = 100),
                                    prior_het = half_normal (scale = 5),
                                    warmup = 4000, iter = 10000,
                                    thin = 10)

TBR_Hypertrophy_NMA_FE_unadj

#Random Effects NMA
TBR_Hypertrophy_NMA_RE_unadj <- nma(TBR_Hypertrophy_Net,
                                    trt_effects = "random",
                                    chains = 4,
                                    prior_trt = normal(scale = 100),
                                    prior_het = half_normal (scale = 5),
                                    warmup = 4000, iter = 10000,
                                    thin = 10)

TBR_Hypertrophy_NMA_RE_unadj

#View Model Fit 
(TBR_Hypertrophy_NMA_FE_dic <- dic(TBR_Hypertrophy_NMA_FE_unadj))
(TBR_Hypertrophy_NMA_RE_dic <- dic(TBR_Hypertrophy_NMA_RE_unadj))

#View Heterogeneity of RE model
summary(TBR_Hypertrophy_NMA_RE_unadj, pars = "tau")

#In this case, the RE model fits the data better, use the RE NMA
plot(TBR_Hypertrophy_NMA_FE_dic)
plot(TBR_Hypertrophy_NMA_RE_dic)#Residual Deviance plot

#Extract Individual Study contributions to Residual Deviance, Leverage, and DIC for FE
TBR_Hypertrophy_NMA_FE_ResDev <- 
  as.data.frame(TBR_Hypertrophy_NMA_FE_dic[["pointwise"]][["agd_contrast"]])
file <- "NMA_Unadjusted_FE_Residual Deviance, Leverage, and DIC.csv"
write.csv(TBR_Hypertrophy_NMA_FE_ResDev, file = file) #can use this data to construct leverage plots

#Extract Individual Study contributions to Residual Deviance, Leverage, and DIC for RE
TBR_Hypertrophy_NMA_RE_ResDev <- 
  as.data.frame(TBR_Hypertrophy_NMA_RE_dic[["pointwise"]][["agd_contrast"]])
file <- "NMA_Unadjusted_RE_Residual Deviance, Leverage, and DIC.csv"
write.csv(TBR_Hypertrophy_NMA_RE_ResDev, file = file) #can use this data to construct leverage plots

#Plot the prior and posterior
plot_prior_posterior(TBR_Hypertrophy_NMA_RE_unadj, ref_line = 0,
                     prior_args = list(colour="darkred"),
                     post_args = list(colour="black", fill='white')) +
  ggplot2::labs(x="SMD (Hedges G), Compared With Control") #save plot
#prior distributions are the lines, and posterior distributions are the histograms


#Relative effects compared with control 
relative_effects_RE_CTRL <- relative_effects(TBR_Hypertrophy_NMA_RE_unadj, trt_ref = "CTRL",
                                          probs = c(0.025,0.975))
relative_effects_RE_CTRL

#Plot the relative effects compared with control
plot(relative_effects_RE_CTRL, ref_line = 0) +
  ggplot2::labs(x="SMD (Hedges G)", y="Treatment Compared with Control")#save plot

#Extract relative effects compared with control 
file <- "Hypertrophy_RE_NMA_Unadjusted_RelativeEffectsvsCTRL.csv"
relative_effects_RE_CTRL <- as.data.frame(relative_effects_RE_CTRL)
write.csv(relative_effects_RE_CTRL, file=file)

#All relative effects
relative_effects_RE_ALL <- relative_effects(TBR_Hypertrophy_NMA_RE_unadj, all_contrasts = TRUE,
                                         probs = c(0.025,0.975))

#plot all of the relative effects 
plot(relative_effects_RE_ALL, ref_line = 0) +
  ggplot2::labs(x="SMD (Hedges G)", y="Comparisons") #save plot

#Extract all relative effects 
file <- "Hypertrophy_RE_NMA_Unadjusted_AllRelativeEffects.csv"
relative_effects_RE_ALL <- as.data.frame(relative_effects_RE_ALL)
write.csv(relative_effects_RE_ALL, file=file)

#League Table Creation
rxp <- "^d\\[(.+) vs\\. (.+)\\]$"
relative_effects_RE_ALL %>%
  as_tibble() %>%
  transmute (trtb= gsub(rxp, "\\1", parameter),
             trta= gsub(rxp, "\\2", parameter),
             estci = base::sprintf("%.2f (%.2f, %.2f)", mean, relative_effects_RE_ALL$`2.5%` , relative_effects_RE_ALL$`97.5%` )) %>%
  pivot_wider(names_from = trtb, values_from = estci) -> League_Table

#Save League Table
file <- "Hypertrophy_RE_NMA_Unadjusted_LeagueTable.csv"
write.csv(League_Table, file=file)

#Generate Predictive Distributions
Predict_Distrbutions <- relative_effects(TBR_Hypertrophy_NMA_RE_unadj, all_contrasts = TRUE,
                                            probs = c(0.025,0.975), predictive_distribution = TRUE)

#plot all of the predictive distributions 
plot(Predict_Distrbutions, ref_line = 0) +
  ggplot2::labs(x="SMD (Hedges G)", y="Comparisons") #save plot

#Extract predictive distributions
file <- "Hypertrophy_RE_NMA_Unadjusted_PredictiveDistributions.csv"
Predict_Distrbutions <- as.data.frame(Predict_Distrbutions)
write.csv(Predict_Distrbutions, file=file)

#Posterior Ranks
TBR_Hypertrophy_RE_Unadj_Ranks <- posterior_ranks(TBR_Hypertrophy_NMA_RE_unadj,
                                                  lower_better = FALSE,
                                                  sucra = TRUE)
TBR_Hypertrophy_RE_Unadj_Ranks

#Plot Ranks
plot(TBR_Hypertrophy_RE_Unadj_Ranks) #Save Plot

#Extract Posterior Ranks
file <- "Hypertrophy_RE_NMA_Unadjusted_PosteriorRanks.csv"
TBR_Hypertrophy_RE_Unadj_Ranks <- as.data.frame(TBR_Hypertrophy_RE_Unadj_Ranks)
write.csv(TBR_Hypertrophy_RE_Unadj_Ranks, file=file)


#Rank Probability
TBR_Hypertrophy_RE_Unadj_RankProbs <- posterior_rank_probs(TBR_Hypertrophy_NMA_RE_unadj,
                                                           lower_better = FALSE,
                                                           sucra = TRUE)
TBR_Hypertrophy_RE_Unadj_RankProbs

#Plot Rank Prob
plot(TBR_Hypertrophy_RE_Unadj_RankProbs) #save plot

#Extract rank prob data
file <- "Hypertrophy_RE_NMA_Unadjusted_RankProb.csv"
TBR_Hypertrophy_RE_Unadj_RankProbs <- as.data.frame(TBR_Hypertrophy_RE_Unadj_RankProbs)
write.csv(TBR_Hypertrophy_RE_Unadj_RankProbs, file=file)

(bcg_predeff_unadj <- relative_effects(bcg_fit_unadj, predictive_distribution = TRUE))




#Assess Consistency Assumptions
#Global Inconsistency --> UME 
TBR_Hypertrophy_NMA_RE_ume <- nma(TBR_Hypertrophy_Net,
                                    consistency = "ume",
                                    trt_effects = "random",
                                    chains = 4,
                                    prior_trt = normal(scale = 100),
                                    prior_het = half_normal (scale = 5),
                                    warmup = 4000, iter = 10000,
                                    thin = 10)

TBR_Hypertrophy_NMA_RE_ume_dic <- dic(TBR_Hypertrophy_NMA_RE_ume)
TBR_Hypertrophy_NMA_RE_dic
TBR_Hypertrophy_NMA_RE_ume_dic

#plot UME vs unadjusted model
plot(TBR_Hypertrophy_NMA_RE_dic, TBR_Hypertrophy_NMA_RE_ume_dic,show_uncertainty = FALSE)+
  ggplot2::labs(x="Residual Deviance (Unadjusted NMA)", y="Residual Deviance (UME)") #save

TBR_Hypertrophy_NMA_RE_ResDev_ume <- 
  as.data.frame(TBR_Hypertrophy_NMA_RE_ume_dic[["pointwise"]][["agd_contrast"]])
file <- "NMA_Unadjusted_RE_UME_Residual Deviance, Leverage, and DIC.csv"
write.csv(TBR_Hypertrophy_NMA_RE_ResDev_ume, file = file)


#Local Inconsistency - NodeSplit
TBR_Hypertrophy_NMA_RE_nodesplit <- nma(TBR_Hypertrophy_Net,
                        consistency = "nodesplit",
                        trt_effects = "random",
                        chains = 4,
                        prior_trt = normal(scale = 100),
                        prior_het = half_normal (scale = 5),
                        warmup = 4000, iter = 10000,
                        thin = 10)

summary(TBR_Hypertrophy_NMA_RE_nodesplit) #copy and paste output into word

#Plot nodesplit
plot(TBR_Hypertrophy_NMA_RE_nodesplit) +
  ggplot2::labs(x="SMD (Hedges G)") #Save image
