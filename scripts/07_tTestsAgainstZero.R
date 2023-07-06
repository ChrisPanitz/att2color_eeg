# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.12.0
# --- script version: May 2023
# --- content: t-tests against zero within all conditions and for aggregated 
# ---          across all cued and all uncued conditions



# load required packages
library(here)
library(tidyr)
library(psych)
library(rstatix)
library(BayesFactor)



# Loading Hilbert data for t-tests
parentFolder <- here()

loadname <- paste0(parentFolder, "/dataframes/dfHilbertMeans.txt")
dfComp <- read.csv(loadname, header = TRUE, sep = ",")

# specify variable types
dfComp$partID <- factor(dfComp$partID)
dfComp$att <- factor(dfComp$att, levels = c("att","ign"))
dfComp$col <- factor(dfComp$col, levels = c("col","gray"))
dfComp$freq <- factor(dfComp$freq, levels = c("857","15"))

# transform into wide format
dfCompWide <- pivot_wider(data = dfComp,
                          id_cols = partID,
                          names_from = c(att,col,freq),
                          values_from = amplitude)

# compute mean across all attended onditions
dfCompWide$att_aggregated <- rowMeans(subset(dfCompWide, select = 
                                               c(att_col_857,att_col_15,
                                                 att_gray_857,att_gray_15)))

# compute mean across all ignored conditions
dfCompWide$ign_aggregated <- rowMeans(subset(dfCompWide, select = 
                                               c(ign_col_857,ign_col_15,
                                                 ign_gray_857,ign_gray_15)))

# get descriptive stats
describe(dfCompWide)


### aggregated data: frequentist t-test, Cohen's d, and Bayesian t-test against zero
# all attended conditions aggregated
t_test(data = dfCompWide, formula = att_aggregated ~ 1, alternative = "greater")
cohens_d(data = dfCompWide, formula = att_aggregated ~ 1)
ttestBF(x = dfCompWide$att_aggregated, nullInterval = c(0,Inf))

# all ignored conditions aggregated
t_test(data = dfCompWide, formula = ign_aggregated ~ 1, alternative = "less")
cohens_d(data = dfCompWide, formula = ign_aggregated ~ 1)
ttestBF(x = dfCompWide$ign_aggregated, nullInterval = c(-Inf,0))



### within conditions: frequentist t-test, Cohen's d, and Bayesian t-test against zero
# express condition as one variable (easier for multiple comparisons)
dfCompUnited <- unite(dfComp,"cond",col,freq,sep = "_")

# conduct t-tests for cued > baseline with Bonferroni-Holm correction
tTestsAtt <- t_test(data = group_by(dfCompUnited[dfCompUnited$att == "att",],cond),
                    formula = amplitude ~ 1, alternative = "greater")
tTestsAtt$p.adj <- p.adjust(tTestsAtt$p, method = "holm")
tTestsAtt

# conduct t-tests for uncued < baseline with Bonferroni-Holm correction
tTestsIgn <- t_test(data = group_by(dfCompUnited[dfCompUnited$att == "ign",],cond),
                    formula = amplitude ~ 1, alternative = "less")
tTestsIgn$p.adj <- p.adjust(tTestsIgn$p, method = "holm")
tTestsIgn

# compute Cohen's d & Bayesian t-tests
# attending color at 8.57 Hz
cohens_d(data = dfComp[dfComp$col == "col" & dfComp$freq == "857" & dfComp$att == "att",],
         formula = amplitude ~ 1)
ttestBF(x = dfCompWide$att_col_857, nullInterval = c(0,Inf))

# attending color at 15 Hz
cohens_d(data = dfComp[dfComp$col == "col" & dfComp$freq == "15" & dfComp$att == "att",],
         formula = amplitude ~ 1)
ttestBF(x = dfCompWide$att_col_15, nullInterval = c(0,Inf))

# attending grayscale at 8.57 Hz
cohens_d(data = dfComp[dfComp$col == "gray" & dfComp$freq == "857" & dfComp$att == "att",],
         formula = amplitude ~ 1)
ttestBF(x = dfCompWide$att_gray_857, nullInterval = c(0,Inf))

# attending grayscale at 15 Hz
cohens_d(data = dfComp[dfComp$col == "gray" & dfComp$freq == "15" & dfComp$att == "att",],
         formula = amplitude ~ 1)
ttestBF(x = dfCompWide$att_gray_15, nullInterval = c(0,Inf))


# ignoring color at 8.57 Hz
cohens_d(data = dfComp[dfComp$col == "col" & dfComp$freq == "857" & dfComp$att == "ign",],
         formula = amplitude ~ 1)
ttestBF(x = dfCompWide$ign_col_857, nullInterval = c(-Inf,0))

# ignoring color at 15 Hz
cohens_d(data = dfComp[dfComp$col == "col" & dfComp$freq == "15" & dfComp$att == "ign",],
         formula = amplitude ~ 1)
ttestBF(x = dfCompWide$ign_col_15, nullInterval = c(-Inf,0))

# ignoring grayscale at 8.57 Hz
cohens_d(data = dfComp[dfComp$col == "gray" & dfComp$freq == "857" & dfComp$att == "ign",],
         formula = amplitude ~ 1)
ttestBF(x = dfCompWide$ign_gray_857, nullInterval = c(-Inf,0))

# ignoring grayscale at 15 Hz
cohens_d(data = dfComp[dfComp$col == "gray" & dfComp$freq == "15" & dfComp$att == "ign",],
         formula = amplitude ~ 1)
ttestBF(x = dfCompWide$ign_gray_15, nullInterval = c(-Inf,0))



# ### non-parametric alternative
# # conduct t-tests for cued > baseline with Bonferroni-Holm correction
# WilcoxonAtt <- wilcox_test(data = group_by(dfCompUnited[dfCompUnited$att == "att",],cond),
#                            formula = amplitude ~ 1, alternative = "greater")
# WilcoxonAtt$p.adj <- p.adjust(WilcoxonAtt$p, method = "holm")
# WilcoxonAtt
# 
# # conduct t-tests for uncued < baseline with Bonferroni-Holm correction
# WilcoxonIgn <- wilcox_test(data = group_by(dfCompUnited[dfCompUnited$att == "ign",],cond),
#                            formula = amplitude ~ 1, alternative = "less")
# WilcoxonIgn$p.adj <- p.adjust(WilcoxonIgn$p, method = "holm")
# WilcoxonIgn