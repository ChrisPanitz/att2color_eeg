# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.12.0
# --- script version: July 2023
# --- content: pairwise comparisons between cued and uncued pictures within chromaticity and driving frequency conditions



# load required packages
library(here)
library(tidyr)
library(rstatix)
library(BayesFactor)



# Loading Hilbert data for pairwise comparisons
parentFolder <- here()

loadname <- paste0(parentFolder, "/dataframes/dfHilbertMeans.txt")
dfComp <- read.csv(loadname, header = TRUE, sep = ",")

# specify variable types
dfComp$partID <- factor(dfComp$partID)
dfComp$att <- factor(dfComp$att, levels = c("att","ign"))
dfComp$col <- factor(dfComp$col, levels = c("col","gray"))
dfComp$freq <- factor(dfComp$freq, levels = c("857","15"))

# express condition as one variable
dfCompUnited <- unite(dfComp,"cond",att,col,freq,sep = "_")

# conduct pairwise t-tests with Bonferroni-Holm correction
pairwise_t_test(data = dfCompUnited, formula = amplitude ~ cond, 
                comparisons = list(c("att_col_857","ign_col_857"),
                                   c("att_col_15","ign_col_15"),
                                   c("att_gray_857","ign_gray_857"),
                                   c("att_gray_15","ign_gray_15")),
                p.adjust.method = "holm", paired = TRUE, alternative = "greater")

# transform into wide format
dfCompWide <- pivot_wider(data = dfComp,
                          id_cols = partID,
                          names_from = c(att,col,freq),
                          values_from = amplitude)

### within each condition, Cohen's d, and Bayesian t-test are computed
# attended vs ignored for color pictures flickered at 8.57 Hz
cohens_d(data = dfComp[dfComp$col == "col" & dfComp$freq == "857",],
         formula = amplitude ~ att, paired = TRUE)
ttestBF(x = dfCompWide$att_col_857, y = dfCompWide$ign_col_857, paired = TRUE, nullInterval = c(0,Inf))

# attended vs ignored for color pictures flickered at 15 Hz
cohens_d(data = dfComp[dfComp$col == "col" & dfComp$freq == "15",],
         formula = amplitude ~ att, paired = TRUE)
ttestBF(x = dfCompWide$att_col_15, y = dfCompWide$ign_col_15, paired = TRUE, nullInterval = c(0,Inf))

# attended vs ignored for grayscale pictures flickered at 8.57 Hz
cohens_d(data = dfComp[dfComp$col == "gray" & dfComp$freq == "857",],
         formula = amplitude ~ att, paired = TRUE)
ttestBF(x = dfCompWide$att_gray_857, y = dfCompWide$ign_gray_857, paired = TRUE, nullInterval = c(0,Inf))

# attended vs ignored for grayscale pictures flickered at 15 Hz
cohens_d(data = dfComp[dfComp$col == "gray" & dfComp$freq == "15",],
         formula = amplitude ~ att, paired = TRUE)
ttestBF(x = dfCompWide$att_gray_15, y = dfCompWide$ign_gray_15, paired = TRUE, nullInterval = c(0,Inf))



# ### non-parametric alternative
# pairwise_wilcox_test(data = dfCompUnited, formula = amplitude ~ cond, 
#                      comparisons = list(c("att_col_857","ign_col_857"),
#                                         c("att_col_15","ign_col_15"),
#                                         c("att_gray_857","ign_gray_857"),
#                                         c("att_gray_15","ign_gray_15")),
#                      p.adjust.method = "holm", paired = TRUE, alternative = "greater")