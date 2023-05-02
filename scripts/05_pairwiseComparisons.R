# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.12.0
# --- script version: May 2023
# --- content: pairwise comparisons between attended and ignored pictures (a) for values aggregated
# --- across all attended & ignored conditions respectively, and (b) within chromaticity and driving frequency conditions



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

# transform into wide format
dfCompWide <- pivot_wider(data = dfComp,
                          id_cols = partID,
                          names_from = c(att,col,freq),
                          values_from = amplitude)

### within each condition, frequentist t-test, Cohen's d, and Bayesian t-test are computed
# attended vs ignored for color pictures flickered at 8.57 Hz
t_test(data = dfComp[dfComp$col == "col" & dfComp$freq == "857",],
       formula = amplitude ~ att, paired = TRUE)
cohens_d(data = dfComp[dfComp$col == "col" & dfComp$freq == "857",],
         formula = amplitude ~ att, paired = TRUE)
ttestBF(x = dfCompWide$att_col_857, y = dfCompWide$ign_col_857, paired = TRUE)

# attended vs ignored for color pictures flickered at 15 Hz
t_test(data = dfComp[dfComp$col == "col" & dfComp$freq == "15",],
       formula = amplitude ~ att, paired = TRUE)
cohens_d(data = dfComp[dfComp$col == "col" & dfComp$freq == "15",],
         formula = amplitude ~ att, paired = TRUE)
ttestBF(x = dfCompWide$att_col_15, y = dfCompWide$ign_col_15, paired = TRUE)

# attended vs ignored for grayscale pictures flickered at 8.57 Hz
t_test(data = dfComp[dfComp$col == "gray" & dfComp$freq == "857",],
       formula = amplitude ~ att, paired = TRUE)
cohens_d(data = dfComp[dfComp$col == "gray" & dfComp$freq == "857",],
         formula = amplitude ~ att, paired = TRUE)
ttestBF(x = dfCompWide$att_gray_857, y = dfCompWide$ign_gray_857, paired = TRUE)

# attended vs ignored for grayscale pictures flickered at 15 Hz
t_test(data = dfComp[dfComp$col == "gray" & dfComp$freq == "15",],
       formula = amplitude ~ att, paired = TRUE)
cohens_d(data = dfComp[dfComp$col == "gray" & dfComp$freq == "15",],
         formula = amplitude ~ att, paired = TRUE)
ttestBF(x = dfCompWide$att_gray_15, y = dfCompWide$ign_gray_15, paired = TRUE)
