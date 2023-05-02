# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.12.0
# --- script version: May 2023
# --- content: t-tests against zero within all conditions and for aggregated 
# ---          across all attended and all ignored conditions



# load required packages
library(here)
library(tidyr)
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



### aggregated data: frequentist t-test, Cohen's d, and Bayesian t-test against zero
# all attended conditions aggregated
t_test(data = dfCompWide, formula = att_aggregated ~ 1)
cohens_d(data = dfCompWide, formula = att_aggregated ~ 1)
ttestBF(x = dfCompWide$att_aggregated)

# all ignored conditions aggregated
t_test(data = dfCompWide, formula = ign_aggregated ~ 1)
cohens_d(data = dfCompWide, formula = ign_aggregated ~ 1)
ttestBF(x = dfCompWide$ign_aggregated)



### within conditions: frequentist t-test, Cohen's d, and Bayesian t-test against zero
# attending color at 8.57 Hz
t_test(data = dfComp[dfComp$col == "col" & dfComp$freq == "857" & dfComp$att == "att",],
       formula = amplitude ~ 1)
cohens_d(data = dfComp[dfComp$col == "col" & dfComp$freq == "857" & dfComp$att == "att",],
         formula = amplitude ~ 1)
ttestBF(x = dfCompWide$att_col_857)

# attending color at 15 Hz
t_test(data = dfComp[dfComp$col == "col" & dfComp$freq == "15" & dfComp$att == "att",],
       formula = amplitude ~ 1)
cohens_d(data = dfComp[dfComp$col == "col" & dfComp$freq == "15" & dfComp$att == "att",],
         formula = amplitude ~ 1)
ttestBF(x = dfCompWide$att_col_15)

# attending grayscale at 8.57 Hz
t_test(data = dfComp[dfComp$col == "gray" & dfComp$freq == "857" & dfComp$att == "att",],
       formula = amplitude ~ 1)
cohens_d(data = dfComp[dfComp$col == "gray" & dfComp$freq == "857" & dfComp$att == "att",],
         formula = amplitude ~ 1)
ttestBF(x = dfCompWide$att_gray_857)

# attending grayscale at 15 Hz
t_test(data = dfComp[dfComp$col == "gray" & dfComp$freq == "15" & dfComp$att == "att",],
       formula = amplitude ~ 1)
cohens_d(data = dfComp[dfComp$col == "gray" & dfComp$freq == "15" & dfComp$att == "att",],
         formula = amplitude ~ 1)
ttestBF(x = dfCompWide$att_gray_15)


# ignoring color at 8.57 Hz
t_test(data = dfComp[dfComp$col == "col" & dfComp$freq == "857" & dfComp$att == "ign",],
       formula = amplitude ~ 1)
cohens_d(data = dfComp[dfComp$col == "col" & dfComp$freq == "857" & dfComp$att == "ign",],
         formula = amplitude ~ 1)
ttestBF(x = dfCompWide$ign_col_857)

# ignoring color at 15 Hz
t_test(data = dfComp[dfComp$col == "col" & dfComp$freq == "15" & dfComp$att == "ign",],
       formula = amplitude ~ 1)
cohens_d(data = dfComp[dfComp$col == "col" & dfComp$freq == "15" & dfComp$att == "ign",],
         formula = amplitude ~ 1)
ttestBF(x = dfCompWide$ign_col_15)

# ignoring grayscale at 8.57 Hz
t_test(data = dfComp[dfComp$col == "gray" & dfComp$freq == "857" & dfComp$att == "ign",],
       formula = amplitude ~ 1)
cohens_d(data = dfComp[dfComp$col == "gray" & dfComp$freq == "857" & dfComp$att == "ign",],
         formula = amplitude ~ 1)
ttestBF(x = dfCompWide$ign_gray_857)

# ignoring grayscale at 15 Hz
t_test(data = dfComp[dfComp$col == "gray" & dfComp$freq == "15" & dfComp$att == "ign",],
       formula = amplitude ~ 1)
cohens_d(data = dfComp[dfComp$col == "gray" & dfComp$freq == "15" & dfComp$att == "ign",],
         formula = amplitude ~ 1)
ttestBF(x = dfCompWide$ign_gray_15)