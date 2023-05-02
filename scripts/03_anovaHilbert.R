# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.12.0
# --- script version: May 2023
# --- content: run frequentist & Bayesian Attention x Chromaticity x Driving Frequency ANOVAs 
# ---          with mean Hilbert amplitude as DV



### header
# exclude participant with < X % of correct responses; set to 0 to analyse whole sample
excludeLowerThan <- 0
# enable multicore use to speed up bayesian ANOVA (does not work on Windows computers)
doMulticore <- TRUE
# set random seed for reproducibility
randSeed <- 46029393
### end header


# load required packages
library(here)
library(psych)
library(ez)
library(rstatix)
library(BayesFactor)
library(bayestestR)



# Loading Hilbert data for ANOVA into data frame & specify the variables
parentFolder <- here()

loadname <- paste0(parentFolder, "/dataframes/dfHilbertMeans.txt")
dfAnova <- read.csv(loadname, header = TRUE, sep = ",")

dfAnova$partID <- factor(dfAnova$partID)
dfAnova$att <- factor(dfAnova$att, levels = c("att","ign"))
dfAnova$col <- factor(dfAnova$col, levels = c("col","gray"))
dfAnova$freq <- factor(dfAnova$freq, levels = c("857","15"))

# exclude participants with low percentatge of correct answers depending on header parameter
loadname <- paste0(parentFolder, "/performanceData/att2color_eeg_performance.txt")
performanceData <- read.csv(loadname, header = TRUE, sep = ";")

partToExclude <-as.character(performanceData$partID[performanceData$percentCorrect < excludeLowerThan])
dfAnova <- dfAnova[!is.element(dfAnova$partID, partToExclude),]

# describe data separate for all 8 conditions
describeBy(dfAnova, group = list(dfAnova$att,dfAnova$col,dfAnova$freq))

# quick descriptive plot
ezPlot(
  data = dfAnova,
  dv = amplitude,
  wid = partID,
  within = .(att,col,freq),
  x = att,
  split = col,
  col = freq
)

# Frequentist Attention x Color x Driving Frequency ANOVA
anovaFreq <- anova_test(data = dfAnova,
                        dv = amplitude,
                        wid = partID,
                        within = c(att, col, freq),
                        type = 3,
                        effect.size = "pes"); anovaFreq

# Bayesian Attention x Color x Driving Frequency ANOVA
# generalTestBF is used to manually include random slopes for all within-subject effects (cf. van den Bergh, 2022, PsyArx)
set.seed(randSeed); anovaBayes <- generalTestBF(
  amplitude ~ att*col*freq + 
    partID + 
    partID:att + partID:col + partID:freq +
    partID:att:col + partID:att:freq + partID:col:freq,
  data = dfAnova,
  whichRandom = c("partID",  
                  "partID:att", "partID:col", "partID:freq",
                  "partID:att:col", "partID:att:freq", "partID:col:freq"),
  neverExclude = c("partID",  
                   "partID:att", "partID:col", "partID:freq",
                   "partID:att:col", "partID:att:freq", "partID:col:freq"),
  whichModels = "all",
  multicore = doMulticore
)

# compute and show Inclusion Bayes Factors
bfIncl <- bf_inclusion(anovaBayes, match_models = FALSE); bfIncl

# save outputs into txt files (the Bayes ANOVA takes a couple of minutes)
if (excludeLowerThan > 0) {
  savename <- paste0(parentFolder, "/output/anovaResults_excludeLowerThan", as.character(excludeLowerThan), ".txt")
} else {
  savename <- paste0(parentFolder, "/output/anovaResults.txt")
}
sink(savename)
cat("FREQUENTIST ANOVA\n")
print(anovaFreq)
cat("\n\n\nINCLUSION BAYES FACTORS\n")
print(bfIncl)
sink()
