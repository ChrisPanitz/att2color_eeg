# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.12.0
# --- script version: June 2023
# --- content: run frequentist & Bayesian Cue x Chromaticity x Driving Frequency ANOVAs with mean
# ---          Hilbert amplitude as DV, separately for subgroups from supplementary EOG control analysis



### header
# exclude participants with < X % of correct responses; set to 0 to analyse whole sample
excludeLowerThan <- 0
# exclude participants with a higher z value than X
excludeEOGaboveZ <- qnorm(.95) # ~ 1.645
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

# add z values from EOG analysis; high values indicate increment in EOG activity
# from baseline to cued attention task; compute group variable high vs low z
loadname <- paste0(parentFolder, "/zValuesEOG/zVals_EOG.txt")
dfEOG <- read.csv(loadname, header = TRUE, sep = ",")

dfAnova <- merge(dfAnova,dfEOG,by = "partID")
dfAnova$highZ <- rep(0,dim(dfAnova)[1])
dfAnova$highZ[dfAnova$zValEOG > excludeEOGaboveZ] <- 1
dfAnova$highZ <- factor(dfAnova$highZ, levels = c(0,1), labels = c("lowZ","highZ"))

# exclude participants with low percentage of correct answers depending on header parameter
loadname <- paste0(parentFolder, "/performanceData/att2color_eeg_performance.txt")
performanceData <- read.csv(loadname, header = TRUE, sep = ";")

partToExclude <-as.character(performanceData$partID[performanceData$percentCorrect < excludeLowerThan])
dfAnova <- dfAnova[!is.element(dfAnova$partID, partToExclude),]

# describe data separate for all 8 conditions
describeBy(dfAnova, group = list(dfAnova$highZ,dfAnova$att,dfAnova$col,dfAnova$freq))

# compute differential ssVEP for cued vs uncued
dfAgg <- aggregate(amplitude ~ partID + att + zValEOG + highZ, data = dfAnova, FUN = "mean")
dfAgg <- aggregate(amplitude ~ partID + zValEOG + highZ, data = dfAgg, FUN = "diff")
dfAgg$amplitude <- dfAgg$amplitude * (-1)

# scatter plot & correlation for EOG z x differential ssVEP 
plot(dfAgg$zValEOG,dfAgg$amplitude)
corr.test(dfAgg$zValEOG,dfAgg$amplitude, method = "pearson")
corr.test(dfAgg$zValEOG,dfAgg$amplitude, method = "kendall")

# quick descriptive plot
ezPlot(
  data = dfAnova,
  dv = amplitude,
  wid = partID,
  between = .(highZ),
  within = .(att),
  x = att,
  split = highZ
)

# Frequentist EOG Group x Cue x Color x Driving Frequency ANOVA
anovaFreq <- anova_test(data = dfAnova,
                        dv = amplitude,
                        wid = partID,
                        between = c(highZ),
                        within = c(att, col, freq),
                        type = 3,
                        effect.size = "pes"); anovaFreq

# Frequentist Cue x Color x Driving Frequency ANOVA for low EOG z values
anovaFreqLowZ <- anova_test(data = dfAnova[dfAnova$highZ == "lowZ",],
                            dv = amplitude,
                            wid = partID,
                            within = c(att, col, freq),
                            type = 3,
                            effect.size = "pes"); anovaFreqLowZ

# Frequentist Cue x Color x Driving Frequency ANOVA for high EOG z values
anovaFreqHighZ <- anova_test(data = dfAnova[dfAnova$highZ == "highZ",],
                             dv = amplitude,
                             wid = partID,
                             within = c(att, col, freq),
                             type = 3,
                             effect.size = "pes"); anovaFreqHighZ

# Bayesian Attention x Color x Driving Frequency ANOVA for low EOG z values
# generalTestBF is used to manually include random slopes for all within-subject effects (cf. van den Bergh, 2022, PsyArx)
set.seed(randSeed); anovaBayesLowZ <- generalTestBF(
  amplitude ~ att*col*freq + 
    partID + 
    partID:att + partID:col + partID:freq +
    partID:att:col + partID:att:freq + partID:col:freq,
  data = dfAnova[dfAnova$highZ == "lowZ",],
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
bfInclLowZ <- bf_inclusion(anovaBayesLowZ, match_models = FALSE); bfInclLowZ

# Bayesian Attention x Color x Driving Frequency ANOVA for high EOG z values
# generalTestBF is used to manually include random slopes for all within-subject effects (cf. van den Bergh, 2022, PsyArx)
set.seed(randSeed); anovaBayesHighZ <- generalTestBF(
  amplitude ~ att*col*freq + 
    partID + 
    partID:att + partID:col + partID:freq +
    partID:att:col + partID:att:freq + partID:col:freq,
  data = dfAnova[dfAnova$highZ == "highZ",],
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
bfInclHighZ <- bf_inclusion(anovaBayesHighZ, match_models = FALSE); bfInclHighZ

# save outputs into txt files (the Bayes ANOVA takes a couple of minutes)
if (excludeLowerThan > 0) {
  savename <- paste0(parentFolder, "/output/anovaResults_excludeLowerThan", as.character(excludeLowerThan), "_EOGcontrol.txt")
} else {
  savename <- paste0(parentFolder, "/output/anovaResults_EOGcontrol.txt")
}
sink(savename)
cat("FREQUENTIST ANOVA - low EOG z values\n")
print(anovaFreqLowZ)
cat("\n\n\nFREQUENTIST ANOVA - high EOG z values\n")
print(anovaFreqHighZ)
cat("\n\n\nINCLUSION BAYES FACTORS - low EOG z values\n")
print(bfInclLowZ)
cat("\n\n\nINCLUSION BAYES FACTORS - high EOG z values\n")
print(bfInclHighZ)
sink()
