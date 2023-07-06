# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.12.0
# --- script version: July 2023
# --- content: summarize and plot performance data

# load required packages
library(here)
library(psych)

# Loading performance data from text file
parentFolder <- here()
filename <- paste0(parentFolder, "/performanceData/att2color_eeg_performance.txt")
dfPerformance <- read.csv(filename, header = TRUE, sep = ";")

# Descriptive statistics
describe(dfPerformance)

# quick histograms for percentage of correct answers and number of trials without response
hist(dfPerformance$percentCorrect, breaks = seq(0,100,10))
hist(dfPerformance$nrRatingsTimedOut,
     breaks = -1:max(dfPerformance$nrRatingsTimedOut))

# ... for percentage of misses (underestimates) in trials with only baseline targets and trials with only cued targets
hist(dfPerformance$missesBL, breaks = seq(0,100,10))
hist(dfPerformance$missesCue, breaks = seq(0,100,10))

# ... for percentage of reporting zero targets in trials without targets and with at least one target
hist(dfPerformance$repZeroTargetsAbsent, breaks = seq(0,100,10))
hist(dfPerformance$repZeroTargetsPresent, breaks = seq(0,100,10))