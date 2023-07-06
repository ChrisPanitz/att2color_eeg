# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.12.0
# --- script version: July 2023
# --- content: Permutation test for comparing cued vs baseline, uncued vs baseline, & cued vs uncued


### settings header
# general
randSeed <- 46029393

# for Hilbert time course plot
#analysisWinMs <- c(500,3500) # time window for statistical analyses in ms relative to cue onset
#plotWinMs <- c(-1200,3500) # entire time window to be plotted in ms relative to cue onset
nrRuns <- 1000
clustThreshold <- .05
sigThreshold <- .05
### end header



# load required packages
library(here)
library(ggplot2)
#library(RColorBrewer)
#library(eegUtils)
#library(OneR)
library(ggpubr)



# load data into data frame and specify variable types for time course plot
parentFolder <- here()

loadname <- paste0(parentFolder, "/dataframes/dfHilbertTimeCourse.txt")
dfTimecourse <- read.csv(loadname, header = TRUE, sep = ",")

dfTimecourse$partID <- factor(dfTimecourse$partID)
dfTimecourse$att <- factor(dfTimecourse$att, levels = c("att","ign"))
dfTimecourse$col <- factor(dfTimecourse$col, levels = c("col","gray"))
dfTimecourse$freq <- factor(dfTimecourse$freq, levels = c("857","15"))

# Grand Average average across chromaticity & driving frequency conditions
library(tidyr)
dfATTvsIGN <- aggregate(amplitude ~ partID + att + time, data = dfTimecourse, mean)
ampMatAtt <- pivot_wider(data = dfATTvsIGN[dfATTvsIGN$att == "att",], id_cols = partID, names_from = time, values_from = amplitude)
ampMatAtt <- as.matrix(ampMatAtt[,-1])
ampMatIgn <- pivot_wider(data = dfATTvsIGN[dfATTvsIGN$att == "ign",], id_cols = partID, names_from = time, values_from = amplitude)
ampMatIgn <- as.matrix(ampMatIgn[,-1])
ampMatDiff <- aggregate(amplitude ~ partID + time, data = dfATTvsIGN, diff)
ampMatDiff$amplitude <- ampMatDiff$amplitude * (-1)
ampMatDiff <- pivot_wider(data = ampMatDiff, id_cols = partID, names_from = time, values_from = amplitude)
ampMatDiff <- as.matrix(ampMatDiff[,-1])
timeVec <- sort(unique(dfATTvsIGN$time))

# compute t-value for each sample (for cued vs BL, uncued vs BL and cued vs uncued)
ampMatAttT <- apply(ampMatAtt,2,mean) / apply(ampMatAtt,2,sd) * sqrt(dim(ampMatAtt)[1])
ampMatIgnT <- apply(ampMatIgn,2,mean) / apply(ampMatIgn,2,sd) * sqrt(dim(ampMatIgn)[1])
ampMatDiffT <- apply(ampMatDiff,2,mean) / apply(ampMatDiff,2,sd) * sqrt(dim(ampMatDiff)[1])

# empirical t-values (i.e., from original data)
# get booleans for signficance and pad them with one FALSE per side to catch edges
edgesLowIgnEmp <- diff(c(FALSE, ampMatIgnT < qt(p = clustThreshold, df = dim(ampMatIgn)[1], ncp = 0), FALSE))
edgesHighAttEmp <- diff(c(FALSE, ampMatAttT > qt(p = 1-clustThreshold, df = dim(ampMatAtt)[1], ncp = 0), FALSE))
edgesHighDiffEmp <- diff(c(FALSE, ampMatDiffT > qt(p = 1-clustThreshold, df = dim(ampMatDiff)[1], ncp = 0), FALSE))

# for each test (each of 3 comparisons with each of two directions of the effect)
# identify clusters of significant t-values and sum up t-values as cluster statistic
tSumsLowIgnEmp <- 0
nrClusters <- sum(edgesLowIgnEmp == 1)
clusterIndLowIgnEmp <- array(dim = c(nrClusters,2))
if (nrClusters > 0){
  for (i in 1:nrClusters){
    clusterIndLowIgnEmp[i,] <- c(which(edgesLowIgnEmp == 1)[i], which(edgesLowIgnEmp == -1)[i]-1)
    tSumsLowIgnEmp[i] <- sum(ampMatIgnT[clusterIndLowIgnEmp[i,1] : clusterIndLowIgnEmp[i,2]])  
  }
}
tSumsHighAttEmp <- 0
nrClusters <- sum(edgesHighAttEmp == 1)
clusterIndHighAttEmp <- array(dim = c(nrClusters,2))
if (nrClusters > 0){
  for (i in 1:nrClusters){
    clusterIndHighAttEmp[i,] <- c(which(edgesHighAttEmp == 1)[i], which(edgesHighAttEmp == -1)[i]-1)
    tSumsHighAttEmp[i] <- sum(ampMatAttT[clusterIndHighAttEmp[i,1] : clusterIndHighAttEmp[i,2]])  
  }
}
tSumsHighDiffEmp <- 0
nrClusters <- sum(edgesHighDiffEmp == 1)
clusterIndHighDiffEmp <- array(dim = c(nrClusters,2))
if (nrClusters > 0){
  for (i in 1:nrClusters){
    clusterIndHighDiffEmp[i,] <- c(which(edgesHighDiffEmp == 1)[i], which(edgesHighDiffEmp == -1)[i]-1)
    tSumsHighDiffEmp[i] <- sum(ampMatDiffT[clusterIndHighDiffEmp[i,1] : clusterIndHighDiffEmp[i,2]])  
  }
}

# define vectors to collect cluster statistic in each iteration
tVecMinIgn <- array()
tVecMaxAtt <- array()
tVecMaxDiff <- array()

# create reference distribution by means of permutating condition labels for each participant
set.seed(randSeed)
for (run in 1:nrRuns){
  # randomly decide whose condition labels get switched
  switchInd <- sample(c(-1,1),dim(ampMatAtt)[1],replace = TRUE)
  tempMatAtt <- ampMatAtt * switchInd
  tempMatIgn <- ampMatIgn * switchInd
  tempMatDiff <- ampMatDiff * switchInd
  
  # compute sample-wise t-values for the shuffled time courses
  tempTAtt <- apply(tempMatAtt,2,mean) / apply(tempMatAtt,2,sd) * sqrt(dim(tempMatAtt)[1])
  tempTIgn <- apply(tempMatIgn,2,mean) / apply(tempMatIgn,2,sd) * sqrt(dim(tempMatIgn)[1])
  tempTDiff <- apply(tempMatDiff,2,mean) / apply(tempMatDiff,2,sd) * sqrt(dim(tempMatDiff)[1])
  
  # get booleans for signficance and pad them with one zero per side to catch edges
  edgesLowIgn <- diff(c(0, tempTAtt < qt(p = .05, df = 23, ncp = 0), 0))
  edgesHighAtt <- diff(c(0, tempTAtt > qt(p = .95, df = 23, ncp = 0), 0))
  edgesHighDiff <- diff(c(0, tempTAtt > qt(p = .95, df = 23, ncp = 0), 0))
  
  # identify clusters of significant t-values and sum up t-values as cluster statistic
  tSumsLowIgn <- 0
  nrClusters <- sum(edgesLowIgn == 1)
  if (nrClusters > 0){
    for (i in 1:nrClusters){
      tSumsLowIgn[i] <- sum(tempTIgn[which(edgesLowIgn == 1)[i] : which(edgesLowIgn == -1)[i]-1])
    }
  }
  tSumsHighAtt <- 0
  nrClusters <- sum(edgesHighAtt == 1)
  if (nrClusters > 0){
    for (i in 1:nrClusters){
      tSumsHighAtt[i] <- sum(tempTAtt[which(edgesHighAtt == 1)[i] : which(edgesHighAtt == -1)[i]-1])
    }
  }
  tSumsHighDiff <- 0
  nrClusters <- sum(edgesHighDiff == 1)
  if (nrClusters > 0){
    for (i in 1:nrClusters){
      tSumsHighDiff[i] <- sum(tempTDiff[which(edgesHighDiff == 1)[i] : which(edgesHighDiff == -1)[i]-1])
    }
  }
  
  tVecMinIgn <- c(tVecMinIgn,min(tSumsLowIgn))
  tVecMaxAtt <- c(tVecMaxAtt,max(tSumsHighAtt))
  tVecMaxDiff <- c(tVecMaxDiff,max(tSumsHighDiff))
}

# plot distribution of cluster statistics via shuffling
hist(tVecMinIgn)
hist(tVecMaxAtt)
hist(tVecMaxDiff)

# return critical cluster statistics
sort(tVecMinIgn)[round(sigThreshold*length(tVecMinIgn))]
sort(tVecMaxAtt)[round((1-sigThreshold)*length(tVecMaxAtt))]
sort(tVecMaxDiff)[round((1-sigThreshold)*length(tVecMaxDiff))]

# plot the empirical data with the cluster thresholds
plotIgnBl <- ggplot() +
  geom_line(aes(x = timeVec, y = ampMatIgnT)) +
  geom_hline(aes(yintercept = qt(p = clustThreshold, df = dim(ampMatIgn)[1], ncp = 0))) +
  labs(title = "ignored < baseline", x = "time in ms", y = "t-value") +
  scale_y_continuous(limits = c(-3,5))
plotAttBl <- ggplot() +
  geom_line(aes(x = timeVec, y = ampMatAttT)) +
  geom_hline(aes(yintercept = qt(p = 1-clustThreshold, df = dim(ampMatAtt)[1], ncp = 0))) +
  labs(title = "attended > baseline", x = "time in ms", y = "t-value") +
  scale_y_continuous(limits = c(-3,5))
plotAttIgn <- ggplot() +
  geom_line(aes(x = timeVec, y = ampMatDiffT)) +
  geom_hline(aes(yintercept = qt(p = 1-clustThreshold, df = dim(ampMatDiff)[1], ncp = 0))) +
  labs(title = "attended > ignored", x = "time in ms", y = "t-value") +
  scale_y_continuous(limits = c(-3,5))

plotComb <- ggarrange(plotAttBl,plotIgnBl,plotAttIgn,
                      nrow = 1, ncol = 3)
plotComb

# identify which clusters are significant (i.e. above the critical t-sum threshold)
sigClusterIndIgnLow <- which(tSumsLowIgnEmp > sort(tVecMinIgn)[round((1-sigThreshold/2)*length(tVecMinIgn))])
sigClusterSampIgnLow <- clusterIndLowIgnEmp[sigClusterIndIgnLow,]

sigClusterIndAttHigh <- which(tSumsHighAttEmp > sort(tVecMaxAtt)[round((1-sigThreshold/2)*length(tVecMaxAtt))])
sigClusterSampAttHigh <- clusterIndHighAttEmp[sigClusterIndAttHigh,]

sigClusterIndDiffHigh <- which(tSumsHighDiffEmp > sort(tVecMaxDiff)[round((1-sigThreshold/2)*length(tVecMaxDiff))])
sigClusterSampDiffHigh <- clusterIndHighDiffEmp[sigClusterIndDiffHigh,]

# export them (no clusters for ignored/uncued pictures)
filename <- paste0(parentFolder, "/output/indicesSigPermClustersDiff.txt")
if (length(dim(sigClusterSampDiffHigh)) == 0){
  write.csv(t(sigClusterSampDiffHigh), file = filename, row.names = FALSE)
} else {
  write.csv(sigClusterSampDiffHigh, file = filename, row.names = FALSE) 
}

filename <- paste0(parentFolder, "/output/indicesSigPermClustersAtt.txt")
if (length(dim(sigClusterSampAttHigh)) == 0){
  write.csv(t(sigClusterSampAttHigh), file = filename, row.names = FALSE)
} else {
  write.csv(sigClusterSampAttHigh, file = filename, row.names = FALSE) 
}
