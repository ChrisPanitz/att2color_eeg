# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.12.0
# --- script version: May 2023
# --- content: read single-subject Hilbert data, create & save data frames for
# ---          statistical analyses & plotting in text format



### Header
# values to change freely
chanInd <- 75 # Oz
analysisWinMs <- c(500,3500) # time window for analyses in ms relative to cue onset
blWinMs <- c(-1000,-200) # time window for baseline correction in ms relative to cue onset; set to NA for no correction
  
# values have to match imported data
segmentWinMs <- c(-1600,4000) # edges of time segment in ms, relative to cue onset
sRate <- 500 # sampling rate

### end of Header



# load required packages
library(here)

# analyzed time window in sample points relative to segment start
analysisSamp <- round(c((analysisWinMs[1]-segmentWinMs[1]) * sRate/1000,
                        (analysisWinMs[2]-segmentWinMs[1]) * sRate/1000))
# baseline time window in sample points relative to segment start
blSamp <- round(c((blWinMs[1]-segmentWinMs[1]) * sRate/1000,
                  (blWinMs[2]-segmentWinMs[1]) * sRate/1000))
# vector of time points for x axis
timeVec <- seq(segmentWinMs[1],segmentWinMs[2],1000/sRate)

# Loading performance data from text file
parentFolder <- here()

loadname <- paste0(parentFolder,"/channelLocations/chanLocs_egi129.txt")
chanLocs <- read.csv(loadname, sep = ";")
chanLocs$thetaRadian <- pi/180*chanLocs$theta
chanLocs$x <- chanLocs$radius*sin(chanLocs$thetaRadian)*200
chanLocs$y <- chanLocs$radius*cos(chanLocs$thetaRadian)*200

# create data frame for plotting Hilbert time course
dfHilbertTimecourse <- data.frame(
  partID = factor(),
  att = factor(levels = c("attended","ignored")),
  col = factor(levels = c("color", "grayscale")),
  freq = factor(levels = c("8.57Hz", "15Hz")),
  time = numeric(),
  amplitude = numeric()
)

# create data frame for topographies
dfHilbertTopo <- data.frame(
  partID = factor(),
  att = factor(levels = c("attended","ignored")),
  col = factor(levels = c("color", "grayscale")),
  freq = factor(levels = c("8.57Hz", "15Hz")),
  electrode = character(),
  x = numeric(),
  y = numeric(),
  amplitude = numeric()
)

# create empty data frame with mean responses for ANOVAs and t-tests
dfHilbertMeans <- data.frame(
  partID = character(),
  att = factor(levels = c("att","ign")),
  col = factor(levels = c("col", "gray")),
  freq = factor(levels = c("857", "15")),
  amplitude = numeric()
)

# set parameters for input data files and fill the data frames
fileFolder <- paste0(parentFolder, "/hilbertData/")
fileList <- list.files(path = fileFolder, pattern = "hilbert.txt")

for (fileI in 1:length(fileList)) {
  currData <- read.csv(paste0(fileFolder,fileList[fileI]), header = FALSE, sep = ",")
  # R gets the participant ID and condition from the filename
  currCondList <- strsplit(fileList[fileI], "_")
  currPartID <- currCondList[[1]][1]
  currAtt <- currCondList[[1]][2]
  currCol <- currCondList[[1]][3]
  currFreq <- currCondList[[1]][4]
  
  # if there is a baseline window specified in the header, divide all values by the baseline mean
  if (!is.na(blWinMs[1])) {
    currData <- ((currData / rowMeans(currData[,blSamp[1]:blSamp[2]])) - 1) * 100
  }
  
  # average across all channels in the header and save it in time course data frame
  currTimeCourse <- colMeans(as.matrix(currData[chanInd,]))
  dfHilbertTimecourse <- rbind(dfHilbertTimecourse,
                               cbind(rep(currPartID,length(timeVec)),
                                     rep(currAtt,length(timeVec)),
                                     rep(currCol,length(timeVec)),
                                     rep(currFreq,length(timeVec)),
                                     timeVec,
                                     currTimeCourse)
  )
  
  # average it across time window in header and save it in topography data frame
  currTopo <- rowMeans(as.matrix(currData[,analysisSamp[1]:analysisSamp[2]]))
  dfHilbertTopo <- rbind(dfHilbertTopo,
                         cbind(rep(currPartID,length(dim(chanLocs)[1])),
                               rep(currAtt,length(dim(chanLocs)[1])),
                               rep(currCol,length(dim(chanLocs)[1])),
                               rep(currFreq,length(dim(chanLocs)[1])),
                               chanLocs$name,
                               chanLocs$x,
                               chanLocs$y,
                               currTopo)
  )
  
  # average it across channels and time window in header and save it in means data frame
  currMean <- mean(as.matrix(currData[chanInd, analysisSamp[1]:analysisSamp[2]]))
  dfHilbertMeans[fileI,] <- c(currCondList[[1]][1:4], as.numeric(currMean))
}

# some renaming and setting variable type for the data frames
names(dfHilbertTimecourse) <- c("partID","att","col","freq","time","amplitude")
dfHilbertTimecourse$time <- as.numeric(dfHilbertTimecourse$time)
dfHilbertTimecourse$amplitude <- as.numeric(dfHilbertTimecourse$amplitude)

names(dfHilbertTopo) <- c("partID","att","col","freq","electrode","x","y","amplitude")
dfHilbertTopo$x <- as.numeric(dfHilbertTopo$x)
dfHilbertTopo$y <- as.numeric(dfHilbertTopo$y)
dfHilbertTopo$amplitude <- as.numeric(dfHilbertTopo$amplitude)

dfHilbertMeans$amplitude <- as.numeric(dfHilbertMeans$amplitude)

# saving the data frames
filename <- paste0(parentFolder,"/dataframes/dfHilbertTimecourse.txt")
write.csv(x = dfHilbertTimecourse, file = filename, row.names = FALSE)

filename <- paste0(parentFolder,"/dataframes/dfHilbertTopos.txt")
write.csv(x = dfHilbertTopo, file = filename, row.names = FALSE)

filename <- paste0(parentFolder,"/dataframes/dfHilbertMeans.txt")
write.csv(x = dfHilbertMeans, file = filename,  row.names = FALSE)