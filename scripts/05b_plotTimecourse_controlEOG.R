# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.12.0
# --- script version: July 2023
# --- content: Plot time courses of Hilbert amplitudes to cued and uncued pictures 
# ---          (across color/gray and driving frequencies), separate for participants
# ---          without and with significant changes in EOG from baseline to cued attention phase


### settings header
# general
plotFS <- 8 # plotting font size - different text elements vary in size around this value (+/-2)

# for Hilbert time course plot
analysisWinMs <- c(500,3500) # time window for statistical analyses in ms relative to cue onset
plotWinMs <- c(-1200,3500) # entire time window to be plotted in ms relative to cue onset

# exclude participants with a higher z value than X
excludeEOGaboveZ <- qnorm(.95) # ~ 1.645
### end header



# load required packages
library(here)
library(ggplot2)
library(RColorBrewer)
library(eegUtils)
library(OneR)
library(ggpubr)



# load data into data frame and specify variable types for time course plot
parentFolder <- here()

loadname <- paste0(parentFolder, "/dataframes/dfHilbertTimeCourse.txt")
dfTimecourse <- read.csv(loadname, header = TRUE, sep = ",")

dfTimecourse$partID <- factor(dfTimecourse$partID)
dfTimecourse$att <- factor(dfTimecourse$att, levels = c("att","ign"))
dfTimecourse$col <- factor(dfTimecourse$col, levels = c("col","gray"))
dfTimecourse$freq <- factor(dfTimecourse$freq, levels = c("857","15"))

# add z values from EOG analysis; high values indicate increment in EOG activity
# from baseline to cued attention task; compute group variable high vs low z
loadname <- paste0(parentFolder, "/zValuesEOG/zVals_EOG.txt")
dfEOG <- read.csv(loadname, header = TRUE, sep = ",")

dfTimecourse <- merge(dfTimecourse,dfEOG,by = "partID")
dfTimecourse$highZ <- rep(0,dim(dfTimecourse)[1])
dfTimecourse$highZ[dfTimecourse$zValEOG > excludeEOGaboveZ] <- 1
dfTimecourse$highZ <- factor(dfTimecourse$highZ, levels = c(0,1), labels = c("lowZ","highZ"))

# Grand Average average across chromaticity & driving frequency conditions
dfATTvsIGN <- aggregate(amplitude ~ att + time + highZ, data = dfTimecourse, mean)

# average across chromaticity & driving frequency conditions to get standard deviations
# for the Grand Average time course
aggSD <- aggregate(amplitude ~ att + time + partID + highZ, data = dfTimecourse, mean)
aggSD <- aggregate(amplitude ~ att + time + highZ, data = aggSD, sd)
# ... then convert into standard error of the mean ...
aggSD$amplitude[aggSD$highZ == "lowZ"] <- aggSD$amplitude[aggSD$highZ == "lowZ"] / 
                                sqrt(length(unique(dfTimecourse$partID[aggSD$highZ == "lowZ"])))
aggSD$amplitude[aggSD$highZ == "highZ"] <- aggSD$amplitude[aggSD$highZ == "highZ"] / 
                                sqrt(length(unique(dfTimecourse$partID[aggSD$highZ == "highZ"])))
# ... and add it to the time course data frame
dfATTvsIGN$sem <- aggSD$amplitude

# set minimum and maximum of y axis based on minimum and maximum plotted values
minY <- floor(min((dfATTvsIGN$amplitude[dfATTvsIGN$time >= plotWinMs[1] & dfATTvsIGN$time <= plotWinMs[2]] -
                       dfATTvsIGN$sem[dfATTvsIGN$time >= plotWinMs[1] & dfATTvsIGN$time <= plotWinMs[2]]) * 
                      10)) / 10
maxY <- ceiling(max((dfATTvsIGN$amplitude[dfATTvsIGN$time >= plotWinMs[1] & dfATTvsIGN$time <= plotWinMs[2]] +
                       dfATTvsIGN$sem[dfATTvsIGN$time >= plotWinMs[1] & dfATTvsIGN$time <= plotWinMs[2]]) * 
                      10)) / 10

# plot Grand Average Hilbert time course aggreated across chromaticity and driving frequency conditions
# for participants without significant changes in EOG activity
plotHilbertAgglowZ <- ggplot(dfATTvsIGN[dfATTvsIGN$highZ == "lowZ",]) + 
  theme_classic() +
  geom_rect(xmin = analysisWinMs[1], xmax = analysisWinMs[2], ymin = minY, ymax = maxY, fill = "gray90") + # marks analysis window
  geom_ribbon(aes(x = time, ymin = amplitude-sem, ymax = amplitude+sem, group = att, fill = att), alpha = .3) + # marks SEM around amplitude
  geom_line(aes(x = time, y = amplitude, group = att, color = att), linewidth = 1.5) + # Hilbert amplitude
  scale_color_manual(name = "Stimulus Array", labels = c("attended", "ignored"),
                     values = brewer.pal(n = 9, "Blues")[c(8,5)]) + # coloring
  scale_fill_manual(name = "Stimulus Array", labels = c("attended", "ignored"),
                    values = brewer.pal(n = 9, "Blues")[c(8,5)]) + # coloring
  scale_x_continuous(name = "Time relative to cue onset (ms)", breaks = seq(-1000,4000,500), limits = plotWinMs) + # x axis
  scale_y_continuous(name = "Amplitude (% rel. to baseline)", expand = c(0,0), limits = c(minY,maxY)) + # y axis
  labs(title = "without EOG changes") +
  theme(
    # legend in top left without title
    legend.position = c(.15,.9),
    legend.title = element_blank(),
    legend.text = element_text(size = plotFS),
    # some more axis formatting
    axis.title.x = element_text(margin = margin(10,0,0,0), size = plotFS+2, face = "bold"),
    axis.title.y = element_text(size = plotFS+2, face = "bold"),
    axis.line = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.text = element_text(color = "black", size = plotFS+1),
    plot.title = element_text(face = "bold", hjust = .5)
  )

# plot Grand Average Hilbert time course aggreated across chromaticity and driving frequency conditions
# for participants with significant changes in EOG activity
plotHilbertAgghighZ <- ggplot(dfATTvsIGN[dfATTvsIGN$highZ == "highZ",]) + 
  theme_classic() +
  geom_rect(xmin = analysisWinMs[1], xmax = analysisWinMs[2], ymin = minY, ymax = maxY, fill = "gray90") + # marks analysis window
  geom_ribbon(aes(x = time, ymin = amplitude-sem, ymax = amplitude+sem, group = att, fill = att), alpha = .3) + # marks SEM around amplitude
  geom_line(aes(x = time, y = amplitude, group = att, color = att), linewidth = 1.5) + # Hilbert amplitude
  scale_color_manual(name = "Stimulus Array", labels = c("attended", "ignored"),
                     values = brewer.pal(n = 9, "Blues")[c(8,5)]) + # coloring
  scale_fill_manual(name = "Stimulus Array", labels = c("attended", "ignored"),
                    values = brewer.pal(n = 9, "Blues")[c(8,5)]) + # coloring
  scale_x_continuous(name = "Time relative to cue onset (ms)", breaks = seq(-1000,4000,500), limits = plotWinMs) + # x axis
  scale_y_continuous(name = "Amplitude (% rel. to baseline)", expand = c(0,0), limits = c(minY,maxY)) + # y axis
  labs(title = "with EOG changes") +
  theme(
    # legend in top left without title
    legend.position = c(.15,.9),
    legend.title = element_blank(),
    legend.text = element_text(size = plotFS),
    # some more axis formatting
    axis.title.x = element_text(margin = margin(10,0,0,0), size = plotFS+2, face = "bold"),
    axis.title.y = element_text(size = plotFS+2, face = "bold"),
    axis.line = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.text = element_text(color = "black", size = plotFS+1),
    plot.title = element_text(face = "bold", hjust = .5)
  )


### arrange both time course plots in one graph
# add some margins to the individual plots
plotHilbertAgglowZ <- plotHilbertAgglowZ + theme(plot.margin = unit(c(10, 10, 10, 10), unit = "pt"))
plotHilbertAgghighZ <- plotHilbertAgghighZ + theme(plot.margin = unit(c(10, 10, 10, 10), unit = "pt"))

# put plots together
plotCombined <- ggarrange(plotHilbertAgglowZ, plotHilbertAgghighZ,
                          nrow = 1, ncol = 2)



### save figure
ggsave(filename = paste0(parentFolder, "/figures/02b_hilbertTimecourse_controlEOG.pdf"), 
       plot = plotCombined,
       device = "pdf",
       width = 21, height = 10, units = "cm")
