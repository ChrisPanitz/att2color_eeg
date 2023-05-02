# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.12.0
# --- script version: May 2023
# --- content: Plot time course and topographies of Hilbert amplitudes to 
# ---          attended and ignored pictures (across color/gray and driving frequencies)


### settings header
# general
plotFS <- 11 # plotting font size - different text elements vary in size around this value (+/-2)

# for Hilbert time course plot
analysisWinMs <- c(500,3500) # time window for statistical analyses in ms relative to cue onset
plotWinMs <- c(-1200,3500) # entire time window to be plotted in ms relative to cue onset

# for topographies
topoRes <- 200 # resolution of topographies (default  = 200 per edge)
nrColors <- 9 # color map of topographies will be binned into X distinct colors
interpMethod <- "gam" # interpolation either "gam" (Generalized Additive Model) or "biharmonic" (Spline)
highlightChans <- c("E75") # names of channels to highlight
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

# Grand Average average across chromaticity & driving frequency conditions
dfATTvsIGN <- aggregate(amplitude ~ att + time, data = dfTimecourse, mean)

# average across chromaticity & driving frequency conditions to get standard deviations
# for the Grand Average time course
aggSD <- aggregate(amplitude ~ att + time + partID, data = dfTimecourse, mean)
aggSD <- aggregate(amplitude ~ att + time, data = aggSD, sd)
# ... then convert into standard error of the mean ...
aggSD$amplitude <- aggSD$amplitude / sqrt(length(levels(dfTimecourse$partID)))
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
plotHilbertAgg <- ggplot(dfATTvsIGN) + 
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
  theme(
    # legend in top left without title
    legend.position = c(.1,.9),
    legend.title = element_blank(),
    legend.text = element_text(size = plotFS),
    # some more axis formatting
    axis.title.x = element_text(margin = margin(10,0,0,0), size = plotFS+2, face = "bold"),
    axis.title.y = element_text(size = plotFS+2, face = "bold"),
    axis.line = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.text = element_text(color = "black", size = plotFS+1)
  )



# load data into data frame for topographies
loadname <- paste0(parentFolder, "/dataframes/dfHilbertTopos.txt")
dfTopos <- read.csv(loadname)

# average topographiies across chromaticity and driving frequency conditions
dfTopos <- aggregate(amplitude ~ att + electrode + x + y, data = dfTopos, mean)

# some data organization
# create dummy topographies to get min and max values for scaling
topoAtt <- topoplot(data = dfTopos[dfTopos$att == "att",], method = interpMethod)
topoIgn <- topoplot(data = dfTopos[dfTopos$att == "ign",], method = interpMethod)
# get min / max / max absolute value from all data points across topographies
topoData <- c(topoAtt$data$fill,topoIgn$data$fill)
minAmp <- min(topoData)
maxAmp <- max(topoData)
absAmp <- max(abs(c(minAmp,maxAmp)))

# compute difference topography ...
topoDataDiff <- topoAtt$data$fill - topoIgn$data$fill
# and get min / max / max absolute value from difference topography
minAmpDiff <- min(topoDataDiff)
maxAmpDiff <- max(topoDataDiff)
absAmpDiff <- max(abs(c(minAmpDiff,maxAmpDiff)))



### here comes the actual plotting ###

# topography for attended stimuli - symmetrically scaled around zero
topoAtt <- topoplot(data = dfTopos[dfTopos$att == "att",],
                    contour = FALSE, scaling = 0.05, highlights = highlightChans,
                    grid_res = topoRes, limits = c(-absAmp,absAmp), method = interpMethod) +
  labs(title = "attended") + # plot title
  theme(legend.position = "bottom",
        legend.text = element_text(size = plotFS-2, angle = 0, vjust = .5, face = "plain"),
        plot.title = element_text(hjust = 0.5, size = plotFS+2, face = "bold"))

# some manual editing of the color bar
topoAtt$guides$fill$barwidth <- unit(6, "lines")
topoAtt$guides$fill$barheight <- unit(.5, "lines")
topoAtt$guides$fill$title <- "% change"
topoAtt$guides$fill$title.theme$size <- plotFS
topoAtt$guides$fill$title.theme$angle <- 0
topoAtt$guides$fill$title.position <- "top"
topoAtt$guides$fill$title.hjust <- 0.5
topoAtt$guides$fill$nbin <- nrColors



# topography for ignored stimuli - symmetrically scaled around zero
topoIgn <- topoplot(data = dfTopos[dfTopos$att == "ign",],
                    contour = FALSE, scaling = 0.05, highlights = highlightChans,
                    grid_res = topoRes, limits = c(-absAmp,absAmp), method = interpMethod) +
  labs(title = "ignored") + # plot title
  theme(legend.position = "bottom",
        legend.text = element_text(size = plotFS-2, angle = 0, vjust = .5),
        plot.title = element_text(hjust = 0.5, size = plotFS+2, face = "bold"))

# some manual editing of the color bar
topoIgn$guides$fill$barwidth <- unit(6, "lines")
topoIgn$guides$fill$barheight <- unit(.5, "lines")
topoIgn$guides$fill$title <- "% change"
topoIgn$guides$fill$title.theme$size <- plotFS
topoIgn$guides$fill$title.theme$angle <- 0
topoIgn$guides$fill$title.position <- "top"
topoIgn$guides$fill$title.hjust <- 0.5
topoIgn$guides$fill$nbin <- nrColors



# bin color data across atttention and ignoring conditions (resulting values: 1 to [nrColors])
fillAll <- as.numeric(bin(data = topoData, nbins = nrColors))

# scale it back to original scale
fillAll <- (fillAll-1) / (nrColors-1)
fillAll <- fillAll*(maxAmp-minAmp) + minAmp

# replace data in topography objects with binned data
topoPoints <- length(fillAll) / 2
topoAtt$data$fill <- fillAll[1 : topoPoints]
topoIgn$data$fill <- fillAll[(topoPoints+1) : (2*topoPoints)]



# difference topography... will be generated with wrong data, data will be replaced afterwards
# (we preferred difference of interpolated data to interpolation of difference data)
topoDiff <- topoplot(data = dfTopos[dfTopos$att == "att",],
                     contour = FALSE, scaling = 0.05, highlights = highlightChans,
                     grid_res = topoRes, limits = c(-absAmpDiff, absAmpDiff), method = interpMethod) +
  labs(title = "attended - ignored") + # plot title
  theme(legend.position = "bottom",
        legend.text = element_text(size = plotFS-2, angle = 0, vjust = .5),
        plot.title = element_text(hjust = 0.5, size = plotFS+2, face = "bold"))

# some manual editing of the color bar
topoDiff$guides$fill$barwidth <- unit(6, "lines")
topoDiff$guides$fill$barheight <- unit(.5, "lines")
topoDiff$guides$fill$title <- expression(paste(Delta, " % change"))
topoDiff$guides$fill$title.theme$size <- plotFS
topoDiff$guides$fill$title.theme$angle <- 0
topoDiff$guides$fill$title.position <- "top"
topoDiff$guides$fill$title.hjust <- 0.5
topoDiff$guides$fill$nbin <- nrColors

# bin data of difference topography and scale binned data back to original limits
# then put it back into topography object
topoDataDiff <- as.numeric(bin(data = as.numeric(topoDataDiff), nbins = nrColors))
topoDataDiff <- (topoDataDiff-1) / (nrColors-1)
topoDataDiff <- topoDataDiff*(maxAmpDiff-minAmpDiff) + minAmpDiff
topoDiff$data$fill <- topoDataDiff



### arrange all time course and topography plots in one graph

# add some margins to the individual plots
plotHilbertAgg <- plotHilbertAgg + theme(plot.margin = unit(c(10, 10, 20, 10), unit = "pt"))
topoAtt <- topoAtt + theme(plot.margin = unit(c(20, 0, 0, 0), unit = "pt"),
                           legend.margin = margin(0,0,10,0))
topoIgn <- topoIgn + theme(plot.margin = unit(c(20, 0, 0, 0), unit = "pt"),
                           legend.margin = margin(0,0,10,0))
topoDiff <- topoDiff + theme(plot.margin = unit(c(20, 0, 0, 0), unit = "pt"),
                             legend.margin = margin(0,0,5,0))

# put plots together
plotCombined <- ggarrange(plotHilbertAgg,
                          ggarrange(topoAtt, topoIgn, topoDiff,
                                    nrow = 1, ncol = 3),
                          nrow = 2, ncol = 1, heights = c(2,2),
                          labels = c("A", "B"))



### save figure
ggsave(filename = paste0(parentFolder, "/figures/02_hilbertTimecourseAndTopos.pdf"), 
       plot = plotCombined,
       device = "pdf",
       width = 21, height = 19, units = "cm")