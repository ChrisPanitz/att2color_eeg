# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.12.0
# --- script version: May 2023
# --- content: plot raincloud plots for all conditions using the ggrain package (Allen et al)



# Header
plotFS <- 12 # general font size, slightly varies between single text elements

# load required packages
library(here)
library(ggplot2)
library(RColorBrewer)
library(ggrain)
library(ggpubr)

# load data into data frame
parentFolder <- here()

loadname <- paste0(parentFolder, "/dataframes/dfHilbertMeans.txt")
dfMeans <- read.csv(loadname, header = TRUE, sep = ",")

# specify varibalevariable types
dfMeans$partID <- factor(dfMeans$partID)
dfMeans$att <- factor(dfMeans$att, levels = c("att","ign"))
dfMeans$col <- factor(dfMeans$col, levels = c("col","gray"))
dfMeans$freq <- factor(dfMeans$freq, levels = c("857","15"))

### some plot settings
# determine minima and maxima on y axes based on data, separate for driving frequencies
min857 <- floor(min(dfMeans$amplitude[dfMeans$freq == "857"])/50) * 50
max857 <- ceiling(max(dfMeans$amplitude[dfMeans$freq == "857"])/50) * 50
min15 <- floor(min(dfMeans$amplitude[dfMeans$freq == "15"])/50) * 50
max15 <- ceiling(max(dfMeans$amplitude[dfMeans$freq == "15"])/50) * 50

# set distance between ticks and determine ticks
tickDist <- 50
minTick857 <- ceiling(min857/tickDist) * tickDist
maxTick857 <- floor(max857/tickDist) * tickDist
minTick15 <- ceiling(min15/tickDist) * tickDist
maxTick15 <- floor(max15/tickDist) * tickDist



# compute condition means
dfGrandMeans <- aggregate(amplitude ~ att + col + freq, data = dfMeans, FUN = mean)



### plotting starts
# plot attended & ignored within color at 8.57 Hz
plot_col_857 <- ggplot(data = dfMeans[dfMeans$col == "col" & dfMeans$freq == "857",], 
                       aes(x = att, y = amplitude, color = att, fill = att, alpha = att)) +
  theme_classic() + 
  # add line connecting the means
  geom_segment(aes(x = 1, xend = 2,
                   y = dfGrandMeans$amplitude[dfGrandMeans$att == "att" & dfGrandMeans$col == "col" & dfGrandMeans$freq == 857],
                   yend = dfGrandMeans$amplitude[dfGrandMeans$att == "ign" & dfGrandMeans$col == "col" & dfGrandMeans$freq == 857]),
               linewidth = 1,
               data = dfGrandMeans[dfGrandMeans$col == "col" & dfGrandMeans$freq == 857,]) +
  # add diamonds to show means
  geom_point(aes(x = att, y = amplitude), shape = 18, size = 5, alpha = 1,
             data = dfGrandMeans[dfGrandMeans$col == "col" & dfGrandMeans$freq == 857,]) +
  # add raincloud plots
  geom_rain(rain.side = "f1x1", id.long.var = "partID",
            point.args.pos = list(position = position_identity()),
            line.args.pos = list(position = position_identity()),
            violin.args = list(color = NA), # no outline for distribution
            boxplot.args = list(color = NA, fill = NA)) + # no boxplots
  # some color settings
  scale_color_manual(values = brewer.pal(n = 5, name = "Reds")[c(5,5)]) +
  scale_fill_manual(values = brewer.pal(n = 5, name = "Reds")[c(5,5)]) +
  scale_alpha_manual(values = c(0.60,0.30)) +
  # axes and title
  scale_x_discrete(labels = c("cued", "uncued")) +
  scale_y_continuous(name = "ssVEP amplitude (% change)", limits = c(min857, max857), breaks = seq(minTick857,maxTick857,tickDist)) +
  labs(title = "Colour Pictures at 8.57 Hz") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", margin = margin(0,10,0,0)),
        axis.text = element_text(size = plotFS, color = "black"),
        plot.margin = unit(c(20, 20, 20, 20), unit = "pt"))



# plot attended & ignored within color at 15 Hz
plot_col_15 <- ggplot(data = dfMeans[dfMeans$col == "col" & dfMeans$freq == "15",], 
                       aes(x = att, y = amplitude, color = att, fill = att, alpha = att)) +
  theme_classic() + 
  # add line connecting the means
  geom_segment(aes(x = 1, xend = 2,
                   y = dfGrandMeans$amplitude[dfGrandMeans$att == "att" & dfGrandMeans$col == "col" & dfGrandMeans$freq == 15],
                   yend = dfGrandMeans$amplitude[dfGrandMeans$att == "ign" & dfGrandMeans$col == "col" & dfGrandMeans$freq == 15]),
               linewidth = 1,
               data = dfGrandMeans[dfGrandMeans$col == "col" & dfGrandMeans$freq == 15,]) +
  # add diamonds to show means
  geom_point(aes(x = att, y = amplitude), shape = 18, size = 5, alpha = 1,
             data = dfGrandMeans[dfGrandMeans$col == "col" & dfGrandMeans$freq == 15,]) +
  # add raincloud plots
  geom_rain(rain.side = "f1x1", id.long.var = "partID",
            point.args.pos = list(position = position_identity()),
            line.args.pos = list(position = position_identity()),
            violin.args = list(color = NA), # no outline for distribution
            boxplot.args = list(color = NA, fill = NA)) + # no boxplots
  # some color settings
  scale_color_manual(values = brewer.pal(n = 5, name = "Reds")[c(5,5)]) +
  scale_fill_manual(values = brewer.pal(n = 5, name = "Reds")[c(5,5)]) +
  scale_alpha_manual(values = c(0.60,0.20)) +
  # axes and title
  scale_x_discrete(labels = c("cued", "uncued")) +
  scale_y_continuous(name = "ssVEP amplitude (% change)", limits = c(min15, max15), breaks = seq(minTick15,maxTick15,tickDist)) +
  labs(title = "Colour Pictures at 15 Hz") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", margin = margin(0,10,0,0)),
        axis.text = element_text(size = plotFS, color = "black"),
        plot.margin = unit(c(20, 20, 20, 20), unit = "pt"))



# plot attended & ignored within grayscale at 15 Hz
plot_gray_857 <- ggplot(data = dfMeans[dfMeans$col == "gray" & dfMeans$freq == "857",], 
                       aes(x = att, y = amplitude, color = att, fill = att, alpha = att)) +
  theme_classic() + 
  # add line connecting the means
  geom_segment(aes(x = 1, xend = 2,
                   y = dfGrandMeans$amplitude[dfGrandMeans$att == "att" & dfGrandMeans$col == "gray" & dfGrandMeans$freq == 857],
                   yend = dfGrandMeans$amplitude[dfGrandMeans$att == "ign" & dfGrandMeans$col == "gray" & dfGrandMeans$freq == 857]),
               linewidth = 1,
               data = dfGrandMeans[dfGrandMeans$col == "gray" & dfGrandMeans$freq == 857,]) +
  # add diamonds to show means
  geom_point(aes(x = att, y = amplitude), shape = 18, size = 5, alpha = 1,
             data = dfGrandMeans[dfGrandMeans$col == "gray" & dfGrandMeans$freq == 857,]) +
  # add raincloud plots
  geom_rain(rain.side = "f1x1", id.long.var = "partID",
            point.args.pos = list(position = position_identity()),
            line.args.pos = list(position = position_identity()),
            violin.args = list(color = NA), # no outline for distribution
            boxplot.args = list(color = NA, fill = NA)) + # no boxplots
  # some color settings
  scale_color_manual(values = c("black","black")) +
  scale_fill_manual(values = c("black","black")) +
  scale_alpha_manual(values = c(0.60,0.20)) +
  # axes and title
  scale_x_discrete(labels = c("cued", "uncued")) +
  scale_y_continuous(name = "ssVEP amplitude (% change)", limits = c(min857, max857), breaks = seq(minTick857,maxTick857,tickDist)) +
  labs(title = "Greyscale Pictures at 8.57 Hz") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", margin = margin(0,10,0,0)),
        axis.text = element_text(size = plotFS, color = "black"),
        plot.margin = unit(c(20, 20, 20, 20), unit = "pt"))



# plot attended & ignored within grayscale at 15 Hz
plot_gray_15 <- ggplot(data = dfMeans[dfMeans$col == "gray" & dfMeans$freq == "15",], 
                       aes(x = att, y = amplitude, color = att, fill = att, alpha = att)) +
  theme_classic() + 
  # add line connecting the means
  geom_segment(aes(x = 1, xend = 2,
                   y = dfGrandMeans$amplitude[dfGrandMeans$att == "att" & dfGrandMeans$col == "gray" & dfGrandMeans$freq == 15],
                   yend = dfGrandMeans$amplitude[dfGrandMeans$att == "ign" & dfGrandMeans$col == "gray" & dfGrandMeans$freq == 15]),
               linewidth = 1,
               data = dfGrandMeans[dfGrandMeans$col == "gray" & dfGrandMeans$freq == 15,]) +
  # add diamonds to show means
  geom_point(aes(x = att, y = amplitude), shape = 18, size = 5, alpha = 1,
             data = dfGrandMeans[dfGrandMeans$col == "gray" & dfGrandMeans$freq == 15,]) +
  # add raincloud plots
  geom_rain(rain.side = "f1x1", id.long.var = "partID",
            point.args.pos = list(position = position_identity()),
            line.args.pos = list(position = position_identity()),
            violin.args = list(color = NA), # no outline for distribution
            boxplot.args = list(color = NA, fill = NA)) + # no boxplots
  # some color settings
  scale_color_manual(values = c("black","black")) +
  scale_fill_manual(values = c("black","black")) +
  scale_alpha_manual(values = c(0.60,0.20)) +
  # axes and title
  scale_x_discrete(labels = c("cued", "uncued")) +
  scale_y_continuous(name = "ssVEP amplitude (% change)", limits = c(min15, max15), breaks = seq(minTick15,maxTick15,tickDist)) +
  labs(title = "Greyscale Pictures at 15 Hz") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", margin = margin(0,10,0,0)),
        axis.text = element_text(size = plotFS, color = "black"),
        plot.margin = unit(c(20, 20, 20, 20), unit = "pt"))



# combine all plots in one figure
rainPlots <- ggarrange(plot_col_857, plot_gray_857,
                       plot_col_15, plot_gray_15,
                       nrow = 2, ncol = 2); rainPlots

# save figure
savename <- paste0(parentFolder, "/figures/03_groupStats.pdf")
ggsave(filename = savename, plot = rainPlots, device = "pdf",
       width = 20, height = 20, unit = "cm")