# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.12.0
# --- script version: May 2023
# --- content: installing required packages but only if not already installed

if(!is.element("here",installed.packages()[,1])) {install.packages("here")}
if(!is.element("psych",installed.packages()[,1])) {install.packages("psych")}
if(!is.element("ez",installed.packages()[,1])) {install.packages("ez")}
if(!is.element("rstatix",installed.packages()[,1])) {install.packages("rstatix")}
if(!is.element("BayesFactor",installed.packages()[,1])) {install.packages("BayesFactor")}
if(!is.element("doMC",installed.packages()[,1])) {install.packages("doMC")}
if(!is.element("bayestestR",installed.packages()[,1])) {install.packages("bayestestR")}
if(!is.element("ggplot2",installed.packages()[,1])) {install.packages("ggplot2")}
if(!is.element("RColorBrewer",installed.packages()[,1])) {install.packages("RColorBrewer")}
if(!is.element("eegUtils",installed.packages()[,1])) {install.packages("eegUtils")}
if(!is.element("OneR",installed.packages()[,1])) {install.packages("OneR")}
if(!is.element("ggpubr",installed.packages()[,1])) {install.packages("ggpubr")}
if(!is.element("tidyr",installed.packages()[,1])) {install.packages("tidyr")}
if(!is.element("ggrain",installed.packages()[,1])) {install.packages("ggrain")}
