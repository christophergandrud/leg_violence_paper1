#######################
# Legislative Violence Expected Value Graphs, Interactions
# Christopher Gandrud
# Updated 24 March 2015
#######################

# Set working directory. Change as needed.
setwd('/git_repositories/leg_violence_paper1/')

# Load required packages
library(Zelig)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Load MinMaxLines function
source('Analysis/MinMaxLines.R')
source('Analysis/sim_strip.R')


## Ranges of fitted values
high_prop.r <- c(0, 1)
dem.r <- seq(from = 1, to = 86, by = 5)
