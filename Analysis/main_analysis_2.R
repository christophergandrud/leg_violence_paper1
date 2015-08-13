############
# Legislative Violence
# Combined scatter plots to illustrate how framework relates to findings
# Christopher Gandrud
# Updated 13 August 2015
############

# Set working directory. Change as needed.
setwd('/git_repositories/leg_violence_paper1/')

# Load packages
library(ggplot2)
library(gridExtra)
library(rio)
library(dplyr)

# Load data
leg_cumulative <- import('Data/LegislativeViolenceMain.csv') %>%
                    filter(year > 1980)

leg_cumulative <- leg_cumulative[!duplicated(leg_cumulative[,
                                        c('iso2c', 'year', 'violence_y_cum')]), ]

leg_cumulative$violence <- factor(leg_cumulative$violence, 
                                  labels = c("No violence", "Violence"))
leg_cumulative <- subset(leg_cumulative, violence != "NA")

cols <- c("1" = "#B7B7B7", "2" = "#7D7D7D", "3" = "#111111", "0" = "#EBEBEB")

## Create scatterplot of DemAge, Disproportionality, and Violence
age.disp.scatter <- qplot(dem_age, disproportionality,
                          position = position_jitter(w = 10),
                          color = factor(violence_y_cum),
                          data = leg_cumulative) +
                    facet_grid(.~violence) +
                    scale_y_log10(breaks = c(1, 2.5, 5, 10, 20, 30),
                                  labels = c(1, 2.5,  5, 10, 20, 30)) +
                    scale_x_continuous(breaks = c(0, 50, 100, 150, 200)) +
                    xlab("\n Age of democracy (years)") +
                    ylab("Disproportionality (log)\n") +
                    scale_colour_manual(values = cols, name = "Brawl/Year") +
                    theme_bw(base_size = 12)


print(age.disp.scatter)
