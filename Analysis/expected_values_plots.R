#######################
# Legislative Violence Expected Value Graphs
# Christopher Gandrud
# Updated 25 March 2015
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
maj.r <- seq(from = 20, to = 100, by = 5)

#### Disporportionality < 6 Dummy ####
## Set fitted values
DN3.high_prop <- setx(DN3, high_prop = high_prop.r)

high_prop_sum <- sim_strip(DN3, DN3.high_prop, high_prop.r)

high_prop_p <- ggplot(high_prop_sum, aes(fitted, Median, ymin = Min_CI,
                    ymax = Max_CI)) +
        geom_pointrange(size = 1) +
        geom_line() +
        scale_x_reverse(breaks = c(0, 1), labels = c("Above", "Below Median")) +
        scale_y_continuous(breaks = c(0, 0.05, 0.1),
                           limits = c(0, 0.17)) +
        xlab("\nDisproportionality") + ylab("") +
        theme_bw(base_size = 12)

#### Age of Democracy ####
# Set fitted values
DN3.dem_age <-setx(DN3, dem_age = dem.r)

dem_age_sum <- sim_strip(DN3, DN3.dem_age, dem.r)

dem_age_p <- DN3.HighProp.p <- ggplot(dem_age_sum, aes(fitted, Median,
                ymin = Min_CI, ymax = Max_CI)) +
        geom_line() +
        geom_ribbon(alpha = 0.3) +
        scale_y_continuous(breaks = c(0, 0.05, 0.1),
                           limits = c(0, 0.17)) +
        scale_x_continuous(breaks = c(1, 25, 50, 75)) +
        xlab("\nAge of Democracy") + ylab("") +
        theme_bw(base_size = 12)

#### Majority ####
# Set fitted values
DN3.maj1 <-setx(DN3, maj = maj.r)

maj_sum <- sim_strip(DN3, DN3.maj1, maj.r)

maj_p <- ggplot(maj_sum, aes(fitted, Median, ymin = Min_CI, ymax = Max_CI)) +
        geom_line() +
        geom_ribbon(alpha = 0.3) +
        scale_y_continuous(breaks = c(0, 0.05, 0.1),
                           limits = c(0, 0.17)) +
        xlab("\nGovernment Majority") + ylab("") +
        theme_bw(base_size = 12)

#### Combibine plots
predicted.combine <- grid.arrange(high_prop_p, dem_age_p, maj_p,
                        ncol = 3,
                        left = "Expected Probability of Violence in a Year")
print(predicted.combine)
