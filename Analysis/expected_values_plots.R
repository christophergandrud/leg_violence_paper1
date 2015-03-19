#######################
# Legislative Violence Expected Value Graphs
# Christopher Gandrud
# Updated 19 March 2015
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

## Ranges of fitted values
high_prop.r <- c(0, 1)
dem.r <- seq(from = 0, to = 85, by = 2)
maj.r <- seq(from = 20, to = 100, by = 2)

#### Disporportionality < 6 Dummy ####
## Set fitted values
DN2.high_prop <- setx(DN2, high_prop = high_prop.r)

# Simulate quantities of interest
DN2.high_prop_sim <- sim(DN2, x = DN2.high_prop)

# Extract/clean simulations
DN2.high_prop_e <- data.frame(simulation.matrix(DN2.high_prop_sim,
                                                "Expected Values: E(Y|X)"))
high_prop_gather <- gather(DN2.high_prop_e, fitted, value)
high_prop_gather$fitted <- gsub('X', '', high_prop_gather$fitted) %>% as.numeric

# Find 0.95, median
high_prop_sum <- MinMaxLines(high_prop_gather)

high_prop_p <- ggplot(high_prop_sum, aes(fitted, Median, ymin = Min_CI,
                    ymax = Max_CI)) +
        geom_pointrange(size = 1) +
        geom_line() +
        scale_x_reverse(breaks = c(1, 2), labels = c("Above", "Below Median")) +
        scale_y_continuous(breaks = c(0, 0.02, 0.05),
                           labels = c("0", "0.02", "0.05"),
                           limits = c(0, 0.071)) +
        xlab("\nDisproportionality") + ylab("") +
        theme_bw(base_size = 12)

#### Age of Democracy ####
# Set fitted values
DN2.dem_age <-setx(DN2, dem_age = dem.r)

# Simulate quantities of interest
DN2.dem_age_sim <- sim(DN2, x = DN2.dem_age)

## Age of Democracy
# Set fitted values
DN2.dem_age <-setx(DN2, dem_age = dem.r)

# Simulate quantities of interest
DN2.dem_age_sim <- sim(DN2, x = DN2.dem_age)

# Extract/clean simulations
DN2.dem_age_e <- data.frame(simulation.matrix(DN2.dem_age_sim,
                                                "Expected Values: E(Y|X)"))
names(DN2.dem_age_e) <- sprintf('X%s', seq(from = 0, to = 85, by = 2))
dem_age_gather <- gather(DN2.dem_age_e, fitted, value)
dem_age_gather$fitted <- gsub('X', '', dem_age_gather$fitted) %>% as.numeric

# Find 0.95, median
dem_age_sum <- MinMaxLines(dem_age_gather)

dem_age_p <- DN2.HighProp.p <- ggplot(dem_age_sum, aes(fitted, Median,
                ymin = Min_CI, ymax = Max_CI)) +
        geom_line() +
        geom_ribbon(alpha = 0.3) +
        scale_y_continuous(breaks = c(0, 0.02, 0.05),
                           labels = c("0", "0.02", "0.05"),
                           limits = c(0, 0.071)) +
        xlab("\nAge of Democracy") + ylab("") +
        theme_bw(base_size = 12)

#### Majority ####
# Set fitted values
DN2.maj1 <-setx(DN2, maj = maj.r)

# Simulate quantities of interest
DN2.maj_sim <- sim(DN2, x = DN2.maj1)

# Extract/clean simulations
DN2.maj_e <- data.frame(simulation.matrix(DN2.maj_sim,
                                                "Expected Values: E(Y|X)"))
names(DN2.maj_e) <- sprintf('X%s', seq(from = 20, to = 100, by = 2))
maj_gather <- gather(DN2.maj_e, fitted, value)
maj_gather$fitted <- gsub('X', '', maj_gather$fitted) %>% as.numeric

# Find 0.95, median
maj_sum <- MinMaxLines(maj_gather)

maj_p <- ggplot(maj_sum, aes(fitted, Median, ymin = Min_CI, ymax = Max_CI)) +
        geom_line() +
        geom_ribbon(alpha = 0.3) +
        scale_y_continuous(breaks = c(0, 0.02, 0.05),
                           labels = c("0", "0.02", "0.05"),
                           limits = c(0, 0.071)) +
        xlab("\nGovernment Majority") + ylab("") +
        theme_bw(base_size = 12)

#### Combibine plots
predicted.combine <- grid.arrange(high_prop_p, dem_age_p, maj_p,
                        ncol = 3,
                        left = "Expected Probability of Violence in a Year")
print(predicted.combine)
