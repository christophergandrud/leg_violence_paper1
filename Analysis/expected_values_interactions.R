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


#### Prop*Self Expression ####
# High self expression
dp1_fitted_high <- setx(DP1, high_prop = high_prop.r, cw_surv_self_expr = 1.35)
dp1_sum_high <- sim_strip(DP1, dp1_fitted_high, high_prop.r)
dp1_sum_high$x2 <- 'high'

# Low self expression
dp1_fitted_low <- setx(DP1, high_prop = high_prop.r, cw_surv_self_expr = 1.1)
dp1_sum_low <- sim_strip(DP1, dp1_fitted_low, high_prop.r)
dp1_sum_low$x2 <- 'low'

dp1_comb <- rbind(dp1_sum_high, dp1_sum_low)

dp1_p <- ggplot(dp1_comb, aes(fitted, Median, ymin = Min_CI, ymax = Max_CI,
                colour = x2)) +
    geom_pointrange(size = 3, alpha = 0.5) +
    geom_line() +
    scale_color_grey(name = 'Self Expression') +
    scale_x_reverse(breaks = c(0, 1), labels = c("Above", "Below Median")) +
    scale_y_continuous(breaks = c(0, 0.05, 0.1),
                       limits = c(0, 0.17)) +
    xlab("\nDisproportionality") + ylab("") +
    theme_bw(base_size = 12)

#### Prop*Ethnic ####
# High Frac
dp2_fitted_high <- setx(DP2, high_prop = high_prop.r, ethnic_alesina = 0.8)
dp2_sum_high <- sim_strip(DP2, dp2_fitted_high, high_prop.r)
dp2_sum_high$x2 <- 'high'

# Low Frac
dp2_fitted_low <- setx(DP2, high_prop = high_prop.r, cw_surv_self_expr = 0.1)
dp2_sum_low <- sim_strip(DP2, dp2_fitted_low, high_prop.r)
dp2_sum_low$x2 <- 'low'

dp2_comb <- rbind(dp2_sum_high, dp2_sum_low)

dp2_p <- ggplot(dp2_comb, aes(fitted, Median, ymin = Min_CI, ymax = Max_CI,
                              colour = x2)) +
    geom_pointrange(size = 1, alpha = 0.5) +
    geom_line() +
    scale_color_grey(name = 'Ethnic Fractionalization') +
    scale_x_reverse(breaks = c(0, 1), labels = c("Above", "Below Median")) +
   # scale_y_continuous(breaks = c(0, 0.05, 0.1),
   #                    limits = c(0, 1)) +
    xlab("\nDisproportionality") + ylab("") +
    theme_bw(base_size = 12)

#### Prop*GINI ####
# High Inequality
dp_3fitted_high <- setx(DP3, high_prop = high_prop.r, gini = 0.6)
dp_3sum_high <- sim_strip(DP3, dp_3fitted_high, high_prop.r)
dp_3sum_high$x2 <- 'high'

# Low Inequality
dp_3fitted_low <- setx(DP3, high_prop = high_prop.r, gini = 0.1)
dp_3sum_low <- sim_strip(DP3, dp_3fitted_low, high_prop.r)
dp_3sum_low$x2 <- 'low'

dp_3comb <- rbind(dp_3sum_high, dp_3sum_low)

dp_3p <- ggplot(dp_3comb, aes(fitted, Median, ymin = Min_CI, ymax = Max_CI,
                              colour = x2)) +
    geom_pointrange(size = 1, alpha = 0.5) +
    geom_line() +
    scale_color_grey(name = 'Inequality (gini)') +
    scale_x_reverse(breaks = c(0, 1), labels = c("Above", "Below Median")) +
    # scale_y_continuous(breaks = c(0, 0.05, 0.1),
    #                    limits = c(0, 1)) +
    xlab("\nDisproportionality") + ylab("") +
    theme_bw(base_size = 12)

#### Dem Age*Etnic ####
# High frac
dp_6fitted_high <- setx(DP6, dem_age = dem.r, polconiii = 0.7)
dp_6sum_high <- sim_strip(DP6, dp_6fitted_high, dem.r)
dp_6sum_high$x2 <- 'high'

# Low frac
dp_6fitted_low <- setx(DP6, dem_age = dem.r, polconiii = 0.5)
dp_6sum_low <- sim_strip(DP6, dp_6fitted_low, dem.r)
dp_6sum_low$x2 <- 'low'

dp_6comb <- rbind(dp_6sum_high, dp_6sum_low)

dp_6p <- ggplot(dp_6comb, aes(fitted, Median, ymin = Min_CI, ymax = Max_CI,
                              fill = x2)) +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    #scale_y_continuous(breaks = c(0, 0.05, 0.1),
    #                   limits = c(0, 0.17)) +
    scale_x_continuous(breaks = c(1, 25, 50, 75)) +
    scale_fill_grey(start = 0.1, end = 0.7, name = 'Ethnic Fractionalization') +
    xlab("\nAge of Democracy") + ylab("") +
    theme_bw(base_size = 12)

#### Dem Age*Pol Constraints ####
# High contraints
dpc_2fitted_high <- setx(DPC2, dem_age = dem.r, polconiii = 0.7)
dpc_2sum_high <- sim_strip(DPC2, dpc_2fitted_high, dem.r)
dpc_2sum_high$x2 <- 'high'

# Low constraints
dpc_2fitted_low <- setx(DPC2, dem_age = dem.r, polconiii = 0.1)
dpc_2sum_low <- sim_strip(DPC2, dpc_2fitted_low, dem.r)
dpc_2sum_low$x2 <- 'low'

dpc_2comb <- rbind(dpc_2sum_high, dpc_2sum_low)

dpc_2p <- ggplot(dpc_2comb, aes(fitted, Median, ymin = Min_CI, ymax = Max_CI,
                              fill = x2)) +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    #scale_y_continuous(breaks = c(0, 0.05, 0.1),
    #                   limits = c(0, 0.17)) +
    scale_x_continuous(breaks = c(1, 25, 50, 75)) +
    scale_fill_grey(start = 0.1, end = 0.7, name = 'Political Constraints') +
    xlab("\nAge of Democracy") + ylab("") +
    theme_bw(base_size = 12)
