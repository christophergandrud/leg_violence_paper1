###################
# Marginal effects for key interactions
# Christopher Gandrud
# 26 March 2015
###################

# Set working directory. Change as needed.
setwd('/git_repositories/leg_violence_paper1/')

# Load required packages
library(ggplot2)
library(gridExtra)

# Load MinMaxLines function
source('Analysis/MinMaxLines.R')
source('Analysis/sim_strip.R')

#### Lower Disproportionality ####
dp1_me <- marginal_effect(obj = DP1, b1 = 'high_prop', b2 = 'cw_surv_self_expr', 
                          X2 = seq(1.1, 1.35, by = 0.05)) 


dp1_me_p <-ggplot(dp1_me, aes(fitted, Median, ymin = Min_CI, ymax = Max_CI)) +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    xlab("\nSelf Expressive Values") + 
    ylab("Marginal Effect of\nLower Disproportionality\n") +
    theme_bw(base_size = 12)


dp2_me <- marginal_effect(obj = DP2, b1 = 'high_prop', b2 = 'ethnic_alesina', 
                        X2 = seq(0.20, 0.8, by = 0.1)) 


dp2_me_p <- ggplot(dp2_me, aes(fitted, Median, ymin = Min_CI, ymax = Max_CI)) +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    xlab("\nEthnic Frac.") + 
    ylab("\n") +
    theme_bw(base_size = 12)


#### Democratic Age ####
dp6_me <- marginal_effect(obj = DP6, b1 = 'log.dem_age.', b2 = 'ethnic_alesina', 
                        X2 = seq(0.1, 0.8, by = 0.1)) 

dp6_me_p <-ggplot(dp6_me, aes(fitted, Median, ymin = Min_CI, ymax = Max_CI)) +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    xlab("\nEthnic Frac.") + 
    ylab("Marginal Effect of\nDemocratic Age\n") +
    theme_bw(base_size = 12)

dpc2_me <- marginal_effect(obj = DPC2, b1 = 'log.dem_age.', b2 = 'polconiii', 
                           X2 = seq(0.1, 0.7, by = 0.1)) 

dpc2_me_p <-ggplot(dpc2_me, aes(fitted, Median, ymin = Min_CI, ymax = Max_CI)) +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    xlab("\nPolitical Constraints") + 
    ylab("") +
    theme_bw(base_size = 12)

grid.arrange(dp1_me_p, dp2_me_p, dp6_me_p, dpc2_me_p)
