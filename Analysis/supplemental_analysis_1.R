############
# Compare incidents to 
# Christopher Gandrud
# 9 April 2015
############

# Set working directory. Change as needed.
setwd('/git_repositories/leg_violence_paper1/')

# Load package
if (!('rio' %in% installed.packages()[, 1]))
    devtools::install_github('leeper/rio')
library(rio)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Load data
all <- import('Data/LegislativeViolenceMain.csv')
all <- subset(all, year > 1980)

#### Keep only democracies ####
dem <- subset(all, polity2 > 5)

# Find year totals
dem$fake <- 1
summed_dem <- dem %>% group_by(year) %>% 
                    summarise(total_dem = sum(fake))
summed_violence <- all %>% group_by(year) %>% 
                    summarise(total_violence = sum(violence))

# Plot of elected legislatures by year
elect_p <- ggplot(summed_dem, aes(as.numeric(year), total_dem)) +
                geom_point() +
                geom_line() +
                scale_x_continuous(breaks = c(1981, 1990, 1995, 2000, 2010)) +
                xlab('') + ylab('Democratic Countries\n') +
                theme_bw()

# Plot of violence by year
violence_p <- ggplot(summed_violence, aes(as.numeric(year), total_violence)) +
                geom_point() +
                stat_smooth(se = F) +
                scale_x_continuous(breaks = c(1981, 1990, 1995, 2000, 2010)) +
                xlab('') + ylab('Violence Incidents in All Legislatures\n') +
                theme_bw()

grid.arrange(elect_p, violence_p)
