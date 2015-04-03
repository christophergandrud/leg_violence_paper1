############
# Compare incidents to 
# Christopher Gandrud
# 31 March 2015
############

# Set working directory. Change as needed.
setwd('~/git_repositories/leg_violence_paper1/')

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
summed_elect <- dem %>% group_by(year) %>% 
                    summarise(total_elect = sum(fake))
summed_violence <- dem %>% group_by(year) %>% 
                    summarise(total_violence = sum(violence))

# Plot of elected legislatures by year
elect_p <- ggplot(summed_elect, aes(as.numeric(year), total_elect)) +
                geom_point() +
                geom_line() +
                scale_x_continuous(breaks = c(1981, 1990, 1995, 2000, 2010)) +
                xlab('') + ylab('Elected Multi-Party National Legislatures\n') +
                theme_bw()

# Plot of violence by year
violence_p <- ggplot(summed_violence, aes(as.numeric(year), total_violence)) +
                geom_point() +
                stat_smooth(se = F) +
                scale_x_continuous(breaks = c(1981, 1990, 1995, 2000, 2010)) +
                xlab('') + ylab('Violence in Elected Legislatures\n') +
                theme_bw()

grid.arrange(elect_p, violence_p)
