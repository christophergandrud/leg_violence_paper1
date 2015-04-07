#####################
# Correlation plot for key variables
# Christopher Gandrud
# 31 March 2015
#####################

setwd('/git_repositories/leg_violence_paper1/')

# Load package
if (!('rio' %in% installed.packages()[, 1]))
    devtools::install_github('leeper/rio')
library(rio)
library(corrgram)

# Load data
dem <- import('Data/LegislativeViolenceMain.csv')

# Keep only data in models
vars <- c('violence', 'high_prop', 'dem_age', 'maj', 'internal_conflict',
          'immunity', 'pr', 'single_party', 'polconiii', 'cw_surv_self_expr', 
          'ethnic_alesina', 'dom_personal_vote', 'women_in_parl', 'murder_rate', 
          'federal', 'govfrac', 'enps', 'gini', 'gdp_per_capita')

dem_sub <- dem[, vars]

# Plot
corrgram(dem_sub, order = TRUE, upper.panel = NULL, diag.panel = panel.minmax)
