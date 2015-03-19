############
# Robustness test with zero inflated negative binomial model
# Christopher Gandrud
# 18 March 2015
############

# Set working directory. Change as needed.
setwd('/git_repositories/leg_violence_paper1/')

# Load package
if (!('rio' %in% installed.packages()[, 1]))
    devtools::install_github('leeper/rio', ref = 'fread')
library(rio)
library(pscl)

# Load data
dem <- import('Data/LegislativeViolenceMain.csv')
dNew <- subset(dem, year > 1989)

#### Estimate Models ####
D1_nb <- zeroinfl(violence ~ high_prop + dem_age + maj, 
               data = dNew.1.c, dist = "negbin", EM = T)

DN1_nb <- zeroinfl(violence ~ high_prop + dem_age, model = "negbinom", 
             data = dNew.1.c, robust = list(method = "weave"), cite = FALSE)
