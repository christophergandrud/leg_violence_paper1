############
# Interactive Robustness Tests for Legislative Violence
# Christopher Gandrud
# 24 March 2015
############

# Set working directory. Change as needed.
setwd('/git_repositories/leg_violence_paper1/')

# Load package
if (!('rio' %in% installed.packages()[, 1]))
    devtools::install_github('leeper/rio')
library(rio)
library(Zelig)

# Load data
all <- import('Data/LegislativeViolenceMain.csv')

#### Keep only democracies ####
dem <- subset(all, polity2 > 5)

dNew <- subset(dem, year > 1989)

# tau's
tau_dNew <- sum(dNew$violence) / nrow(dNew)

# Complete cases
vars_1 <- c("violence", "dem_age", "high_prop", 'maj', 'internal_conflict',
            'cw_surv_self_expr')
dNew_1.c <- dNew[complete.cases(dNew[vars_1]),]

vars_2 <- c("violence", "dem_age", "high_prop", 'maj', 'internal_conflict',
            'ethnic_alesina')
dNew_2.c <- dNew[complete.cases(dNew[vars_2]),]


vars_3 <- c("violence", "dem_age", "high_prop", 'maj', 'internal_conflict',
            'gini')
dNew_3.c <- dNew[complete.cases(dNew[vars_3]),]

vars_4 <- c("violence", "dem_age", "high_prop", 'maj', 'internal_conflict',
            'polconiii')
dNew_4.c <- dNew[complete.cases(dNew[vars_4]),]

###########################
### Sample of countries with elected legislatures from 1990
## Societal Interactions with high_prop
DP1 <- zelig(violence ~ log(dem_age) + maj+ internal_conflict +
                high_prop*cw_surv_self_expr, model = "relogit",
             data = dNew_1.c, tau = tau_dNew, robust = list(method = "weave"),
             cite = FALSE)

DP2 <- zelig(violence ~ log(dem_age) + maj+ internal_conflict +
                high_prop*ethnic_alesina, model = "relogit",
             data = dNew_2.c, tau = tau_dNew, robust = list(method = "weave"),
             cite = FALSE)

DP3 <- zelig(violence ~ log(dem_age) + maj + internal_conflict +
                high_prop*gini,
             model = "relogit", data = dNew_3.c, tau = tau_dNew,
             robust = list(method = "weave"), cite = FALSE)

DP4 <- zelig(violence ~ log(dem_age) + maj + internal_conflict +
                high_prop*log(gdp_per_capita),
             model = "relogit", data = dNew, tau = tau_dNew,
             robust = list(method = "weave"), cite = FALSE)

DP5 <- zelig(violence ~ high_prop + maj+ internal_conflict +
             log(dem_age)*cw_surv_self_expr, model = "relogit",
          data = dNew, tau = tau_dNew, robust = list(method = "weave"),
          cite = FALSE)

DP6 <- zelig(violence ~ high_prop + maj+ internal_conflict +
            log(dem_age)*ethnic_alesina, model = "relogit",
          data = dNew_2.c, tau = tau_dNew, robust = list(method = "weave"),
          cite = FALSE)

DP7 <- zelig(violence ~ high_prop + maj + internal_conflict +
            log(dem_age)*gini,
          model = "relogit", data = dNew, tau = tau_dNew,
          robust = list(method = "weave"), cite = FALSE)

DP8 <- zelig(violence ~ high_prop + maj + internal_conflict +
            log(dem_age)*log(gdp_per_capita),
          model = "relogit", data = dNew, tau = tau_dNew,
          robust = list(method = "weave"), cite = FALSE)


#### Interactions with political constraints
DPC1 <- zelig(violence ~ polconiii*high_prop + log(dem_age) + maj, model = "relogit",
             data = dNew, tau = tau_dNew, robust = list(method = "weave"),
             cite = FALSE)

DPC2 <- zelig(violence ~ polconiii*log(dem_age) + high_prop + maj, model = "relogit",
             data = dNew_4.c, tau = tau_dNew, robust = list(method = "weave"),
             cite = FALSE)
