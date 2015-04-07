############
# Interactive Robustness Tests for Legislative Violence
# Christopher Gandrud
# 26 March 2015
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
vars_1_1 <- c("violence", "dem_age", "high_prop", 'maj')
dNew_1_1.c <- dNew[complete.cases(dNew[vars_1_1]), ]

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
## Lower disproportionality
DP1_1 <- zelig(violence ~ maj + log(dem_age)*high_prop, model = "relogit",
              data = dNew_1_1.c, tau = tau_dNew, robust = list(method = "weave"),
              cite = FALSE)

DP1 <- zelig(violence ~ maj + log(dem_age) + 
                high_prop*cw_surv_self_expr, model = "relogit",
             data = dNew_1.c, tau = tau_dNew, robust = list(method = "weave"),
             cite = FALSE)

DP2 <- zelig(violence ~ maj + log(dem_age) +
                high_prop*ethnic_alesina, model = "relogit",
             data = dNew_2.c, tau = tau_dNew, robust = list(method = "weave"),
             cite = FALSE)

DP3 <- zelig(violence ~ maj + log(dem_age) +
                high_prop*gini,
             model = "relogit", data = dNew_3.c, tau = tau_dNew,
             robust = list(method = "weave"), cite = FALSE)

DP4 <- zelig(violence ~ maj + log(dem_age) +
                high_prop*log(gdp_per_capita),
             model = "relogit", data = dNew, tau = tau_dNew,
             robust = list(method = "weave"), cite = FALSE)

DPC1 <- zelig(violence ~ maj + log(dem_age) +
              high_prop*polconiii,
              model = "relogit", data = dNew, tau = tau_dNew, 
              robust = list(method = "weave"),
              cite = FALSE)

#### Democratic age interactions
DP5 <- zelig(violence ~ maj + high_prop +
             log(dem_age)*cw_surv_self_expr, model = "relogit",
          data = dNew, tau = tau_dNew, robust = list(method = "weave"),
          cite = FALSE)

DP6 <- zelig(violence ~ maj + high_prop +
            log(dem_age)*ethnic_alesina, model = "relogit",
          data = dNew_2.c, tau = tau_dNew, robust = list(method = "weave"),
          cite = FALSE)

DP7 <- zelig(violence ~ maj + high_prop +
            log(dem_age)*gini,
          model = "relogit", data = dNew, tau = tau_dNew,
          robust = list(method = "weave"), cite = FALSE)

DP8 <- zelig(violence ~ maj + high_prop +
            log(dem_age)*log(gdp_per_capita),
          model = "relogit", data = dNew, tau = tau_dNew,
          robust = list(method = "weave"), cite = FALSE)

DPC2 <- zelig(violence ~ maj + high_prop + log(dem_age)*polconiii, 
              model = "relogit",
             data = dNew_4.c, tau = tau_dNew, robust = list(method = "weave"),
             cite = FALSE)
