############
# Main Analysis 4 for Legislative Violence
# Christopher Gandrud
# 7 April 2015
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

dem <- subset(dem, year > 1980)
dNew <- subset(dem, year > 1989)

# tau's
tau_dem <- sum(dem$violence) / nrow(dem)
tau_dNew <- sum(dNew$violence) / nrow(dNew)

# Subset complete cases for model that will be used in simulations
vars.3 <- c("violence", "dem_age", "high_prop", 'maj', 'internal_conflict')

dNew_3_complete <- dNew[complete.cases(dNew[vars.3]), ]

###########################
### Sample of countries with elected legislatures
D1 <- zelig(violence ~ high_prop + log(dem_age) + maj, model = "relogit",
            data = dem, tau = tau_dem, robust = list(method = "weave"),
            cite = FALSE)

D2 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict,
            model = "relogit", data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D3 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict +
                immunity + single_party,
            model = "relogit", data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D4 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict +
                polconiii,
            model = "relogit", data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D5 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict +
                cw_surv_self_expr + ethnic_alesina, model = "relogit",
            data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D6 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict +
                dom_personal_vote,
            model = "relogit", data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D7 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict +
                women_in_parl,
            model = "relogit", data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D8 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict
            + murder_rate,
            model = "relogit", data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D9 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict +
                federal + govfrac,
            model = "relogit", data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D10 <- zelig(violence ~ high_prop + log(dem_age) + maj + + internal_conflict +
                enps,
            model = "relogit", data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D11 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict + gini,
            model = "relogit", data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D12 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict +
                 log(gdp_per_capita),
            model = "relogit", data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

###########################
### Sample of countries with elected legislatures from 1990
DN1 <- zelig(violence ~ high_prop + log(dem_age), model = "relogit",
             data = dNew, tau = tau_dNew, robust = list(method = "weave"),
             cite = FALSE)

DN2 <- zelig(violence ~ high_prop + log(dem_age) + maj, model = "relogit",
             data = dNew, tau = tau_dNew, robust = list(method = "weave"),
             cite = FALSE)

DN3 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict,
             model = "relogit", data = dNew_3_complete, tau = tau_dNew,
             robust = list(method = "weave"),
             cite = FALSE)

DN4 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict +
                 immunity + single_party,
             model = "relogit", data = dNew, tau = tau_dNew,
             robust = list(method = "weave"), cite = FALSE)

DN5 <- zelig(violence ~ high_prop + log(dem_age) + maj+ internal_conflict +
                 polconiii, model = "relogit",
             data = dNew, tau = tau_dNew, robust = list(method = "weave"),
             cite = FALSE)

DN6 <- zelig(violence ~ high_prop + log(dem_age) + maj+ internal_conflict +
                 cw_surv_self_expr, model = "relogit",
             data = dNew, tau = tau_dNew, robust = list(method = "weave"),
             cite = FALSE)

DN7 <- zelig(violence ~ high_prop + log(dem_age) + maj+ internal_conflict +
                 ethnic_alesina, model = "relogit",
             data = dNew, tau = tau_dNew, robust = list(method = "weave"),
             cite = FALSE)

DN8 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict +
              dom_personal_vote,
            model = "relogit", data = dNew, tau = tau_dNew,
            robust = list(method = "weave"), cite = FALSE)

DN9 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict +
                 women_in_parl,
             model = "relogit", data = dNew, tau = tau_dNew,
             robust = list(method = "weave"), cite = FALSE)

DN10 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict +
                 murder_rate,
             model = "relogit", data = dNew, tau = tau_dNew,
             robust = list(method = "weave"), cite = FALSE)

DN11 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict +
                 federal + govfrac,
             model = "relogit", data = dNew, tau = tau_dNew,
             robust = list(method = "weave"), cite = FALSE)

DN12 <- zelig(violence ~ high_prop + log(dem_age) + maj +internal_conflict +
                  enps,
             model = "relogit", data = dNew, tau = tau_dNew,
             robust = list(method = "weave"), cite = FALSE)

DN13 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict +
                  gini,
             model = "relogit", data = dNew, tau = tau_dNew,
             robust = list(method = "weave"), cite = FALSE)

DN14 <- zelig(violence ~ high_prop + log(dem_age) + maj + internal_conflict +
                 log(gdp_per_capita),
             model = "relogit", data = dNew, tau = tau_dNew,
             robust = list(method = "weave"), cite = FALSE)
