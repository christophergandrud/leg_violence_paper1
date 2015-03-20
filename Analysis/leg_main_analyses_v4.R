############
# Main Analysis 4 for Legislative Violence
# Christopher Gandrud
# 20 March 2015
############

# Set working directory. Change as needed.
setwd('/git_repositories/leg_violence_paper1/')

# Load package
if (!('rio' %in% installed.packages()[, 1]))
    devtools::install_github('leeper/rio')
library(rio)
library(Zelig)

# Load data
dem <- import('Data/LegislativeViolenceMain.csv')
dem <- subset(dem, year > 1980)
dNew <- subset(dem, year > 1989)

# tau's
tau_dem <- sum(dem$violence) / nrow(dem)
tau_dNew <- sum(dNew$violence) / nrow(dNew)

# Subset complete cases for model that will be used in simulations
vars.3 <- c("violence", "dem_age", "high_prop", 'maj', 'immunity',
            'pr', 'single_party', 'internal_conflict')

dem.3.c <- dem[complete.cases(dem[vars.3]),]
dNew.3.c <- dNew[complete.cases(dNew[vars.3]),]

###########################
### Sample of countries with elected legislatures
D1 <- zelig(violence ~ high_prop + dem_age + maj, model = "relogit",
            data = dem, tau = tau_dem, robust = list(method = "weave"),
            cite = FALSE)

D2 <- zelig(violence ~ high_prop + dem_age + maj + internal_conflict,
            model = "relogit", data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D3 <- zelig(violence ~ high_prop + dem_age + maj + internal_conflict + immunity
            + pr + single_party,
            model = "relogit", data = dem.3.c, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D4 <- zelig(violence ~ high_prop + dem_age + maj + internal_conflict +
                cw_surv_self_expr + ethnic_alesina, model = "relogit",
            data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D5 <- zelig(violence ~ high_prop + dem_age + maj + internal_conflict +
                women_in_parl,
            model = "relogit", data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D6 <- zelig(violence ~ high_prop + dem_age + maj + internal_conflict
            + murder_rate,
            model = "relogit", data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D7 <- zelig(violence ~ high_prop + dem_age + maj + internal_conflict +
                federal + govfrac,
            model = "relogit", data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D8 <- zelig(violence ~ high_prop + dem_age + maj + + internal_conflict +
                enps,
            model = "relogit", data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D9 <- zelig(violence ~ high_prop + dem_age + maj + internal_conflict + gini,
            model = "relogit", data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D10 <- zelig(violence ~ high_prop + dem_age + maj + internal_conflict +
                 log(gdp_per_capita),
            model = "relogit", data = dem, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

###########################
### Sample of countries with elected legislatures from 1990
DN1 <- zelig(violence ~ high_prop + dem_age + maj, model = "relogit",
             data = dNew, tau = tau_dNew, robust = list(method = "weave"),
             cite = FALSE)

DN2 <- zelig(violence ~ high_prop + dem_age + maj + internal_conflict,
             model = "relogit", data = dNew, tau = tau_dNew,
             robust = list(method = "weave"),
             cite = FALSE)

DN3 <- zelig(violence ~ high_prop + dem_age + maj + internal_conflict +
                 immunity + pr + single_party,
             model = "relogit", data = dNew.3.c, tau = tau_dem,
             robust = list(method = "weave"), cite = FALSE)

DN4 <- zelig(violence ~ high_prop + dem_age + maj+ internal_conflict +
                 cw_surv_self_expr, model = "relogit",
             data = dNew, tau = tau_dem, robust = list(method = "weave"),
             cite = FALSE)

DN5 <- zelig(violence ~ high_prop + dem_age + maj+ internal_conflict +
                 ethnic_alesina, model = "relogit",
             data = dNew, tau = tau_dem, robust = list(method = "weave"),
             cite = FALSE)

DN6 <- zelig(violence ~ high_prop + dem_age + maj + internal_conflict +
                 women_in_parl,
             model = "relogit", data = dNew, tau = tau_dem,
             robust = list(method = "weave"), cite = FALSE)

DN7 <- zelig(violence ~ high_prop + dem_age + maj + internal_conflict +
                 murder_rate,
             model = "relogit", data = dNew, tau = tau_dem,
             robust = list(method = "weave"), cite = FALSE)

DN8 <- zelig(violence ~ high_prop + dem_age + maj + internal_conflict +
                 federal + govfrac,
             model = "relogit", data = dNew, tau = tau_dem,
             robust = list(method = "weave"), cite = FALSE)

DN9 <- zelig(violence ~ high_prop + dem_age + maj +internal_conflict + enps,
             model = "relogit", data = dNew, tau = tau_dem,
             robust = list(method = "weave"), cite = FALSE)

DN10 <- zelig(violence ~ high_prop + dem_age + maj + internal_conflict + gini,
             model = "relogit", data = dNew, tau = tau_dem,
             robust = list(method = "weave"), cite = FALSE)

DN11 <- zelig(violence ~ high_prop + dem_age + maj + internal_conflict +
                 log(gdp_per_capita),
             model = "relogit", data = dNew, tau = tau_dem,
             robust = list(method = "weave"), cite = FALSE)


#### Alternate PR system variable
### Sample of countries with elected legislatures from 1990
DNP1 <- zelig(violence ~ pr + dem_age + maj, model = "relogit",
             data = dNew, tau = tau_dNew, robust = list(method = "weave"),
             cite = FALSE)

DNP2 <- zelig(violence ~ pr + dem_age + maj + internal_conflict,
             model = "relogit", data = dNew, tau = tau_dNew,
             robust = list(method = "weave"),
             cite = FALSE)

DNP3 <- zelig(violence ~ pr + dem_age + maj+ internal_conflict +
                 cw_surv_self_expr, model = "relogit",
             data = dNew, tau = tau_dem, robust = list(method = "weave"),
             cite = FALSE)

DNP4 <- zelig(violence ~ pr + dem_age + maj+ internal_conflict +
                 ethnic_alesina, model = "relogit",
             data = dNew, tau = tau_dem, robust = list(method = "weave"),
             cite = FALSE)

DNP5 <- zelig(violence ~ pr + dem_age + maj + internal_conflict +
                 women_in_parl,
             model = "relogit", data = dNew, tau = tau_dem,
             robust = list(method = "weave"), cite = FALSE)

DNP6 <- zelig(violence ~ pr + dem_age + maj + internal_conflict +
                 murder_rate,
             model = "relogit", data = dNew, tau = tau_dem,
             robust = list(method = "weave"), cite = FALSE)

DNP7 <- zelig(violence ~ pr + dem_age + maj + internal_conflict +
                 federal + govfrac,
             model = "relogit", data = dNew, tau = tau_dem,
             robust = list(method = "weave"), cite = FALSE)

DNP8 <- zelig(violence ~ pr + dem_age + maj +internal_conflict + enps,
             model = "relogit", data = dNew, tau = tau_dem,
             robust = list(method = "weave"), cite = FALSE)

DNP9 <- zelig(violence ~ pr + dem_age + maj + internal_conflict + gini,
              model = "relogit", data = dNew, tau = tau_dem,
              robust = list(method = "weave"), cite = FALSE)

DNP10 <- zelig(violence ~ pr + dem_age + maj + internal_conflict +
                  log(gdp_per_capita),
              model = "relogit", data = dNew, tau = tau_dem,
              robust = list(method = "weave"), cite = FALSE)
