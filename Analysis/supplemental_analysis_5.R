##################
# Fractionalisation list-wise inclusion
# Christopher Gandrud
# 26 March 2015
#################


F1 <- zelig(violence ~ ethnic_alesina , model = "relogit",
            data = dNew, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

F2 <- zelig(violence ~ ethnic_alesina + log(dem_age), model = "relogit",
            data = dNew, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

F3 <- zelig(violence ~ ethnic_alesina + log(dem_age) + maj, model = "relogit",
            data = dNew, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

F4 <- zelig(violence ~ ethnic_alesina + log(dem_age) + maj + high_prop, 
            model = "relogit",
            data = dNew, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)
