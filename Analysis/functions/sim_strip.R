sim_strip <- function(estimates, fitted, fitted_values){
    require(tidyr)
    require(dplyr)
    
    # Simulate quantities of interest
    sim_x <- sim(estimates, x = fitted)
    
    # Extract/clean simulations
    extracted <- data.frame(simulation.matrix(sim_x, "Expected Values: E(Y|X)"))
    names(extracted) <- sprintf('X%s', fitted_values)
    extracted_gathered <- gather(extracted, fitted, value)
    extracted_gathered$fitted <- gsub('X', '', extracted_gathered$fitted) %>% 
                                    as.numeric
    
    # Find 0.95, median
    extracted_gathered_sum <- MinMaxLines(extracted_gathered)
    return(extracted_gathered_sum)
}

marginal_effect <- function(obj, b1, b2, X2, nsim = 1000){
    library(dplyr)
    # Create simulation ID variable
    SimID <- 1:nsim
    
    # Parameter estimates & Variance/Covariance matrix
    Coef <- matrix(coef(obj))
    VC <- vcov(obj)
    
    # Draw covariate estimates from the multivariate normal distribution
    Drawn <- mvrnorm(n = nsim, mu = Coef, Sigma = VC)
    DrawnDF <- data.frame(Drawn)
    dfn <- names(DrawnDF)
    
    bs <- c(b1, b2)
    bpos <- match(bs, dfn)
    binter <- paste0(bs[[1]], ".", bs[[2]])
    binter <- match(binter, dfn)
    NamesInt <- c(bpos, binter)
    
    Simb <- data.frame(SimID, Drawn[, NamesInt])
    
    X2df <- data.frame(X2)
    names(X2df) <- c("X2")
    Simb <- merge(Simb, X2df)
    
    Simb$QI <- Simb[, 2] + (Simb[, 4] * Simb[, 5])
    
    Simb <- Simb %>% select(X2, QI)
    names(Simb) <- c('fitted', 'value')
    
    out <- MinMaxLines(Simb)
    
    return(out)
}