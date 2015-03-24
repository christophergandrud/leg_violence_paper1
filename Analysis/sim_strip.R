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