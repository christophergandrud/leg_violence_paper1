MinMaxLines <- function(df, byVars = "fitted", clean = T)
{
    require(dplyr)
    df <- dplyr::group_by_(df, .dots = byVars)

    Linesdf <- mutate(df, Median = median(value))
    Linesdf <- mutate(Linesdf, Max = quantile(value, 0.975))
    Linesdf <- mutate(Linesdf, Min = quantile(value, 0.025))
    Linesdf <- mutate(Linesdf, Lower50 = quantile(value, 0.25))
    Linesdf <- mutate(Linesdf, Upper50 = quantile(value, 0.75))

    Linesdf <- distinct_(Linesdf, .dots = byVars)

    Linesdf <- ungroup(Linesdf)

    if (isTRUE(clean)){
        Linesdf <- Linesdf[, c(byVars, 'Min', 'Lower50', 'Median', 'Upper50',
                               'Max')]
        names(Linesdf) <- c(byVars, 'Min_CI', 'Lower50_CI','Median',
                            'Upper50_CI', 'Max_CI')
    }
    class(Linesdf) <- 'data.frame'
    return(Linesdf)
}
