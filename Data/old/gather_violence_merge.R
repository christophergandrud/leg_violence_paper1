##########################
# Combine Violence Data Sets
# Christopher Gandrud
# 30 March 2015
# MIT License
##########################

# Set working directory. Change as needed.
setwd('/git_repositories/leg_violence_paper1/')

# Load required packages
if (!('googlesheets' %in% installed.packages()[, 1]))
    devtools::install_github('jennybc/googlesheets')
library(googlesheets)
if (!('rio' %in% installed.packages()[, 1]))
    devtools::install_github('leeper/rio')
library(rio)
library(dplyr)
library(lubridate)
library(countrycode)
library(DataCombine)

#### Download original googlesheets versions ###
main <- register_ss('10cK1dmAoWgBLtayc3lFMKeLQ7IM_lq0MG1G61K5VeDo')

newer <- main %>% get_via_csv(ws = 'Version2')

# New ymd variable
newer$date <- sprintf('%s-%s-%s', newer$year, newer$month, newer$day) %>% 
                ymd
newer$iso2c <- countrycode(newer$country, origin = 'country.name', 
                           destination = 'iso2c')

newer_sub <- newer %>% select(iso2c, date, URL) %>% as.data.frame


#### Download pre-2012 version ####
older_url <- 'https://raw.githubusercontent.com/christophergandrud/leg_violence_paper1/master/Data/raw/leg_data_pre_2012.csv'
older <- import(older_url)

# New ymd variable
older$date <- sprintf('%s-%s-%s', older$Year, older$Month, older$Day) %>% 
                    ymd
older$iso2c <- countrycode(older$Country, origin = 'country.name', 
                           destination = 'iso2c')
older <- older %>% dplyr::rename(URL = `Link to source`)

older_sub <- older %>% select(iso2c, date, URL)

#### Fill in Missing URLs ####
comb <- FillIn(D1 = newer_sub, D2 = older_sub, Var1 = 'URL', Var2 = 'URL',
               KeyVar = c('iso2c', 'date'))

only_old <- anti_join(older_sub, comb)

comb <- rbind(comb, only_old)

comb$country <- countrycode(comb$iso2c, origin = 'iso2c', 
                            destination = 'country.name')

comb <- comb %>% MoveFront('country') %>% dplyr::rename(source_url = URL) %>%
        arrange(country, date)

# Drop US-Alabama 2007 fight as it is not a national legislature
comb <- comb %>% DropNA('iso2c')

#### Save 
export(comb, 'Data/violence_sources.csv')

