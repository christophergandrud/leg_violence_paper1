##########################
# Combine Violence Data Sets
# Christopher Gandrud
# 30 March 2015
# MIT License
##########################

# Load required packages
if (!('googlesheets' %in% installed.packages()[, 1]))
    devtools::install_github('jennybc/googlesheets')
library(googlesheets)
if (!('rio' %in% installed.packages()[, 1]))
    devtools::install_github('leeper/rio')
library(rio)
library(dplyr)
library(lubridate)

#### Download original googlesheets versions ###
main <- register_ss('10cK1dmAoWgBLtayc3lFMKeLQ7IM_lq0MG1G61K5VeDo')

newer <- main %>% get_via_csv(ws = 'Version2')

# New ymd variable
newer$date <- sprintf('%s-%s-%s', newer$year, newer$month, newer$day) %>% 
                ymd


#### Download pre-2012 version ####
older_url <- 'https://raw.githubusercontent.com/christophergandrud/leg_violence_paper1/master/Data/raw/leg_data_pre_2012.csv'
older <- import(older_url)

# New ymd variable
older$date <- sprintf('%s-%s-%s', older$Year, older$Month, older$Day) %>% 
                    ymd
