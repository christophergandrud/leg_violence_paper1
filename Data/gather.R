# ---------------------------------------------------------------------------- #
# Gather/Clean/Merge Data for Two Sword Lengths Apart
# Christopher Gandrud
# 2 April 2015
# MIT License
# ---------------------------------------------------------------------------- #

# Set working directory. Change as needed.
setwd('/git_repositories/leg_violence_paper1/')

# Load packages
library(dplyr)
library(WDI)
library(countrycode)
library(psData)
library(DataCombine)
if (!('rio' %in% installed.packages()[, 1]))
    devtools::install_github('leeper/rio')
library(rio)
library(foreign)
library(tidyr)
library(repmis)
library(stringr)
library(lubridate)

#### ----------------- Main Leg. Violence Data ---------------------------- ####
main_violence <- import('Data/violence_sources.csv') %>%
                    dplyr::select(iso2c, date)

main_violence$year <- year(main_violence$date)
main_violence <- main_violence %>% filter(year <= 2012)
main_violence$violence <- 1

main_violence <- main_violence %>% group_by(iso2c, year) %>%
    mutate(violence_y_cum = sum(violence))

#### ------------------ World Bank Development Indicators ----------------- ####
indicators_wdi <- c('NY.GDP.PCAP.KD')
wdi <- WDI(indicator = indicators_wdi, start = 1975, end = 2014, extra = T) %>%
    dplyr::rename(gdp_per_capita = NY.GDP.PCAP.KD) %>%
    filter(region != 'Aggregates') %>%
    dplyr::select(iso2c, year, gdp_per_capita)

# GDP to thousands of dollars
wdi$gdp_per_capita <- wdi$gdp_per_capita / 1000

wdi <- wdi[!duplicated(wdi[, c('iso2c', 'year')]),]

#### ------------------ GINI ---------------------------------------------- ####
gini <- import('Data/raw/wiid_3b_1.csv')
gini <- gini[c('Country', 'Year', 'Gini')]
gini$iso2c <- countrycode(gini$Country, origin = 'country.name',
                          destination = 'iso2c')
gini <- gini %>% dplyr::select(iso2c, Year, Gini)
names(gini) <- c('iso2c', 'year', 'gini')

# Average multiple sources
gini <- gini %>% group_by(iso2c, year) %>%
            summarise(gini = mean (gini, na.rm = T))

gini <- gini[!duplicated(gini[, c('iso2c', 'year')]),]

#### ------------------- Murder Rate -------------------------------------- ####
murder <- import('Data/raw/UNdata_HomicideRate.csv') %>%
            dplyr::select(`Country or Area`, Year, Rate) %>%
            rename(country = `Country or Area`)

murder$iso2c <- countrycode(murder$country, origin = 'country.name',
                          destination = 'iso2c')
murder$Rate <- as.numeric(murder$Rate)
murder <- murder %>% dplyr::select(iso2c, Year, Rate)
names(murder) <- c('iso2c', 'year', 'murder_rate')

#### ------------------ Political Constraints ----------------------------- ####
pol_constraints <- import('Data/raw/polcon2012.dta') %>%
                    select(polity_country, year, polconiii, polconv)

pol_constraints$iso2c <- countrycode(pol_constraints$polity_country,
                                     origin = 'country.name',
                                     destination = 'iso2c')

pol_constraints <- DropNA(pol_constraints, 'iso2c')
pol_constraints <- pol_constraints %>% select(-polity_country) %>%
                    filter(year >= 1980)


#### --------------- Armed Conflict --------------------------------------- ####
conflict <- import('Data/raw/UCDPPrioArmedConflictDataset4a-2014.csv') %>%
                filter(Year >= 1980) %>%
                dplyr::select(ConflictId, Location, Year, TypeOfConflict)

conflict <- conflict[!duplicated(conflict[, c('Location', 'Year')]), ] %>%
                arrange(Location, Year)

# Extract conflicts in involving two countries
split_location <- str_split_fixed(conflict$Location, ', ', n = 2) %>%
                    as.data.frame
split_location$row_id <- 1:nrow(conflict)
sub_location <- subset(split_location, V2 != '' & V2 != 'FYR')

sub_conflict <- conflict[sub_location$row_id, ]
sub_conflict$Location <- sub_location$V2

# Clean Iraq 2003 war
sub_conflict <- sub_conflict[-2, ]
iraq_war <- data.frame(ConflictId = rep('1-226', 3),
                       Location = c('Iraq', 'Unites Kingdom', 'United States'),
                       Year = rep(2003, 3),
                       TypeOfConflict = rep(2, 3))

# Combine
conflict <- rbind(conflict[!(1:nrow(conflict) %in% sub_location$row_id), ],
                  sub_conflict)
conflict <- rbind(conflict, iraq_war)

# Clean up
conflict$iso2c <- countrycode(conflict$Location, origin = 'country.name',
                              destination = 'iso2c')

# Recode conflict
conflict$internal_conflict <- 1
conflict$internal_conflict[conflict$TypeOfConflict < 3] <- 0

conflict <- conflict %>% DropNA('iso2c') %>%
                dplyr::select(iso2c, Year, TypeOfConflict, internal_conflict) %>%
                arrange(iso2c, Year)
names(conflict) <- c('iso2c', 'year', 'type_of_conflict', 'internal_conflict')

#### --------------- Dominant Tier Personal Vote--------------------------- ####
# Johnson and Wallack's (n.d.) Domminant Personalisation Index
# Downloaded from http://hdl.handle.net/1902.1/17901

personal_vote <- import('Data/raw/espv_sep2010.xls', sheet = 2)

personal_vote <- personal_vote[3:nrow(personal_vote), c(2, 4, 10)]
names(personal_vote) <- c('country', 'year', 'dom_personal_vote')
personal_vote$year <- as.integer(personal_vote$year)
personal_vote$dom_personal_vote <- as.numeric(personal_vote$dom_personal_vote)

personal_vote$iso2c <- countrycode(personal_vote$country, 
                                   origin = 'country.name', 
                                   destination = 'iso2c')
personal_vote <- personal_vote %>% DropNA(c('iso2c', 'dom_personal_vote')) %>%
                    select(-country)


#### --------------- Women in Parliament ---------------------------------- ####
# From 1997 data from Inter-Parliamentary Union via World Bank Development
# Indicators
## WDI indicator ID: SG.GEN.PARL.ZS
women <- WDI(indicator = "SG.GEN.PARL.ZS", start = 1997) %>%
    dplyr::select(-country) %>% rename(women_in_parl = SG.GEN.PARL.ZS) %>%
    dplyr::select(iso2c, year, women_in_parl)

# Data from before 1997 from ICPSR: http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/24340
women_old <- read.dta('Data/raw/24340-0001-Data.dta')
women_old<- women_old[, 5:64]

women_old <- gather(women_old, year, women_in_parl, -COUNTRYN)
women_old$year <- women_old$year %>% gsub('P', '', .) %>% as.numeric

women_old$iso2c <- countrycode(women_old$COUNTRYN, origin = 'country.name',
                               destination = 'iso2c')

women_old <- women_old %>% dplyr::select(iso2c, year, women_in_parl) %>%
    arrange(iso2c, year) %>% filter(year >= 1980) %>%
    filter(year < 1997)

women <- rbind(women_old, women) %>% arrange(iso2c, year)

women <- women[!duplicated(women[, c('iso2c', 'year')]),]


#### --------------- Age of Democracy ------------------------------------- ####
polity <- PolityGet(url = 'http://www.systemicpeace.org/inscr/p4v2013.sav') %>%
    arrange(iso2c, year)

# Create democratic age variable
polity$democracy <- 0
polity$democracy[polity$polity2 > 5] <- 1
polity$democracy[is.na(polity$polity2)] <- NA

cum_nozero <- function(x){
    polity <- as.data.frame(polity)
    comb <- vector()
    for (u in unique(polity$iso2c)){
        message(u)
        temp <- subset(polity, iso2c == u)
        temp_cum <- rep(0, nrow(temp))

        for (i in 1:nrow(temp)){
            message(i)
            if (i == 1) {
                temp_cum[1] <- temp[1, x]
            }
            else if (i > 1) {
                if (is.na(temp[i, x])){
                    temp_cum[i] <- temp[i, x]
                }
                else if (!is.na(temp[i, x])){
                    if (!is.na(temp[i-1, x])){
                        if (temp[i-1, x] == 0 & temp[i, x] > 0) {
                            temp_cum[i] <- 1
                        }
                        else if (temp[i-1, x] > 0){
                            temp_cum[i] <- temp_cum[i-1] + 1
                        }
                    }
                    else if (is.na(temp[i-1, x])){
                        temp_cum[i] <- NA
                    }
                }
            }
        }
        comb <- c(comb, temp_cum)
    }
    return(comb)
}

polity$dem_age <- cum_nozero(x = 'democracy')

polity <- polity %>% dplyr::select(iso2c, year, polity2, dem_age)
polity <- polity[!duplicated(polity[, c('iso2c', 'year')]),]

#### ---------------------- Database of Political Institutions ------------ ####
tmpfile <- tempfile()
download.file('http://bit.ly/1jZ3nmM', tmpfile)
DpiData <- read.dta(tmpfile)
unlink(tmpfile)

# Correct South Africa
DpiData$countryname <- as.character(DpiData$countryname)
DpiData$countryname[DpiData$countryname == "S. Africa"] <- 'South Africa'
DpiData$iso2c <- countrycode(DpiData$countryname, origin = 'country.name',
                             destination = 'iso2c')

dpi <- DpiData %>% dplyr::select(iso2c, year, maj, system, govfrac, pr, liec)

for (i in names(dpi)) dpi[, i][dpi[, i] == -999] <- NA

# Convert maj from a proportion to a percent
dpi$maj <- dpi$maj * 100

# Create single_party government variable if govfrac == 0
dpi$single_party[!is.na(dpi$govfrac)] <- 0
dpi$single_party[dpi$govfrac == 0] <- 1
dpi$single_party[is.na(dpi$govfrac)] <- NA

dpi <- dpi[!duplicated(dpi[, c('iso2c', 'year')]),]

#### ----------------- Dispoportionality ---------------------------------- ####
disprop <- import('http://bit.ly/Ss6zDO', format = 'csv') %>%
    dplyr::select(iso2c, year, disproportionality) %>% filter(year >= 1980) %>%
    filter(year <= 2012)

# Create disproportionality threshold variable where 1 < 5.76
disprop$high_prop <- 0
disprop$high_prop[disprop$disproportionality < 6.34] <- 1
disprop$high_prop[is.na(disprop$high_prop)] <- NA

disprop <- disprop[!duplicated(disprop[, c('iso2c', 'year')]),]

#### ----------------- Legislative Immunity ------------------------------- ####
immunity <- import('Data/raw/fish_k_immunity.csv') %>% dplyr::select(-year)
immunity <- immunity[!duplicated(immunity[, 'iso2c']),]

#### ----------------- Ethnic Fractionalization---------------------------- ####
ethnic_frac <- 'http://www.anderson.ucla.edu/faculty_pages/romain.wacziarg/downloads/fractionalization.xls' %>%
    source_XlsxData(sheet = 1)
ethnic_frac <- ethnic_frac[3:217, c(1, 4)]
names(ethnic_frac) <- c('country', 'ethnic_alesina')
ethnic_frac$ethnic_alesina[ethnic_frac$ethnic_alesina == '.'] <- NA
ethnic_frac$ethnic_alesina <- ethnic_frac$ethnic_alesina %>% as.character %>%
    as.numeric

ethnic_frac$iso2c <- countrycode(ethnic_frac$country, origin = 'country.name',
                                 destination = 'iso2c')
ethnic_frac <- dplyr::select(ethnic_frac, iso2c, ethnic_alesina)
ethnic_frac <- ethnic_frac[!duplicated(ethnic_frac[, 'iso2c']),]

#### ------------------ World Values Survey ------------------------------- ####
wvs <- import('Data/raw/wvs.csv')
wvs <- wvs[!duplicated(wvs[, c('iso2c', 'year')]),]

#### ------------------ Federal ----------- ------------------------------- ####
federal <- import('Data/raw/federal.csv')
federal <- federal[!duplicated(federal[, c('iso2c', 'year')]),]

#### ------------------ Effective No. Parties------------------------------ ####
enpv_enps <- import('Data/raw/enpv_epns.csv')

#### ----------------- Merge together ------------------------------------- ####
## Merge dpi with disproportionality
comb <- merge(dpi, disprop, by = c('iso2c', 'year'), all = T) %>%
    arrange(iso2c, year)

## Merge in legislative violence variables
comb <- merge(main_violence, comb, by = c('iso2c', 'year'), all = T) %>%
    arrange(iso2c, year)

## Merge in polity
comb <- merge(comb, polity, by = c('iso2c', 'year'), all = T) %>%
    arrange(iso2c, year)

## Merge wdi
comb <- merge(comb, wdi, by = c('iso2c', 'year'), all.x = T) %>%
    arrange(iso2c, year)

## Merge in women
comb <- merge(comb, women, by = c('iso2c', 'year'), all.x = T) %>%
    arrange(iso2c, year)

## Merge in immunity
comb <- merge(comb, immunity, by = c('iso2c'), all.x = T) %>%
    arrange(iso2c, year)

## Merge in dominant personal vote
comb <- merge(comb, personal_vote, by = c('iso2c', 'year'), all.x = T) %>%
    arrange(iso2c, year)

comb <- comb %>% group_by(iso2c) %>% mutate(dom_personal_vote =
                                        FillDown(Var = dom_personal_vote)) %>%
            as.data.frame

## Merge ethic fractionalisation
comb <- merge(comb, ethnic_frac, by = c('iso2c'), all.x = T) %>%
    arrange(iso2c, year)

## Merge World Values survey and extend
comb <- merge(comb, wvs, by = c('iso2c', 'year'), all.x = T) %>%
    arrange(iso2c, year)
comb <- comb %>% group_by(iso2c) %>% mutate(higher_trust =
                                                FillDown(Var = higher_trust))
comb <- comb %>% group_by(iso2c) %>% mutate(cw_surv_self_expr =
                                        FillDown(Var = cw_surv_self_expr)) %>%
            as.data.frame

## Merge political constraints
comb <- merge(comb, pol_constraints, by = c('iso2c', 'year'), all.x = T) %>%
    arrange(iso2c, year)

## Merge federal and extend
comb <- merge(comb, federal, by = c('iso2c', 'year'), all.x = T) %>%
    arrange(iso2c, year)
comb <- comb %>% group_by(iso2c) %>% mutate(federal =
                                    FillDown(Var = federal)) %>% as.data.frame

## Merge in murder rate
comb <- merge(comb, murder, by = c('iso2c', 'year'), all.x = T) %>%
    arrange(iso2c, year)

## Merge enps/enpv
comb <- merge(comb, enpv_enps, by = c('iso2c', 'year'), all.x = T) %>%
    arrange(iso2c, year)
comb <- comb %>% group_by(iso2c) %>% mutate(enpv = FillDown(Var = enpv))
comb <- comb %>% group_by(iso2c) %>% mutate(enps = FillDown(Var = enps)) %>%
    as.data.frame

# Fill in disproportionality between election years
comb <- comb %>% group_by(iso2c) %>%
    mutate(disproportionality = FillDown(Var = disproportionality))
comb <- comb %>% mutate(high_prop = FillDown(Var = high_prop)) %>%
    as.data.frame

## Merge armed conflict
comb <- merge(comb, conflict, by = c('iso2c', 'year'), all.x = T) %>%
    arrange(iso2c, year)

# Assume all missing are non-conflict years
for (i in c('type_of_conflict', 'internal_conflict')) {
    comb[, i][is.na(comb[, i])] <- 0
}

## Merge gini
comb <- merge(comb, gini, by = c('iso2c', 'year'), all.x = T) %>%
    arrange(iso2c, year)
comb <- comb %>% group_by(iso2c) %>% mutate(gini = FillDown(Var = gini))

## Final clean
#### Only countries with elected legislatures
# comb <- comb %>% filter(liec > 5 & !is.na(liec))

comb <- comb %>% filter(!is.na(iso2c))

violence_sub <- comb %>% filter(!is.na(violence))

# Convert NAs to missing
for (i in c('violence', 'violence_y_cum')) comb[, i][is.na(comb[, i])] <- 0

# Limit to 1980-2012
comb <- comb %>% filter(year >= 1980) %>% filter(year <= 2012)

# Add country names
comb$country <- countrycode(comb$iso2c, origin = 'iso2c',
                            destination = 'country.name')
comb <- MoveFront(comb, c('country', 'iso2c', 'year'))

#### -------------------- Save -------------------------------------------- ####
export(comb, 'Data/LegislativeViolenceMain.csv')
