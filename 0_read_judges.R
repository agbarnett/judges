# 0_read_data.R
# read the judges data
# information about date here https://www.fjc.gov/history/judges/biographical-directory-article-iii-federal-judges-export
# March 2022
library(readxl)
library(janitor)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

# read the judges data, from https://www.fjc.gov/history/judges
judges = read_excel('data/US_Fedral_judges.xlsx') %>%
  clean_names() %>%
  dplyr::select('nid', 'first_name', 'last_name', 'gender', starts_with('birth_'), starts_with('death_'), 'recess_appointment_date_1','nomination_date_1', 'commission_date_1') %>%
  mutate(name = paste(first_name , last_name),
         birth_day = ifelse(is.na(birth_day), 15, birth_day), # missing day of birth
         birth_month = ifelse(is.na(birth_month), 6, birth_month), # missing month of birth
         death_day = ifelse(is.na(death_day) & !is.na(death_year), 15, death_day), # missing day of death (only if year not missing)
         death_month = ifelse(is.na(death_month) & !is.na(death_year), 6, death_month), # missing month of death (only if year not missing)
         nomination_date = as.Date(nomination_date_1, format='%Y-%m-%d'), # make dates
         commission_date = as.Date(commission_date_1, format='%Y-%m-%d'),
         recess_appointment_date_1 = as.Date(recess_appointment_date_1, format='%Y-%m-%d')) %>%
  dplyr::select(nid, name, gender, 'nomination_date', 'commission_date', 'recess_appointment_date_1', starts_with('birth_'), starts_with('death_'))
# check that ID is unique:
table(table(judges$nid))

# Missing birth year
n = nrow(judges)
judges = filter(judges, !is.na(birth_year))
no_birth_year = n - nrow(judges)

# make dates from day/month/year information
# combine two dates to get non-missing at-risk date (recess_appointment_date_1 only used for jid 3844)
judges = mutate(judges,
                at_risk_date = coalesce(nomination_date, commission_date, recess_appointment_date_1), # combine to get a non-missing date
                birth_date = as.Date(ISOdate(day = birth_day, month = birth_month, year = birth_year)),
                death_date = as.Date(ISOdate(day = death_day, month = death_month, year = death_year))) %>%
  dplyr::select(nid, name, gender, birth_date, at_risk_date, death_date)

## alive or dead
censor_date = as.Date('2022-03-01') # 1st March
judges = mutate(judges,
  status = ifelse(is.na(death_date)==TRUE, 'Living', 'Dead'),
  death_censor_date = ifelse(status=='Living', censor_date, death_date),
  death_censor_date = as.Date(death_censor_date, origin= '1970-01-01')) # death or censor date for those dead/alive

### Life tables ###
## using code from politicians ## see dumped.R for trying to use updated tables ##
# get life table data from politicians
load('../politicians/data/USA/USA.imputed.RData') # years 1850 to 2016
remove(politcians) # do not need this
# carry forward 2016 life table to 2017 to 2021
for (y in 2017:2021){
  carry = filter(life.table, Year==2016) %>% # use latest available
    mutate(Year = y, imputed = 'Yes') # overwrite Year
  life.table = bind_rows(life.table, carry) # concatenate
}
life.table = arrange(life.table, Year, Sex, Age)

## make meta-data, start and end year
meta = data.frame(country ='USA', 
                  Syear = 1850,  # start year for life tables
                  Eyear= 2021, 
                  Oldest = 100, # have up to 110 for some life table years 
                  censor.date = censor_date,
                  median.year = 1930,
                  imputed.life = TRUE)

## save
save(judges, life.table, no_birth_year, meta, file='data/analysis_ready.RData')

# quick check of death years
library(ggplot2)
years = filter(judges, !is.na(death_date)) %>%
        mutate(year = as.numeric(format(death_date, '%Y'))) %>%
  group_by(year) %>%
  tally()
ggplot(data=years, aes(x=year, y=n))+
  geom_line()+
  theme_bw()
