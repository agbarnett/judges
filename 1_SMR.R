# 1_SMR.R
# Calculate annual standardised mortality ratio
# Extended to examine multiple follow-up times for animations
# March 2022
library(dplyr)

## load data, from 0_read_judges.R
load('data/analysis_ready.RData')

## only judges at risk from 1850 onwards (start of life table)
n = nrow(judges)
judges = filter(judges,
                at_risk_date >= as.Date('1850-01-01'))
pre_1850 = n - nrow(judges)

# Is there imputed life table data
judges = mutate(judges, entry.year = as.numeric(format(at_risk_date, '%Y'))) %>% # create entry year, needed later for fractional year in election year
  rename('Status' = 'status', # rename to suit politicians code
         'Sex' = 'gender')

## Outer loop for sensitivity analysis with specific years of follow-up
SMR = ages = NULL
for (follow.cut in c(10:60, 100)){ # years of follow-up after entry in scale of years ; add 100 as largest possible follow-up
  # Calculate follow-up time depending on years of follow-up
  # note using death_censor_date which is a combination of death and censoring (those still alive)
  to.SMR = mutate(judges, 
                  follow.time = as.numeric(difftime(death_censor_date, at_risk_date, units='days')/365.25), # observed follow-up time in years from entry to death (or censoring)
                  death_datex = ifelse(follow.time > follow.cut, at_risk_date + (follow.cut*365.25), death_censor_date), # censor time at cut-off time if observed time is longer than time to death
                  death_datex = as.Date(death_datex, origin='1970-01-01'),
                  Statusx = ifelse(follow.time > follow.cut, 'Living', 'Dead'), # change status to living if still alive at this time
                  Statusx = ifelse(death_datex >= meta$censor.date, Status, Statusx), # revert back to previous status if beyond censor date
                  death_datex = ifelse(death_datex >= meta$censor.date, death_censor_date, death_datex), # revert back to previous time if beyond censor date
                  death_datex = as.Date(death_datex, origin='1970-01-01')) %>%
    select(-death_date, -Status) %>% # over-write Status and date-of-death
    rename('death_date' = 'death_datex', 'Status' = 'Statusx')
  
  ## Inner loop of calculating SMR
  # merge a year of judges with a year of data
  for (year in meta$Syear:meta$Eyear){ 
    # get life table for this year
    this.life = filter(life.table, Year==year) %>% 
      dplyr::select(Age, Sex, qx, imputed)
    # get relevant judges
    year.start = as.Date(paste(year, '-01-01', sep="")) # start of year
    year.end = as.Date(paste(year, '-12-31', sep="")) # end of year
    # calculate expected deaths; 
    exp = filter(to.SMR, at_risk_date <= year.end & death_date >= year.start) %>% # entered some time during the year and were alive at the start of the year
      mutate(age = floor(as.numeric(difftime(year.start, birth_date, units='days')/365.25)),  # calculate age at start of year ('current' age)
             age = ifelse(age > meta$Oldest, meta$Oldest, age)) # change any ages over limit of life table
    # calculate average age of judges
    av.age = summarise(exp, mean=mean(age)) %>%
      mutate(follow=follow.cut, year = year)
    ages = rbind(ages, av.age)
    # fractional first year (could not get this to work in dplyr)
    index = exp$entry.year == year
    exp$frac = 1 
    if(any(index)){exp$frac[index] = as.numeric(difftime(year.end, exp$at_risk_date[index], units='days'))/365.25}
    # merge with life table by age and sex
    exp = left_join(exp, this.life, by=c('age'='Age', 'Sex'='Sex')) %>% 
      summarise(n = n(), # number contributing to expected
                expected = sum(qx*frac)) # expected deaths, fractional time in first year
    
    # calculate observed deaths
    obs = filter(to.SMR, death_date >= year.start & death_date <= year.end & Status=='Dead') %>% # died this year (not censored)
      summarise(observed=n())
    # calculate SMR and concatenate; add imputed (yes/no) flag for life tables
    frame = data.frame(follow=follow.cut, year=year, O=obs$observed, E=exp$expected, n=exp$n, imputed=this.life$imputed[1])
    SMR = rbind(SMR, frame)
  } # end of inner loop 
} # end of outer loop for follow-cut
SMR = mutate(SMR, SMR=O/E) # calculate SMR

## save
outfile = 'data/SMR.RData'
save(SMR, ages, meta, file=outfile)

# quick check
with(filter(SMR, follow==100), plot(year, SMR))
with(filter(ages, follow==100), plot(year, mean))
