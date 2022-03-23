# 1_lexis_diagram.R
# Lexis diagram of judges
# March 2022
library(Epi)
library(dplyr)
library(season) # for year fraction
library(ggplot2)

# from 0_read_judges.R
load('data/analysis_ready.RData')

# prepare judges data for Lexis diagram
judges = mutate(judges, 
                dead = as.numeric(status=='Dead'),
    dob = as.numeric(format(birth_date,'%Y')) + yrfraction(birth_date),
    agein = as.numeric(difftime(at_risk_date, birth_date, units='days')/365.25),
    ageout = as.numeric(difftime(death_censor_date, birth_date, units='days')/365.25))

#### Section 1: Static Lexis Diagram ####
# States are 0 = voted in, 1 = dead, 2 = still alive 
judgesL <- Lexis(entry = list("period" = agein + dob, "age" = agein), exit = list("age" = ageout), 
                  entry.status=0, exit.status = dead, id = nid, data = judges, states=c('Dead','Alive'))
# with different symbol for dead/alive:
plot(judgesL, type='b', ylim=c(20, 105), pch=c(1,16)[as.numeric(judgesL$dead==1)+1])
summary(judgesL)

# export
outfile = 'figures/Lexis.jpg'
jpeg(outfile, width=5, height=4, units='in', res=500, quality = 100)
par(mai=c(0.8, 0.8, 0.05, 0.05), las=1)
plot(judgesL, type='l', ylim=c(25, 106), ylab='Age, years', xlab='Year')
points(filter(judgesL, status=='Dead'), type='p', ylim=c(25, 106), pch=1, col='red', cex=0.5) # just dead
dev.off()

# save for 2_summary.Rmd
save(judgesL, file='results/lexis.RData')

