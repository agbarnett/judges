# 2_make_Bayes_SMR.R
# Create the Bayesian models of SMR in WinBUGS code
# with versions for politics models
# March 2022

## A. Bayes model of SMR with natural spline basis
bfile = 'bayes.ns.txt' # in R folder
bugs = file(bfile, 'w')
cat('model{
    for (i in 1:N){
    O[i] ~ dpois(mu[i])
    log(mu[i]) <- log(E[i]) + regress[i]
    regress[i] <- inprod(beta[1:J], X[i, 1:J])
    }
    for (k in 1:J){
    beta[k] ~ dnorm(0, 0.0001)
    }
  }', file=bugs)
close(bugs)

