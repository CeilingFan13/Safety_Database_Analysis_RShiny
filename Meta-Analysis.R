## load libraries 
## Al Amer, F. M., Thompson, C. G., & Lin, L. (2021). Bayesian Methods for Meta-Analyses of Binary Outcomes: Implementations, Examples, and Impact of Priors. International journal of environmental research and public health, 18(7), 3492. https://doi.org/10.3390/ijerph18073492
library("rjags")
library("coda")

# mu = baseline risk in each study
# thetha = overall log OR
# tau x tau = between-studies variance
# delta = true log ORs within studies
# log_or = log odd ratio
# prec = precision; error measure
################################################################
## functions
ma <- function(prior.distribution){
  # inverse-gamma; alpha > 0: shape, beta > 0: scale
  # As both hyper-parameters approach zero, 
  # this prior corresponds to a flat prior for tau x tau
  if (prior.distribution == "Inverse-Gamma") {
    out <- "model{
 for(i in 1:n.studies){
 delta[i,1] <- 0
 mu[i] ~ dnorm(0, 0.0001) # vague priors for trial baselines
 for(k in 1:2){
 r[i,k] ~ dbin(p[i,k], n[i,k]) # binomial likelihood with probability parameter p and integer size parameter n
 logit(p[i,k]) <- mu[i] + delta[i,k]
 }
 delta[i,2] ~ dnorm(log_or, prec) # trial-specific log_or distributions
 }
 log_or ~ dnorm(0, 0.0001) # vague priors for log odds ratio
 # inverse gamma prior
 tau2 <- 1/prec
 tau <- sqrt(1/prec)
 prec ~ dgamma(alpha, beta)
 OR <- exp(log_or)
    }" }
  # uniform distribution, U(0, c)
  # c determines the prior’s upper bound
  else if (prior.distribution == "Uniform") {
    out <- "model{
 for(i in 1:n.studies){
 delta[i,1] <- 0
 mu[i] ~ dnorm(0, 0.0001) # vague priors for trial baselines
 for(k in 1:2){
 r[i,k] ~ dbin(p[i,k], n[i,k]) # binomial likelihood
 logit(p[i,k]) <- mu[i] + delta[i,k]
 }
 delta[i,2] ~ dnorm(log_or, prec) # trial-specific log_or distributions
 }
 log_or ~ dnorm(0, 0.0001) # vague priors for log odds ratio
 # uniform prior
 tau2 <- tau*tau
 prec <- 1/tau2
 tau ~ dunif(alpha, beta)
 OR <- exp(log_or)
 }" }
  else if (prior.distribution == "Half-Normal") {
    out <- "model{
 for(i in 1:n.studies){
 delta[i,1] <- 0
 mu[i] ~ dnorm(0, 0.0001) # vague priors for trial baselines
 for(k in 1:2){
 r[i,k] ~ dbin(p[i,k], n[i,k]) # binomial likelihood
 logit(p[i,k]) <- mu[i] + delta[i,k]
 }
 delta[i,2] ~ dnorm(log_or, prec) # trial-specific log_or distributions
 }
 log_or ~ dnorm(0, 0.0001) # vague priors for log odds ratio
 # half-mormal prior
 tau2 <- tau*tau
 prec <- 1/tau2
 tau ~ dnorm(alpha, 1/beta) T(0,)
 OR <- exp(log_or)
    }" }
  # make sure to include table 1 from the paper
  else if (prior.distribution == "Log-Normal") {
    out <- "model{
 for(i in 1:n.studies){
 delta[i,1] <- 0
 mu[i] ~ dnorm(0, 0.0001) # vague priors for trial baselines
 for(k in 1:2){
 r[i,k] ~ dbin(p[i,k], n[i,k]) # binomial likelihood
 logit(p[i,k]) <- mu[i] + delta[i,k]
 }
 delta[i,2] ~ dnorm(log_or, prec) # trial-specific log_or distributions
 }
 log_or ~ dnorm(0, 0.0001) # vague priors for log odds ratio
 # log-normal prior
 prec <- 1/tau2
 tau <- sqrt(1/prec)
 tau2 ~ dlnorm(alpha, 1/(beta)^2)
 OR <- exp(log_or)
    }"
    }
  else {
    print("Check prior distribution")
  }
  return(out)
}

#--------------------------------------------------------------------------------

prior_results <- function(data, prior, alpha, beta,
                          n.burnin, n.iter, n.chains,
                          n.adapt = 1000, thin = 2, seed = 1234){
  dat.jags <- list(n.studies = length(data$r1), alpha = alpha, beta = beta, 
                   r = cbind(data$r1,data$r2), n = cbind(data$n1, data$n2))
  inits <- list()
  for(i in 1:n.chains){
    num <- "123"
    end <- i + 3
    for (j in 4:end) {
      num <- paste(num, j, sep = "")
    } 
    init <- list(log_or = 0, mu = rep(0, length(data$r1)),
                  .RNG.name = "base::Wichmann-Hill", .RNG.seed = as.integer(num))
    inits[[i]] <- init
    }
    
  params <- c("OR", "log_or", "prec", "tau", "tau2")
  set.seed(seed)
  jags.ma <- jags.model(file = textConnection(ma(prior)), data = dat.jags,
                        n.chains = n.chains, n.adapt = n.adapt, inits = inits)
  update(jags.ma, n.iter = n.burnin)
  coda.ma <- coda.samples(model = jags.ma, variable.names = params,
                          n.iter = n.iter, thin = thin)
  smry.ma <- summary(coda.ma)
  # print(smry.ma)
  # print(smry.ma$quantiles[c("OR", "tau") , c("2.5%", "50%", "97.5%")])
  out <- round(smry.ma$quantiles[ , c("2.5%", "50%", "97.5%")], digits = 2)
  colname <- c("lb", "median", "ub")
  return(list(out, coda.ma))
  dev.off()
  
}
