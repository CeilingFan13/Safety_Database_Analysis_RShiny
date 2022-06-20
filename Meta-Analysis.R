## load libraries 
library("rjags")
library("coda")

# mu = baseline risk in each study
# thetha = overall log OR
# tau x tau = between-studies variance
# delta = true log ORs within studies
# log_or = log odd ratio
# prec = error measure
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
 r[i,k] ~ dbin(p[i,k], n[i,k]) # binomial likelihood
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
  # write.table(matrix(out, byrow = FALSE, nrow = 5), file = "summary1_MA.txt",
  #             row.names = c("OR", "log_or", "prec", "tau", "tau2"),
  #             col.names = colname)
  # # trace plot of log OR
  # png(paste0(paste("traceplot_", prior), "_log_or", ".png"),
  #     res = 600, height = 8.5, width = 11, units = "in")
  # par(mfcol = c(length(coda.ma), 1))
  # for(k in 1:length(coda.ma)){
  #    temp <- as.vector(coda.ma[[k]][,"log_or"])
  #    p <- plot(temp, type = "l", col = "red", cex.lab = 1.5, cex.main = 1.5,
  #         xlab = "Iteration", ylab = "Log odds ratio",
  #         main = paste("Chain", k))
  # }
  return(list(out, coda.ma))
  dev.off()
  # density plot of log OR
  png(paste0("densityplot", "log_or", ".png"),
      res = 600, height = 8.5, width = 11, units = "in")
  post.coda <- NULL
  for(k in 1:length(coda.ma)){
    post.coda <- rbind(post.coda, coda.ma[[k]][,"log_or"])
  }
  postden <- density(post.coda)
  plot(postden, main = "Posterior density", xlab = "Log odds ratio")
  polygon(postden, col = "lightblue", border = "darkblue")
  dev.off()
}
# ################################################################
# ## data of five real-world meta-analyses
# Stillbirth <- list(r1 = c(20, 1884, 1402, 179, 1144, 257, 1832, 1309,
#                           3407, 145, 660, 803, 477),
#                    r2 = c(2, 45, 18, 5, 72, 5, 106, 50, 222, 3, 21, 12, 13),
#                    n1 = c(3160, 402201, 524328, 33715, 242672, 70942, 533258,
#                           306627, 568315, 51762, 242881, 250769, 144565),
#                    n2 = c(316, 1979, 2363, 364, 2168, 373, 3161, 2677, 5996,
#                           348, 1323, 2058, 872))
# PPI <- list(r1 = c(10, 66, 20, 27, 522, 31, 31, 95),
#             r2 = c(16, 62, 14, 43, 570, 63, 63, 116),
#             n1 = c(63, 87, 194, 682, 1381, 1149, 1149, 3297),
#             n2 = c(63, 86, 146, 682, 1412, 2301, 2301, 2949))
# data <- list(r1 = c(2, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0),
#                 r2 = c(6, 2, 0, 2, 0, 3, 0, 3, 2, 2, 2, 1, 0),
#                 n1 = c(255, 268, 129, 135, 309, 309, 62, 151, 171, 171, 578,
#                        205, 102),
#                 n2 = c(266, 287, 131, 142, 343, 339, 59, 154, 179, 178, 609,
#                        206, 268))
# Hepatitis <- list(r1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#                   r2 = c(0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 2, 0, 0),
#                   n1 = c(255, 268, 129, 135, 111, 309, 309, 62, 397, 151,
#                          171, 171, 578, 205, 102),
#                   n2 = c(266, 287, 131, 142, 236, 343, 339, 59, 406, 154,
#                          179, 178, 609, 206, 268))
# ARTI <- list(r1 = c(33, 69, 126, 54, 11, 458, 29, 245, 53, 155, 39, 38, 276,
#                     96, 80, 17, 53, 75, 93, 58, 10, 93, 5, 24, 14),
#              r2 = c(32, 68, 97, 39, 4, 438, 30, 260, 44, 154, 26, 26, 303,
#                     185, 70, 32, 94, 76, 85, 83, 10, 110, 4, 17, 16),
#              n1 = c(76, 167, 229, 84, 24, 1030, 89, 1505, 103, 161, 62, 58,
#                     360, 197, 234, 99, 80, 118, 125, 103, 11, 207, 35, 52, 16),
#              n2 = c(81, 167, 224, 80, 24, 1034, 86, 1506, 141, 161, 62, 58,
#                     399, 397, 258, 148, 156, 122, 125, 137, 14, 201, 54, 55, 18))
# ################################################################
# ## Bayesian meta-analysis for each dataset
# data <- Colitis #Hepatitis #Stillbirth # PPI, Colitis, Hepatitis, or ARTI
#prior_results(data = data, prior = "Inverse-Gamma", alpha = 0.001, beta = 0.001)
# prior_results(data = data, prior = "Inverse-Gamma", alpha = 0.01, beta = 0.01)
# prior_results(data = data, prior = "Inverse-Gamma", alpha = 0.1, beta = 0.1)
#prior_results(data = data, prior = "Uniform", alpha = 0, beta = 2)
# prior_results(data = data, prior = "Uniform", alpha = 0, beta = 10)
# prior_results(data = data, prior = "Uniform", alpha = 0, beta = 100)
# prior_results(data = data, prior = "Half-Normal", alpha = 0, beta = 0.1)
# prior_results(data = data, prior = "Half-Normal", alpha = 0, beta = 1)
# prior_results(data = data, prior = "Half-Normal", alpha = 0, beta = 2)
# # for Stillbirth and PPI
# prior_results(data = data, prior = "Log-Normal", alpha = -3.93, beta = 1.51)
# prior_results(data = data, prior = "Log-Normal", alpha = -2.89, beta = 1.91)
# prior_results(data = data, prior = "Log-Normal", alpha = -2.01, beta = 1.64)
# # for Colitis, Hepatitis, and ARTI
# prior_results(data = data, prior = "Log-Normal", alpha = -4.06, beta = 1.45)
# prior_results(data = data, prior = "Log-Normal", alpha = -3.02, beta = 1.85)
# prior_results(data = data, prior = "Log-Normal", alpha = -2.13, beta = 1.58)
# 

