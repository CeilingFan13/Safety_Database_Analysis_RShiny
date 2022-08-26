# H. Amy Xia , Haijun Ma & Bradley P. Carlin (2011) Bayesian Hierarchical Modeling for Detecting Safety Signals in Clinical Trials, Journal of Biopharmaceutical Statistics, 21:5, 1006-1029, DOI: 10.1080/10543406.2010.520181
# dplyr, tidyr, binom, ggplot2, data.table, foreach, doParallel, mcmcplots, rjags, R2jags
packages <-
  c(
    "ggplot2",
    "dplyr",
    "tidyr",
    "binom",
    "data.table",
    "foreach",
    "doParallel",
    "mcmcplots",
    "rjags",
    "R2jags"
  )

for (pac in packages) {
  if (pac %in% rownames(installed.packages()) == FALSE) {
    install.packages(pac)
  }
}
lapply(packages, require, character.only = TRUE)

preprocess <- function(AE, Nc = 50, Nt = 60) {
  # This function requires AE file consists of
  ## USUBJID: Unique Subject Identifier
  ## AEBODSYS: Body System or Organ Class (SOC)
  ## AEDECOD: Dictionary-Derived Term (Perferred Term, PT)
  ## TRTCRT: Treatment vs. control
  # and also total patient number in control and treatment groups
  AE1 <- count(AE, AEBODSYS, AEDECOD, TRTCTR)
  # get the number of entries for each combination of AEBODSYS and AEDECOD for TRTCTR=1 and TRTCTR=0, respectively
  AE2 <- spread(AE1, TRTCTR, n)
  
  # change the column names for AE2 with TRTCTR=1 be AEc, which means # of AEs in control groups
  # and TRTCTR=0 be AEt, which means # of AEs in treatment group
  colnames(AE2)[colnames(AE2) == '1'] <- 'AEt'
  colnames(AE2)[colnames(AE2) == '0'] <- 'AEc'
  AE2[is.na(AE2)] <- 0
  
  # get the total number of subjects in control and treatment group
  AE2$Nc <- Nc
  AE2$Nt <- Nt
  
  # for further analysis get 3 more columns for the dataset
  # b is a column of integers with each integer represent one SOC
  # i is a column of integers with each integer represent one PT
  # j is a column of integers with each integer which is the order of PT in each SOC
  AE2['b'] <- as.integer(factor(AE2$AEBODSYS))
  AE2['i'] <- as.integer(factor(AE2$AEDECOD))
  AE2 <-
    AE2 %>% group_by(b) %>% mutate(j = as.integer(factor(AEDECOD)))
  
  # add two columns to dataset,
  # Diff_raw%
  pi_treat <- round(AE2$AEt / AE2$Nt, digits = 2)
  pi_ctrl <- round(AE2$AEc / AE2$Nc, digits = 2)
  AE2$Diff_raw <- pi_treat - pi_ctrl
  # OR_raw, Odds ratio
  odds_treat <- round(pi_treat / (1 - pi_treat), digits = 2)
  odds_ctrl <- round(pi_ctrl / (1 - pi_ctrl), digits = 2)
  AE2$OR_raw <- round(odds_treat / odds_ctrl, digits = 2)
  
  # order the dataset according to SOC and PT
  AE2 <- AE2[order(AE2$b, AE2$j),]
  names(AE2)[1:2] <- c("SOC", "PT")
  # return the dataframe
  return (AE2)
}
################################################################################
# M1b: Binomial model with mixture prior----------------------------------------
model <-
  function(data,
           n_burn,
           n_iter,
           thin,
           n_adapt,
           n_chain,
           alpha.gamma = 3,
           beta.gamma = 1,
           alpha.theta = 3,
           beta.theta = 1,
           mu.gamma.0.0 = 0,
           tau.gamma.0.0 = 0.1,
           alpha.gamma.0.0 = 3,
           beta.gamma.0.0 = 1,
           lambda.alpha = 0.1,
           lambda.beta = 0.1,
           mu.theta.0.0 = 0,
           tau.theta.0.0 = 0.1,
           alpha.theta.0.0 = 3,
           beta.theta.0.0 = 1) {
    model.binom <- "model{
  for (i in 1:Nae) {
  X[i] ~ dbin(c[b[i], j[i]], Nc)
  Y[i] ~ dbin(t[b[i], j[i]], Nt)
  logit(c[b[i], j[i]]) <- gamma[b[i], j[i]]
  logit(t[b[i], j[i]]) <- gamma[b[i], j[i]] + theta[b[i], j[i]]
  gamma[b[i], j[i]] ~ dnorm(mu.gamma[b[i]], tau.gamma[b[i]])
  p0[i] ~ dbern(pi[b[i]] ) # prob of point mass
  theta1[b[i], j[i]] ~ dnorm(mu.theta[b[i]], tau.theta[b[i]])
  # theta=0 w.p. pi[i] and theta=theta1 w.p. 1-pi[i]
  theta[b[i], j[i]] <- (1- p0[i]) * theta1[b[i], j[i]]
  OR[b[i],j[i]] <- exp(theta[b[i],j[i]] )
  ORpv2[b[i], j[i]] <- step(OR[b[i],j[i]] - 2 )  # OR >= 2
  ORpv1.2[b[i], j[i]] <- step(OR[b[i],j[i]] -1.2 ) # OR >= 1.2
  ORpv[b[i], j[i]] <- 1- step(-OR[b[i],j[i]]) # OR >1
  RD[b[i], j[i]] <- t[b[i], j[i]] - c[b[i], j[i]]
  RDpv[b[i], j[i]] <- 1 - step(c[b[i], j[i]] - t[b[i], j[i]]) # RD>0
  RDpv2[b[i], j[i]] <- step(t[b[i], j[i]] - c[b[i], j[i]]- 0.02) # RD>=2%
  RDpv5[b[i], j[i]] <- step(t[b[i], j[i]] - c[b[i], j[i]]- 0.05) # RD>=5%
  D[i] <- X[i]*log(c[b[i], j[i]]) + (Nc-X[i])*log(1-c[b[i], j[i]]) + Y[i]*log(t[b[i], j[i]]) + (Nt-Y[i])*log(1-t[b[i],j[i]])
  Diff[b[i], j[i]] <- t[b[i], j[i]] - c[b[i], j[i]]
  }
  Dbar <- -2* sum(D[]) # -2logL without normalizing constant
  # SOC level parameters
  for(k in 1:B){
  pi[k] ~ dbeta(alpha.pi, beta.pi)
  mu.gamma[k] ~ dnorm(mu.gamma.0, tau.gamma.0)
  tau.gamma[k] ~ dgamma(alpha.gamma,beta.gamma)
  mu.theta[k] ~ dnorm(mu.theta.0, tau.theta.0)
  tau.theta[k] ~ dgamma(alpha.theta,beta.theta)
  }
  # hyperpriors for gammas;
  mu.gamma.0 ~ dnorm(mu.gamma.0.0, tau.gamma.0.0)
  tau.gamma.0 ~ dgamma(alpha.gamma.0.0, beta.gamma.0.0)
  # hyperpriors for thetas;
  mu.theta.0 ~ dnorm(mu.theta.0.0, tau.theta.0.0)
  tau.theta.0 ~ dgamma(alpha.theta.0.0,beta.theta.0.0)
  # hyperpriors for pi?s;
  alpha.pi ~ dexp(lambda.alpha)I(1,)
  beta.pi ~ dexp(lambda.beta)I(1,)
}"
    
    
    param <- c("OR", "Diff", "gamma", "theta")
    
    data <-
      list(
        Nae = nrow(data),
        Nc = data$Nc[1],
        Nt = data$Nt[1],
        B = max(data$b),
        b = data$b,
        j = data$j,
        Y = data$AEt,
        X = data$AEc,
        alpha.gamma = alpha.gamma,
        beta.gamma = beta.gamma,
        alpha.theta = alpha.theta,
        beta.theta = beta.theta,
        mu.gamma.0.0 = mu.gamma.0.0,
        tau.gamma.0.0 = tau.gamma.0.0,
        alpha.gamma.0.0 = alpha.gamma.0.0,
        beta.gamma.0.0 = beta.gamma.0.0,
        lambda.alpha = lambda.alpha,
        lambda.beta = lambda.beta,
        mu.theta.0.0 = mu.theta.0.0,
        tau.theta.0.0 = tau.theta.0.0,
        alpha.theta.0.0 = alpha.theta.0.0,
        beta.theta.0.0 = beta.theta.0.0
      )
    
    
    # # we use parallel computing for n_chain>1
    # if (n_chain>1){
    #   #setup parallel backend to use multiple processors
    #   library(foreach)
    #   library(doParallel)
    #   cores<-detectCores()
    #   cl<-makeCluster(cores[1]-1)
    #   registerDoParallel(cl)
    #
    #   param.est<-list()
    #   param.est<-foreach(m=1:n_chain) %dopar% {
    #     library(mcmcplots)
    #     library(rjags)
    #     library(R2jags)
    #     temp.fit <- jags.model(textConnection(model.binom), data=data, n.chains=1, n.adapt=n_adapt, quiet=TRUE)
    #     update(temp.fit, n.iter=n_burn)
    #
    #     # summary of posterior samples
    #     temp.param.samples <- coda.samples(temp.fit,param,n.iter=n_iter,thin=thin)
    #     temp.param.est <- data.frame(as.matrix(temp.param.samples))
    #     param.est[[m]]<-temp.param.est
    #   }
    #   stopCluster(cl)
    #
    #   ## combine the result from seperate chains together
    #   Final.est<-param.est[[1]]
    #   for (m in 2:n_chain){
    #     Final.est<-rbind(Final.est, param.est[[m]])
    #   }
    # }
    
    #else {
    temp.fit <-
      jags.model(
        textConnection(model.binom),
        data = data,
        n.chains = n_chain,
        n.adapt = n_adapt,
        quiet = TRUE
      )
    update(temp.fit, n.iter = n_burn)
    
    # summary of posterior samples
    temp.param.samples <-
      coda.samples(temp.fit, param, n.iter = n_iter, thin = thin)
    temp.param.est <- data.frame(as.matrix(temp.param.samples))
    Final.est <- temp.param.est
    #}
    
    return(Final.est)
  }
overview <- function(data, model_output) {
  # Get the statistics for parameters
  model_output_Diff <-
    model_output[, grepl("Diff", names(model_output))]
  model_output_OR <- model_output[, grepl("OR", names(model_output))]
  #model_output_ORpv <- model_output[, grepl("ORpv", names(model_output))]
  #model_output_RD <- model_output[, grepl("RD", names(model_output))]
  model_output_DiffOR <- cbind(model_output_Diff, model_output_OR)
  param.sum <- sapply(model_output_DiffOR, parameter_stat)
  summary <- as.data.frame(t(param.sum))
  # associate the parameters with SOC and PT names
  # sort data by j, since parameters in summary are sorted by j and then by b.
  AEDECOD <- data[order(data$j, data$b), ]
  SOC <- rep(as.character(AEDECOD$SOC), 2)
  PT <- rep(as.character(AEDECOD$PT), 2)
  Sub <- c(rep('Diff', nrow(data)), rep('OR', nrow(data)))
  col.name <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%")
  colnames(summary) <- col.name
  summary <- cbind(Sub, SOC, PT, summary)
  # Summary statistics for parameter Diff
  summary.diff <-
    summary[summary$Sub == 'Diff', c('SOC', 'PT', 'Mean', '2.5%', '97.5%')]
  colnames(summary.diff)[3:5] <-
    c('Diff_mean', 'Diff_2.5%', 'Diff_97.5%')
  # Summary statistics for parameter OR
  summary.OR <-
    summary[summary$Sub == 'OR', c('SOC', 'PT', '50%', '2.5%', '97.5%')]
  colnames(summary.OR)[3:5] <- c('OR_median', 'OR_2.5%', 'OR_97.5%')
  out <- merge(summary.diff, summary.OR, by = c('SOC', 'PT'))
  return(out)
}
merge_data_model <- function(data, summary) {
  all0 <- merge(data, summary, by = c("SOC", "PT"))
  all1 <- all0[,!names(all0) %in% c("b", "i", "j")]
  return(all1)
}
parameter_stat <- function(model_output) {
  # this functin will take the output from Hier_history as input
  # and return the summary statistics for each parameter
  # the summary function is applyed on each column
  xbar <- round(mean(model_output), digits = 2)
  xsd  <- round(sd(model_output), digits = 2)
  x2.5 <- round(quantile(model_output, 0.025), digits = 2)
  x25  <- round(quantile(model_output, 0.25), digits = 2)
  x50 <- round(quantile(model_output, 0.5), digits = 2)
  x75  <- round(quantile(model_output, 0.75), digits = 2)
  x97.5 <- round(quantile(model_output, 0.975), digits = 2)
  out <- c(xbar, xsd, x2.5, x25, x50, x75, x97.5)
  return(out)
}
topAE_table_plot <- function(result, param, num = 10) {
  # takes in result from merge_data_model and parameter (risk different or odds ratio)
  input <- copy(result)
  setnames(input, old = c("Diff_2.5%", "Diff_97.5%", "OR_2.5%", "OR_97.5%"), 
           new = c("Diff_lower", "Diff_upper", "OR_lower", "OR_upper"))
  input <- do.call(data.frame, lapply(input, function(x) replace(x, is.infinite(x), NA)))
  num <- min(num, dim(input)[1])
  if (param == "risk difference") {
    # order risk difference by mean and only take top few
    input_ordered0 <- head(input[order(input[, "Diff_mean"], decreasing = TRUE),], num)
    input_ordered <- rbind(input_ordered0, input_ordered0)
    input_ordered$Diff <- c(input_ordered$Diff_mean[1:num], 
                            input_ordered$Diff_raw[(num+1):(2*num)])
    input_ordered$Lower <- c(input_ordered$Diff_lower[1:num], rep(NA, 10))
    input_ordered$Upper <- c(input_ordered$Diff_upper[1:num], rep(NA, 10))
    input_ordered$Type <- c(rep("Model", num), rep("Raw", num))
    percent_c <- round(input_ordered$AEc*100/input_ordered$Nc, digits = 2)
    percent_t <- round(input_ordered$AEt*100/input_ordered$Nt, digits = 2)
    input_ordered$label <- paste0("Control: ", percent_c, "% vs. ", 
                                  "Treatment: ", percent_t, "%")
    input_ordered$label[1:num] <- NA
    input_ordered$y <- c(seq(num+0.15, 1.15, by = -1), seq(num-0.15, 0.85, by = -1))
    title = paste0("Top ", num, " AEs of Mean Risk Difference")
    p <- ggplot(input_ordered, aes(x = Diff, y = y, shape = Type)) + 
      geom_point() +
      geom_errorbarh(aes(xmin = Lower, xmax = Upper), na.rm = TRUE) +
      geom_text(aes(label=label, x=0, y=y-0.2), size = 3, na.rm = TRUE) +
      labs(title = title, subtitle = "with 95% Confidence") +
      xlab("Risk Difference") +
      #ylab("PT") +
      theme(axis.title.y = element_blank()) +
      theme(axis.title.x = element_text(size=13),
            axis.text.y = element_text(color = as.factor(input_ordered$SOC[1:num]), 
                                       size = 13, angle = 60)) +
      scale_y_continuous(breaks = seq(num, 1, by = -1), labels = input_ordered$PT[1:num]) +
      theme(legend.text = element_text(size=13), legend.title = element_blank(), 
            legend.position = "bottom", 
            legend.background = element_rect()) +
      xlim(-1*abs(min(input_ordered$Lower[1:num])), max(input_ordered$Upper[1:num]+0.05))
    return(list(p, input_ordered0))
  }
  else if (param == "odds ratio") {
    # order odds ratio by mean and only take top few
    input_ordered0 <- head(input[order(input[, "OR_median"], decreasing = TRUE),], num)
    input_ordered <- rbind(input_ordered0, input_ordered0)
    input_ordered$OR <- c(input_ordered$OR_median[1:num], 
                            input_ordered$OR_raw[(num+1):(2*num)])
    input_ordered$Lower <- c(input_ordered$OR_lower[1:num], rep(NA, 10))
    input_ordered$Upper <- c(input_ordered$OR_upper[1:num], rep(NA, 10))
    input_ordered$Type <- c(rep("Model", num), rep("Raw", num))
    percent_c <- round(input_ordered$AEc*100/input_ordered$Nc, digits = 2)
    percent_t <- round(input_ordered$AEt*100/input_ordered$Nt, digits = 2)
    input_ordered$label <- paste0("Control: ", percent_c, "% vs. ", 
                                  "Treatment: ", percent_t, "%")
    input_ordered$label[1:num] <- NA
    input_ordered$y <- c(seq(num+0.15, 1.15, by = -1), seq(num-0.15, 0.85, by = -1))
    title = paste0("Top ", num, " AEs of Odds Ratio Median")
    p <- ggplot(input_ordered, aes(x = OR, y = y, shape = Type)) + 
      geom_point() +
      geom_errorbarh(aes(xmin = Lower, xmax = Upper), na.rm = TRUE) +
      geom_text(aes(label=label, x=0, y=y-0.2), size = 3, na.rm = TRUE) +
      labs(title = title, subtitle = "with 95% Confidence") +
      xlab("OR") +
      #ylab("PT") +
      theme(axis.title.y = element_blank()) +
      theme(axis.title.x = element_text(size=13),
            axis.text.y = element_text(color = as.factor(input_ordered$SOC[1:num]), 
                                       size = 13, angle = 60)) +
      scale_y_continuous(breaks = seq(num, 1, by = -1), labels = input_ordered$PT[1:num]) +
      theme(legend.text = element_text(size=13), legend.title = element_blank(), 
            legend.position = "bottom", 
            legend.background = element_rect()) +
      xlim(-1*abs(min(input_ordered$Lower[1:num]))-1, max(input_ordered$Upper[1:num]+1))
    return(list(p, input_ordered0))
  }
}


# #-------------------------------------------------------------------------------
# library(readr)
# AE <- read_csv("AE.csv")
# data<-preprocess(AE)
# #-------------------------------------------------------------------------------
# post<-model(data=data, n_burn=1000, n_iter=1000, thin=20, n_adapt=1000, n_chain=2)
# o <- overview(data, post)
# m <- merge_data_model(data, o)
# plot <- topAE_table_plot(m, param = "risk difference")[[1]]
# table <- topAE_table_plot(m, param = "risk difference")[[2]]
# plot <- topAE_table_plot(m, param = "odds ratio")
# #-------------------------------------------------------------------------------

################################################################################
# M2b: Poisson model with mixture prior-----------------------------------------
# not ready
model2 <-
  function(data,
           n_burn,
           n_iter,
           thin,
           n_adapt,
           n_chain,
           alpha.gamma = 3,
           beta.gamma = 1,
           alpha.theta = 3,
           beta.theta = 1,
           mu.gamma.0.0 = 0,
           tau.gamma.0.0 = 0.1,
           alpha.gamma.0.0 = 3,
           beta.gamma.0.0 = 1,
           lambda.alpha = 0.1,
           lambda.beta = 0.1,
           mu.theta.0.0 = 0,
           tau.theta.0.0 = 0.1,
           alpha.theta.0.0 = 3,
           beta.theta.0.0 = 1) {
    model.binom <- "model{
  for (i in 1:Nae) {
  X[i]  ̃ dpois(c[b[i], j[i]])
  Y[i]  ̃ dpois(t[b[i], j[i]])
  
  c[b[i], j[i]] <- lambda_c[b[i], j[i]] * dur0[i]
  t[b[i], j[i]] <- lambda_t[b[i], j[i]] * dur1[i]
  
  log(lambda_c[b[i], j[i]]) <- gamma[b[i], j[i]]
  log(lambda_t[b[i], j[i]]) <- gamma[b[i], j[i]] + theta[b[i], j[i]]
  
  tgamma[i] ̃ dnorm(mu.gamma[b[i]], tau.gamma[b[i]])
  gamma[b[i], j[i]] ~ tgamma[i]
  
  p0[i] ~ dbern(pi[b[i]] ) # prob of point mass
  z[i] <- 1 - p0[i]
  ttheta[i] ~ dnorm(mu.theta[b[i]], tau.theta[b[i]])
  theta1[b[i], j[i]] ~ ttheta[i]
  
  theta[b[i], j[i]] <- (1- p0[i]) * theta1[b[i], j[i]]
  
  D[i] <- X[i]*log(c[b[i], j[i]]) + Y[i]*log(t[b[i], j[i]]) + c[b[i], j[i]] - t[b[i], j[i]]
  
  RR[b[i],j[i]] <- exp(theta[b[i],j[i]] )
  }
  Dbar <- -2* sum(D[]) # -2logL without normalizing constant
  # SOC level parameters
  for(k in 1:B){
  pi[k] ~ dbeta(alpha.pi, beta.pi)
  mu.gamma[k] ~ dnorm(mu.gamma.0, tau.gamma.0)
  tau.gamma[k] ~ dgamma(alpha.gamma,beta.gamma)
  mu.theta[k] ~ dnorm(mu.theta.0, tau.theta.0)
  tau.theta[k] ~ dgamma(alpha.theta,beta.theta)
  }
  # hyperpriors for gammas;
  mu.gamma.0 ~ dnorm(mu.gamma.0.0, tau.gamma.0.0)
  tau.gamma.0 ~ dgamma(alpha.gamma.0.0, beta.gamma.0.0)
  # hyperpriors for thetas;
  mu.theta.0 ~ dnorm(mu.theta.0.0, tau.theta.0.0)
  tau.theta.0 ~ dgamma(alpha.theta.0.0,beta.theta.0.0)
  # hyperpriors for pi?s;
  alpha.pi ~ dexp(lambda.alpha)I(1,)
  beta.pi ~ dexp(lambda.beta)I(1,)
}"
    
    
    param <- c("OR", "Diff", "gamma", "theta")
    
    data <-
      list(
        Nae = nrow(data),
        Nc = data$Nc[1],
        Nt = data$Nt[1],
        B = max(data$b),
        b = data$b,
        j = data$j,
        Y = data$AEt,
        X = data$AEc,
        alpha.gamma = alpha.gamma,
        beta.gamma = beta.gamma,
        alpha.theta = alpha.theta,
        beta.theta = beta.theta,
        mu.gamma.0.0 = mu.gamma.0.0,
        tau.gamma.0.0 = tau.gamma.0.0,
        alpha.gamma.0.0 = alpha.gamma.0.0,
        beta.gamma.0.0 = beta.gamma.0.0,
        lambda.alpha = lambda.alpha,
        lambda.beta = lambda.beta,
        mu.theta.0.0 = mu.theta.0.0,
        tau.theta.0.0 = tau.theta.0.0,
        alpha.theta.0.0 = alpha.theta.0.0,
        beta.theta.0.0 = beta.theta.0.0
      )
    
    
    # # we use parallel computing for n_chain>1
    # if (n_chain>1){
    #   #setup parallel backend to use multiple processors
    #   library(foreach)
    #   library(doParallel)
    #   cores<-detectCores()
    #   cl<-makeCluster(cores[1]-1)
    #   registerDoParallel(cl)
    #
    #   param.est<-list()
    #   param.est<-foreach(m=1:n_chain) %dopar% {
    #     library(mcmcplots)
    #     library(rjags)
    #     library(R2jags)
    #     temp.fit <- jags.model(textConnection(model.binom), data=data, n.chains=1, n.adapt=n_adapt, quiet=TRUE)
    #     update(temp.fit, n.iter=n_burn)
    #
    #     # summary of posterior samples
    #     temp.param.samples <- coda.samples(temp.fit,param,n.iter=n_iter,thin=thin)
    #     temp.param.est <- data.frame(as.matrix(temp.param.samples))
    #     param.est[[m]]<-temp.param.est
    #   }
    #   stopCluster(cl)
    #
    #   ## combine the result from seperate chains together
    #   Final.est<-param.est[[1]]
    #   for (m in 2:n_chain){
    #     Final.est<-rbind(Final.est, param.est[[m]])
    #   }
    # }
    
    #else {
    temp.fit <-
      jags.model(
        textConnection(model.binom),
        data = data,
        n.chains = n_chain,
        n.adapt = n_adapt,
        quiet = TRUE
      )
    update(temp.fit, n.iter = n_burn)
    
    # summary of posterior samples
    temp.param.samples <-
      coda.samples(temp.fit, param, n.iter = n_iter, thin = thin)
    temp.param.est <- data.frame(as.matrix(temp.param.samples))
    Final.est <- temp.param.est
    #}
    
    return(Final.est)
  }



  