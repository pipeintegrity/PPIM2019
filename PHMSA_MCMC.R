### Model code for the Bayesian First Aid two sample Poisson test ###
require(rjags)
require(dplyr)

####injury MCMC####
# Setting up the data
byyear <- read.csv("byyear.csv")
byy <- byyear %>% group_by(BA) %>%summarise(inj=sum(Injuries),fat=sum(Fatalities), ser=sum(Serious),sig=sum(Significant), count=length(Injuries))
x <- c(byy[2,2], byy[1,2]) 
t <- c(byy[2,6], byy[1,6]) 
rm(samples_inj)

# The model string written in the JAGS language
model_string <- "model {
  for(group_i in 1:2) {
    x[group_i] ~ dpois(lambda[group_i] * t[group_i])
    #lambda[group_i] ~ dgamma(0.5, 0.00001)
    lambda[group_i] ~ dgamma(2, 0.1)
    x_pred[group_i] ~ dpois(lambda[group_i] * t[group_i])
  }
  rate_diff <- lambda[1] - lambda[2]
  rate_ratio <- lambda[1] / lambda[2]
}"

# Running the model
model <- jags.model(textConnection(model_string), data = list(x = x, t = t), n.chains = 3)
samples_inj <- coda.samples(model, c("lambda", "x_pred", "rate_diff", "rate_ratio"), n.iter=5000)

# Inspecting the posterior
summary(samples_inj)  
injMCMC <- as_tibble(as.matrix(samples_inj))

####Fatalitiy MCMC####

# Setting up the data
#byyear <- read.csv("byyear.csv")
#byy <- byyear %>% group_by(BA) %>%summarise(inj=sum(Injuries),fat=sum(Fatalities), ser=sum(Serious),sig=sum(Significant))
x <- c(byy[2,3], byy[1,3]) 
t <- c(byy[2,6], byy[1,6])
rm(samples_sig)

# The model string written in the JAGS language
model_string <- "model {
  for(group_i in 1:2) {
    x[group_i] ~ dpois(lambda[group_i] * t[group_i])
    #lambda[group_i] ~ dgamma(0.5, 0.00001)
    lambda[group_i] ~ dgamma(3, 1)
    x_pred[group_i] ~ dpois(lambda[group_i] * t[group_i])
  }
  rate_diff <- lambda[1] - lambda[2]
  rate_ratio <- lambda[1] / lambda[2]
}"

# Running the model
model <- jags.model(textConnection(model_string), data = list(x = x, t = t), n.chains = 3)
samples_fat <- coda.samples(model, c("lambda", "x_pred", "rate_diff", "rate_ratio"), n.iter=5000)
summary(samples_fat)
fatMCMC <- as_tibble(as.matrix(samples_fat))

###Significant MCMC ####

# Setting up the data
#byyear <- read.csv("byyear.csv")
#byy <- byyear %>% group_by(BA) %>%summarise(inj=sum(Injuries),fat=sum(Fatalities), ser=sum(Serious),sig=sum(Significant))
x <- c(byy[2,5], byy[1,5]) 
t <- c(byy[2,6], byy[1,6])
rm(samples_sig)

# The model string written in the JAGS language
model_string <- "model {
  for(group_i in 1:2) {
    x[group_i] ~ dpois(lambda[group_i] * t[group_i])
    #lambda[group_i] ~ dgamma(0.5, 0.00001)
    lambda[group_i] ~ dgamma(3, 1)
    x_pred[group_i] ~ dpois(lambda[group_i] * t[group_i])
  }
  rate_diff <- lambda[1] - lambda[2]
  rate_ratio <- lambda[1] / lambda[2]
}"

# Running the model
model <- jags.model(textConnection(model_string), data = list(x = x, t = t), n.chains = 3)
samples_sig <- coda.samples(model, c("lambda", "x_pred", "rate_diff", "rate_ratio"), n.iter=5000)
summary(samples_sig)
sigMCMC <- as_tibble(as.matrix(samples_sig))
###Serious MCMC ####

# Setting up the data
#byyear <- read.csv("byyear.csv")
#byy <- byyear %>% group_by(BA) %>%summarise(inj=sum(Injuries),fat=sum(Fatalities), ser=sum(Serious),sig=sum(Significant))
x <- c(byy[2,4], byy[1,4]) 
t <- c(byy[2,6], byy[1,6])
rm(samples_ser)

# The model string written in the JAGS language
model_string <- "model {
  for(group_i in 1:2) {
    x[group_i] ~ dpois(lambda[group_i] * t[group_i])
    #lambda[group_i] ~ dgamma(0.5, 0.00001)
    lambda[group_i] ~ dgamma(3, 1)
    x_pred[group_i] ~ dpois(lambda[group_i] * t[group_i])
  }
  rate_diff <- lambda[1] - lambda[2]
  rate_ratio <- lambda[1] / lambda[2]
}"

# Running the model
model <- jags.model(textConnection(model_string), data = list(x = x, t = t), n.chains = 3)
samples_ser <- coda.samples(model, c("lambda", "x_pred", "rate_diff", "rate_ratio"), n.iter=5000)
summary(samples_ser)

serMCMC <- as_tibble(as.matrix(samples_ser))
