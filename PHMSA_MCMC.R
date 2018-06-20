### Model code for the two sample Poisson test ###
require(rjags)
require(dplyr)
require(BEST)

####injury MCMC####
# Setting up the data
byyear <- read.csv("byyear.csv")  #read in the data
byy <- byyear %>% group_by(BA) %>%
  summarise(inj=sum(Injuries),fat=sum(Fatalities), ser=sum(Serious),
            sig=sum(Significant), count=length(Injuries))

x <- c(byy[[2,2]], byy[[1,2]]) #injury count
t <- c(byy[[2,6]], byy[[1,6]]) #time count

rm(samples_inj) #remove old samples before running

# The model string written in the JAGS language
model_string <- "model {
  for(group_i in 1:2) {
    x[group_i] ~ dpois(lambda[group_i] * t[group_i])
    #lambda[group_i] ~ dgamma(0.5, 0.00001)
    lambda[group_i] ~ dgamma(2, 0.1)
    x_pred[group_i] ~ dpois(lambda[group_i] * t[group_i])
  }
  rate_diff <- lambda[2] - lambda[1]
  rate_ratio <- lambda[2] / lambda[1]
}"

# Running the Injury model
model <- jags.model(textConnection(model_string), data = list(x = x, t = t), n.chains = 3)
samples_inj <- coda.samples(model, c("lambda", "x_pred", "rate_diff", "rate_ratio"), n.iter=1e4)#injury posterior samples

# Inspecting the posterior for injuries
#summary(samples_inj)  
injMCMC <- as_tibble(as.matrix(samples_inj))
#injMCMC$metric <- "Injuries" #add a metric column and populate it.


####Fatalitiy MCMC####

# Setting up the data
#byyear <- read.csv("byyear.csv")
#byy <- byyear %>% group_by(BA) %>%summarise(inj=sum(Injuries),fat=sum(Fatalities), ser=sum(Serious),sig=sum(Significant))

x <- c(byy[[2,3]], byy[[1,3]]) #fatality count

rm(samples_fat) #remove the old samples before running

model <- jags.model(textConnection(model_string), data = list(x = x, t = t), n.chains = 3)
samples_fat <- coda.samples(model, c("lambda", "x_pred", "rate_diff", "rate_ratio"), n.iter=1e4)
#summary(samples_fat)
fatMCMC <- as_tibble(as.matrix(samples_fat))
#fatMCMC$metric <- "Fatalities"


###Significant MCMC ####

# Setting up the data
#byyear <- read.csv("byyear.csv")
#byy <- byyear %>% group_by(BA) %>%summarise(inj=sum(Injuries),fat=sum(Fatalities), ser=sum(Serious),sig=sum(Significant))
x <- c(byy[[2,5]], byy[[1,5]]) 

rm(samples_sig)

model <- jags.model(textConnection(model_string), data = list(x = x, t = t), n.chains = 3)
samples_sig <- coda.samples(model, c("lambda", "x_pred", "rate_diff", "rate_ratio"), n.iter=1e4)
#summary(samples_sig)
sigMCMC <- as_tibble(as.matrix(samples_sig))
#sigMCMC$metric <- "Significant"

###Serious MCMC ####

# Setting up the serious incident data
#byyear <- read.csv("byyear.csv")
#byy <- byyear %>% group_by(BA) %>%summarise(inj=sum(Injuries),fat=sum(Fatalities), ser=sum(Serious),sig=sum(Significant))
x <- c(byy[[2,4]], byy[[1,4]]) 

rm(samples_ser) #remove previous samples

model <- jags.model(textConnection(model_string), data = list(x = x, t = t), n.chains = 3)
samples_ser <- coda.samples(model, c("lambda", "x_pred", "rate_diff", "rate_ratio"), n.iter=1e4)#posterior serious samples
#summary(samples_ser)
serMCMC <- as_tibble(as.matrix(samples_ser)) #convert to a tibble
#serMCMC$metric <- "Serious"

#allMCMC <- bind_rows(serMCMC, sigMCMC, injMCMC, fatMCMC) #bind all the samples into one df

# ggplot(allMCMC, aes(rate_ratio))+geom_histogram(aes(fill=metric), col='black')+theme_bw(14, "serif")+facet_wrap(~metric,scales = "free_x")+labs(y=NULL)

##plotting all four together ####
par(mfrow=c(2,2))
plotPost(injMCMC$rate_ratio, compVal = 1, main="Ratio of Mean Rates of Injuries",xlab=bquote(lambda[After]/lambda[Before]))
plotPost(fatMCMC$rate_ratio, compVal = 1, main="Ratio of Mean Rates of Fatalities",xlab=bquote(lambda[After]/lambda[Before]))
plotPost(sigMCMC$rate_ratio, compVal = 1, main="Ratio of Mean Rates of Significant Incidents",xlab=bquote(lambda[After]/lambda[Before]))
plotPost(serMCMC$rate_ratio, compVal = 1, main="Ratio of Mean Rates of Serious Incidents",xlab=bquote(lambda[After]/lambda[Before]))
