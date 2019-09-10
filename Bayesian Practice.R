### Bayesian practice
### how many half court shots made in 100 attempts?
### Prior info 1: proportion made has mean=.15, SD=.02 from 1000 data points
### New Data 1: Made 15/100
proportion_made<-rnorm(1000,.15,.02)
n_made<-rbinom(1000,100,proportion_made)
prior<-data.frame(proportion_made,n_made)
posterior<-prior[prior$n_made==15,]
hist(posterior$proportion_made)
### Prior info 1: proportion made has mean=.06, SD=.01 from 1000 data points
### New Data 1: Made 6/100
proportion_made2<-rnorm(1000,.06, .005)
n_made2<-rbinom(1000,100,proportion_made2)
prior2<-data.frame(proportion_made2,n_made2)
posterior2<-prior2[prior2$n_made2==6,]
hist(posterior2$proportion_made2)
(sum(prior2$n_made2==6))/length(prior2$n_made2)

###From DataCamp
n_draws <- 100000
n_ads_shown <- 100
proportion_clicks <- runif(n_draws, min = 0.0, max = 0.2)
n_visitors <- rbinom(n = n_draws, size = n_ads_shown, 
                     prob = proportion_clicks)
prior <- data.frame(proportion_clicks, n_visitors)

# Create the posteriors for video and text ads
posterior_video <- prior[prior$n_visitors == 13, ]
posterior_text <- prior[prior$n_visitors == 6,]

# Visualize the posteriors
hist(posterior_video$proportion_clicks, xlim = c(0, 0.25))
hist(posterior_text$proportion_clicks, xlim=c(0,0.25))

##Getting more complicated
# The IQ of a bunch of zombies
iq <- c(55, 44, 34, 18, 51, 40, 40, 49, 48, 46)
# Defining the parameter grid
mu<-seq(0,150,length=100)
print(mu)
pars <- expand.grid(mu = seq(0, 150, length.out = 100), 
                    sigma = seq(0.1, 50, length.out = 100))
# Defining and calculating the prior density for each parameter combination
pars$mu_prior <- dnorm(pars$mu, mean = 100, sd = 100)
pars$sigma_prior <- dunif(pars$sigma, min = 0.1, max = 50)
pars$prior <- pars$mu_prior * pars$sigma_prior
# Calculating the likelihood for each parameter combination
for(i in 1:nrow(pars)) {
  likelihoods <- dnorm(iq, pars$mu[i], pars$sigma[i])
  pars$likelihood[i] <- prod(likelihoods)
}
# Calculate the probability of each parameter combination
pars$probability <- pars$likelihood*pars$prior
pars$probability<- pars$probability/sum(pars$probability)
