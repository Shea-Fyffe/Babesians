# Project 4 Bayesian Estimation with Binomial Data
# PS = prior success, PF = prior failure for beta dist. 
# Default uniform distribution.
# k = number of observed successes in the data, n = total trials


library(tidyr)
library(ggplot2)

bayes_beta <- function(PS = 1, PF = 1, k = 0, n = 0, title = "Beta Distributions"){
  
  # Generate prior, likelihood, and posterior distributions
  y = seq(.001, .999, .001) 
  
  prior = dbeta(y, PS, PF)
  likelihood = dbinom(rep(k, length(y)), n, y)
  posterior = dbeta(y, PS + k, PF + n - k)
  
  # Standardize
  prior = prior/sum(prior)
  likelihood = likelihood/sum(likelihood)
  posterior = posterior/sum(posterior)

  # Use ggplot to graph data
  dfBeta = data.frame(y, prior, likelihood, posterior)
  dfBeta = gather(dfBeta, Distributions, Density, -y)
  
  dfBeta= dfBeta[dfBeta$Density > 0.001, ]
    
  p = ggplot(dfBeta, aes(y, Density, color = Distributions)) + geom_line() + labs(title = title)
  p
  
}

# Trump popular vote in 2016: 46.4%
# Current Trump popularity poll from 538 Data: 43.2%
bayes_beta(232, 268, 864, 2000, title = "Will Trump win the popular vote in 2020?")
