# Project 4 mBayesian Estimation with Binomial Data
# PS = prior success, PF = prior failure for beta dist. 
# Default uniform distribution.                                                                                                  

library(tidyr)
library(ggplot2)

prosteph<-read.csv("prosteph.csv")
usteph<-read.csv("collegesteph.csv")

bayes_multiple <- function(cs, cf, rps, rpa, ps, pa, title = "Beta Distributions"){
  
  # cs=college makes, cf=college misses, rps=rookie makes, rpa=rookie attempts pm=pro makes,pa=pro attempts
  # Generate prior, likelihood, and posterior distributions
  y = seq(.001, .999, .001)
  
  prior = dbeta(y, cs, cf)
  likelihood = dbinom(rep(rps, length(y)), rpa, y)
  likelihood2= dbinom(rep(ps, length(y)), pa, y)
  rookieposterior = dbeta(y, cs + rps, cf + rpa - rps)
  totalposterior = dbeta(y, cs + rps + ps, cf + rpa - rps + pa - ps)
  
  # Standardize
  prior = prior/sum(prior)
  likelihood = likelihood/sum(likelihood)
  likelihood2=likelihood2/sum(likelihood2)
  rookieposterior = rookieposterior/sum(rookieposterior)
  totalposterior = totalposterior/sum(totalposterior)
  
  # Use ggplot to graph data
  dfBeta = data.frame(y, prior, likelihood, likelihood2, rookieposterior, totalposterior)
  dfBeta = gather(dfBeta, Distributions, Density, -y)
  
  dfBeta= dfBeta[dfBeta$Density > 0.001, ]
  
  p = ggplot(dfBeta, aes(y, Density, color = Distributions)) + geom_line() + labs(title = title)
  p
  
}

bayes_multiple(414,590,166,380,2483,5690,title="College vs. Pro")

