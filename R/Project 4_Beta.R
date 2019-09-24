# Project 4 Bayesian Estimation with Binomial Data
# PS = prior success, PF = prior failure for beta dist. 
# Default uniform distribution.
# k = number of observed successes in the data, n = total trials

bayes_beta <- function(PS = 1, PF = 1, k = 0, n = 0){
  
  # Generate prior, likelihood, and posterior distributions
  x = seq(.001, .999, .001) 
  
  prior = dbeta(x, PS, PF)
  likelihood = dbeta(x, 1 + k, 1 + n - k)
  posterior = dbeta(x, PS + k, PF + n - k)
  
  # Standardize
  prior<-prior/max(prior)
  likelihood<-likelihood/max(likelihood)
  posterior<-posterior/max(posterior)
  
  # Create Plots
  ylim<-c(0,max(prior))
  if(n>0){ylim<-c(0,max(c(prior,likelihood,posterior)))}
  
  Title = paste("Beta (",PS, ",",PF, ") to Beta (",PS + k,",",PF + n - k,")")
  
  plot(x, prior, xlim=c(0,1), ylim=ylim, type = "l", ylab= "Density", lty = 2,
       xlab= "Probability of success", las=1, main= Title,lwd=3,
       cex.lab=1.5, cex.main=1.5, col = "skyblue", axes=FALSE)
  
  axis(1, at = seq(0,1,.2)) #adds custom x axis
  axis(2, las=1) # custom y axis
  
  lines(x, likelihood, type = "l", col = "darkorange", lwd = 2, lty = 3)
  lines(x, posterior, type = "l", col = "darkorchid1", lwd = 3)
  legend("topright", c("Prior", "Posterior", "Likelihood"), col = c("skyblue", "darkorchid1", "darkorange"), 
         lty = c(2,1,3), lwd = c(3,5,2), bty = "n", y.intersp = .55, x.intersp = .1, seg.len=.7)
  
  
}

# Trump popular vote in 2016: 46.4%
# Current Trump popularity poll from 538 Data: 43.2%
bayes_beta(232, 268, 432, 1000)
