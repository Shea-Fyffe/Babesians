# Project 4 Bayesian Estimation with Binomial Data

# PS = prior success, PF = prior failure for beta dist. 
# Default uniform distribution.
# k = number of observed successes in the data, n = total trials

bayes_beta <- function(PS = 1, PF = 1, k = 0, n = 0){
  
  # Generate prior, likelihood, and posterior distributions
  x = seq(.001, .999, .001) 
  
  y1 = dbeta(x, PS, PF)
  y2 = dbeta(x, 1 + k, 1 + n - k)
  y3 = dbeta(x, PS + k, PF + n - k)
  
  # Create Plots
  y.max = 1.25 * max(y1,y2,y3,1.6)
  
  Title = paste("Beta (", PS, ",", PF, ") to Beta (", PS + k, ",", PF + n - k, ")")
  
  plot(x, y1, xlim=c(0,1), ylim=c(0, y.max), type = "l", ylab= "Density", lty = 2,
       xlab= "Probability of success", las=1, main= Title,lwd=3,
       cex.lab=1.5, cex.main=1.5, col = "skyblue", axes=FALSE)
  
  axis(1, at = seq(0,1,.2)) #adds custom x axis
  axis(2, las=1) # custom y axis
  
  lines(x, y2, type = "l", col = "darkorange", lwd = 2, lty = 3)
  lines(x, y3, type = "l", col = "darkorchid1", lwd = 4)
  legend("topright", c("Prior", "Posterior", "Likelihood"), col = c("skyblue", "darkorchid1", "darkorange"), 
         lty = c(2,1,3), lwd = c(3,5,2), bty = "n", y.intersp = .55, x.intersp = .1, seg.len=.7)
  
  
}

bayes_beta(0.5, 05, 30, 100)
