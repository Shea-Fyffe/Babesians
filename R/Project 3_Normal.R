# Project 3 Bayesian Estimation with Normal Data

library("ggsci")

bayes_normal <- function(data_mean, data_sd, prior_mean, prior_sd){
  
  # Calculate posterior parameters
  n = length(data_mean)
  post_mean = ((prior_mean/prior_sd^2) + ((n * data_mean)/data_sd^2))/((1/prior_sd^2) + (n/data_sd^2))
  post_sd   = sqrt(1/((1/prior_sd^2) + (n/data_sd^2)))
  
  # Generate prior, data, and posterior plots
  set.seed(100)
  y       = seq(post_mean - 8 * post_sd,
                post_mean + 8 * post_sd,
                length.out = 500)
  y_prior = dnorm(y, prior_mean, prior_sd)
  y_lik   = dnorm(y, data_mean,  data_sd)
  y_post  = dnorm(y, post_mean,  post_sd)
  
  y_max = max(c(y_prior, y_lik, y_post))
  
  Title = "Prior-to-Posterior Transformation with Normal Data"
  
  plot(y, y_prior, type = "l", col = "skyblue",lty = 2, 
       xlim = c(min(y), max(y)), ylim = c(0, y_max),
       ylab = "density", main=Title, lwd = 2)
  lines(y, y_lik,  type = "l", col = "darkorange", lwd = 2)
  lines(y, y_post, type = "l", col = "darkorchid1", lwd = 2)
  
  legend("topright", col = c("skyblue", "darkorange", "darkorchid1"),lty = c(2, 1, 1), 
         cex = 1.5, lwd = 2, bty = "n",
         legend = c("Prior", "Likelihood", "Posterior"))
  
}

bayes_normal(110, 20, 100, 15)
