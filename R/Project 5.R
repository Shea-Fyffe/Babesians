# Project 5                                                                                                  
# Import Steph Curry Data
prosteph = read.csv(file ="file:///C:/Users/gxesp/Documents/R/bayesian statistics/prosteph.csv", header = T)
usteph = read.csv(file ="file:///C:/Users/gxesp/Documents/R/bayesian statistics/collegesteph.csv", header = T)

# Find Prior Proportion
PS = cumsum(usteph$X3P[1:3])
PS = sum(PS)
PF = cumsum(usteph$X3PA[1:3] - cumsum(usteph$X3P[1:3]))
PF = sum(PF)

# Give a prior graph
prior = dbeta(y, PS, PF)
prior = prior/sum(prior)

# Get a vector of Season Data
k = cumsum(prosteph$X3P[1:10])
n = cumsum(prosteph$X3PA[1:10])


# Creat null Matrices
Ml = matrix(data = 0, nrow = 999, ncol = 10)
Mpos = matrix(data = 0, nrow = 999, ncol = 10)
i = 0

y = seq(.001, .999, .001)

# Update 11 times, get posterior vector
for (num in 1:10){
  
  i = i + 1
  # Set prior: 
  PS = 820
  PF = 623
  
  # Generate likelihood, and posterior distributions
  likelihood = dbeta(y, 1 + k[i], 1 + n[i] - k[i])
  posterior = dbeta(y, PS + k[i], PF + n[i] - k[i])
  
  # Standardize
  likelihood = likelihood/sum(likelihood)
  posterior = posterior/sum(posterior)
  
  #Assign null vectors
  Ml[, i] = likelihood
  Mpos[, i] = posterior
  
  
}

# Convert matrices to dataframes (might need later?)
likelihood_df = as_data_frame(Ml)
posterior_df = as_data_frame(Mpos)

# Basic Plot
plot(y, type="n", ylim=c(0,max(posterior_df)), xlim=c(0, max(y)), ylab="Density")
colors = heat.colors(10)
for(i in 1:10){
  t <- Mpos[, i]
  lines(y, t, col=colors[11-i])
  
}
# add prior line in blue
lines(y, prior, col= "dark blue", lwd =2)

# Add more info
title("Steph Curry's Performance Throughout His Career")
legend("topright", col = c("dark blue", "red"),lty = c(1, 1), 
       cex = 1, lwd = 1, bty = "n",
       legend = c("End of College", "Current Pro"))
