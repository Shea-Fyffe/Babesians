# Project 5                                                                                                  

library(dplyr)
library(tidyr)
library(ggplot2)

# Import Steph Curry Data
prosteph<-read.csv("prosteph.csv")
usteph<-read.csv("collegesteph.csv")

pptcollege<-data.frame(usteph$Season,usteph$X3P,usteph$X3PA,usteph$X3P.)
colnames(pptcollege)[1] <- "Season"
colnames(pptcollege)[2] <- "Makes"
colnames(pptcollege)[3] <- "Attempts"
colnames(pptcollege)[4] <- "Percentage"
pptcollege

pptpro<-data.frame(prosteph$Season,prosteph$X3P,prosteph$X3PA,prosteph$X3P.)
colnames(pptpro)[1] <- "Season"
colnames(pptpro)[2] <- "Makes"
colnames(pptpro)[3] <- "Attempts"
colnames(pptpro)[4] <- "Percentage"
pptpro

# Find Prior Proportion
PS = sum(usteph$X3P[1:3])
PF = sum(usteph$X3PA[1:3]) - PS

y = seq(.001, .999, .001)

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


# Update 11 times, get posterior vector
for (num in 1:10){
  
  i = i + 1
  
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


# Basic Plot
plot(y, type="n", ylim=c(0,max(Mpos)), xlim=c(0, max(y)), ylab="Density")
colors = heat.colors(10)
for(i in 1:10){
  t <- Mpos[, i]
  lines(y, t, col=colors[11-i])
  
}

# Add prior line in blue
lines(y, prior, col= "dark blue", lwd =2)

# Add more info
title("Steph Curry's Predicted Performance Level")
legend("topright", col = c("dark blue", "red"),lty = c(1, 1), 
       cex = 1, lwd = 1, bty = "n",
       legend = c("End of College", "Current Pro"))



# Convert matrices to dataframes
prior_df = as_data_frame(prior)
names(prior_df) = "V0"
y_df = as_data_frame(y)
names(y_df) = "y"
likelihood_df = as_data_frame(Ml)
posterior_df = as_data_frame(Mpos)


dfBeta = cbind(y, prior, posterior_df)
names(dfBeta)[-c(1, 2)] <- as.character(2009:2018)
dfBeta = gather(dfBeta, Seasons, Density, -y)
dfBeta= dfBeta[dfBeta$Density > 0.001, ]

library(bayestestR)
# Return final posterior for HDI
hdiposterior <- rbinom(y,2897,6694-2897)
hdiposterior<dbinom(y,)
hdiposterior

2897/(3207+590+2897)

?pbinom

# Compute HDI
ci_hdi <- ci(hdiposterior, method = "HDI")
ci_hdi

#ggplot
p
