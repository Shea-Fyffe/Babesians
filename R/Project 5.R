# Project 5                                                                                                  

library(tidyr)

# Import Steph Curry Data
prosteph = read.csv(file ="file:///C:/Users/gxesp/Documents/R/bayesian statistics/prosteph.csv", header = T)
usteph = read.csv(file ="file:///C:/Users/gxesp/Documents/R/bayesian statistics/collegesteph.csv", header = T)

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


#ggplot
p = ggplot(dfBeta, aes(y, Density, color = Seasons)) + geom_line() + 
  labs(title = "Steph Curry's Predicted Performance Level")
p


# Credibility Interval
# function for ploting densities
plotting=function(data, ... ,title ) {
  library(ggplot2)
  ggplot(data, ...)+
    geom_bar(stat="identity", alpha=0.5)+
    theme_minimal()+
    theme(axis.text.y = element_blank())+
    ylab("Probability density")+
    ggtitle(title)+
    labs(fill='95% confidence interval')
}

# Compute posterior HDI for latest season
posterior_df$posterior_cumsum=cumsum(posterior_df$V10)
posterior_df$posterior_CI=ifelse(posterior_df$posterior_cumsum<0.025|posterior_df$posterior_cumsum>0.975, "outside CI", "inside CI")
plotting(data=posterior_df, aes(y_df$y, posterior_df$V10, fill=posterior_CI), title="Posterior CI")

# Compute prior HDI
prior_df$prior_cumsum=cumsum(prior_df$V0)
prior_df$prior_CI=ifelse(prior_df$prior_cumsum<0.025|prior_df$prior_cumsum>0.975, "outside CI", "inside CI")
plotting(data=prior_df, aes(y_df$y, prior_df$V0, fill=prior_CI), title="Prior CI")


