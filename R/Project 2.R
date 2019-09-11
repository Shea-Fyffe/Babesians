# Bayesian Project 2
library(plyr)
library(tidyverse)

# Read in prior data
train.dat <- read.csv(file ="file:///C:/Users/gxesp/Documents/R/bayesian statistics/train_LZdllcl.csv", header = T, stringsAsFactors = T)
train.dat$promotion <- train.dat$KPIs_met..80

# Read in likelihood data
test.dat <- read.csv(file ="file:///C:/Users/gxesp/Documents/R/bayesian statistics/test_2umaH9m.csv", header = T, stringsAsFactors = T)
test.dat$promotion <- test.dat$KPIs_met..80

# Dataframe filtered on Sales
filter.dat <- test.dat %>% filter(department == 'Sales & Marketing')

# Contingency Table
con1 <- table(train.dat$education, train.dat$promotion)
con2 <- table(test.dat$education, test.dat$promotion)
con3 <- table(filter.dat$education, filter.dat$promotion)

# Function Edit Contingency Table
ftable <- function(con){
  con <- cbind(con, rowSums(con))
  con <- rbind(con, colSums(con))
  colnames(con)[3] = 'Education'
  rownames(con)[3] = 'Promotion'
  
  con
}

# Edit Cons and display new
con1 <- ftable(con1)
con2 <- ftable(con2)
con3 <- ftable(con3)

print(con1)
print(con2)
print(con3)

# Bayes function for frequency counts
bayes <- function(prior_h1, con) {
  lh <- (con[2, 2]/con[3, 2]) / (((con[2, 2]/con[2, 3]) * prior_h1) + ((con[1, 2]/con[1, 3]) * (1 - prior_h1)))
  lh * prior_h1
}

# Initial prior probability from training data
prior_promotion <- con1[3, 2] / con1[3, 3]
print(prior_promotion)

# Update based on testing data
bayes(prior_promotion, con2)

# Update based on filtered sales data
bayes(prior_promotion, con3)

