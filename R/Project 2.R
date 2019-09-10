# Bayesian Project 2
library(plyr)
library(tidyverse)

# Read in prior data
train.dat <- read.csv(file ="file:///C:/Users/gxesp/Documents/R/bayesian statistics/train_LZdllcl.csv", header = T, stringsAsFactors = T)
train.dat$promotion <- train.dat$KPIs_met..80

# Read in likelihood data
test.dat <- read.csv(file ="file:///C:/Users/gxesp/Documents/R/bayesian statistics/test_2umaH9m.csv", header = T, stringsAsFactors = T)
test.dat$promotion <- test.dat$KPIs_met..80

# Data frame filtered on Sales
filter.dat <- test.dat %>% filter(department == 'Sales & Marketing')

# Contingency Table
con1 <- table(train.dat$education, train.dat$promotion)
con2 <- table(test.dat$education, test.dat$promotion)
con3 <- table(filter.dat$education, filter.dat$promotion)

# Probability Table
con1 <- cbind(con1, rowSums(con1))
con1 <- rbind(con1, colSums(con1))
colnames(con1)[3] = 'Education'
rownames(con1)[3] = 'Promotion'

print(con1)


con2 <- cbind(con2, rowSums(con2))
con2 <- rbind(con2, colSums(con2))
colnames(con2)[3] = 'Education'
rownames(con2)[3] = 'Promotion'

print(con2)


con3 <- cbind(con3, rowSums(con3))
con3 <- rbind(con3, colSums(con3))
colnames(con3)[3] = 'Education'
rownames(con3)[3] = 'Promotion'

print(con3)

# Bayes function for frequency counts
bayes <- function(prior_h1, prior_h2, con) {
  lh <- (con[2, 2]/con[3, 2]) / (((con[2, 2]/con[3, 2]) * prior_h1) + ((con[2, 1]/con[3, 1]) * prior_h2))
  lh * prior_h1
}


# Initial prior probability from training data
prior_promotion <- con1[3, 2] / con1[3, 3]
prior_nopromotion <- con1[3, 1] / con1[3, 3]

# Update based on testing data
bayes(prior_promotion, prior_nopromotion, con2)

# Update based on filtered sales data
bayes(prior_promotion, prior_nopromotion, con3)

