# Bayesian Project 2
library(plyr)

# Read in prior data
train.dat <- read.csv(file ="file:///C:/Users/gxesp/Documents/R/bayesian statistics/train_LZdllcl.csv", header = T, stringsAsFactors = T)
train.dat$promotion <- train.dat$KPIs_met..80

# Read in likelihood data
test.dat <- read.csv(file ="file:///C:/Users/gxesp/Documents/R/bayesian statistics/test_2umaH9m.csv", header = T, stringsAsFactors = T)
test.dat$promotion <- test.dat$KPIs_met..80

# Contingency Table
con1 <- table(train.dat$education, train.dat$promotion)
con1 <- con1 / sum(con1)
con2 <- table(test.dat$education, test.dat$promotion)
con2 <- con2 / sum(con2)

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

# Prior probability of getting promoted
probyes <- con1[3, 2]

#likelihood ratio
likelihood <- con2[2, 2]/ con2[2, 3]

# Posterior
post <- likelihood * probyes
print(post)

