###Functions

##Function to Create Matrix
fmatrix<-function(con){
  con<-cbind(con, rowSums(con))
  con<-rbind(con, colSums(con))
  colnames(con)[3]='Education'
  rownames(con)[3]='Promotion'
  
  con
}

##Bayes function for frequency counts
bayes <- function(prior_h1, con) {
  lh <- (con[2, 2]/con[3, 2]) / (((con[2, 2]/con[3, 2]) * prior_h1) 
                                 + ((con[2, 1]/con[3, 1]) * (1 - prior_h1)))
  lh * prior_h1
}

###Bayesian Project 2
prior<-read.csv("train.csv")
prior$promotion<-prior$KPIs_met..80.
evidence1<-read.csv("test.csv")
evidence1$promotion<-evidence1$KPIs_met..80.
evidence2<-subset(evidence1, department=="Sales & Marketing")

###Prior Data set
#Contingency Table
priorcon<-table(prior$education, prior$promotion)
priorcon<-priorcon/sum(priorcon)
priorcon
#Probability Matrix
priormat<-fmatrix(priorcon)
priormat

#chance of anyone getting promotion
prior_all<-priormat[3,2]
#Conditional, given Master's and Above
prior_grad<-priormat[2,2]/priormat[2,3]

###Evidence Data Sets

##Education - Master's & above

#Contingency Table
evicon1<-table(evidence1$education, evidence1$promotion)
evicon1<-evicon1/sum(evicon1)
evicon1

#Probability Matrix
evimat1<-fmatrix(evicon1)
evimat1

#chance of anyone getting promotion
evi_all<-evimat1[3,2]

#Conditional, given Master's and Above
evi_grad<-evimat1[2,2]/evimat1[2,3]

##Department - Sales & Marketing

#Contingency Table
evicon2<-table(evidence2$education, evidence2$promotion)
evicon2<-evicon2/sum(evicon2)
evicon2

#Probability Matrix
evimat2<-fmatrix(evicon2)
evimat2

#Conditional, given Sales & Marketing Dept.
evi_sm<-evimat2[3,2]
evi_sm

###Posteriors (Updating Priors)
##For Anyone
bayes(prior_all, evimat)

##For Master's and Above
bayes(prior_grad, evimat)

################End of Working code, on to functions and sources##################

# Initial prior probability from training data
prior_promotion <- priormat[3, 2] / priormat[3, 3]
print(prior_promotion)

# Update based on testing data
bayes(prior_promotion, con2)
# Update based on filtered sales data
bayes(prior_promotion, con3)

##Unconditioned


##Graduate Degree Condition


##Shea's Function to Get Posterior Probabilities
#'
#' @param h Numeric. vector of length 2 P(H)
#' @param d Numeric. vector of length 2 P(D|H)
#' @param H1 Logical. Assume the Null Hypothesis is False?
#'
#' @return
#' @export
#'
#' @examples
bayes_rule <- function(h, d, H1 = TRUE) {
  if (length(h) == 2L) {
    dh <- c(d, 1 - d)
  }
  if (length(revise_cell) != 2L) {
    stop("revise_cell should specify row and column number")
  }
  ct <- h * matrix(dh, ncol = 2)
  if (H1) {
    pos <- ct[, 1] / sum(ct[, 1])
  } else {
    pos <- ct[, 2] / sum(ct[, 2])
  }
  ct <- cbind(h, d, ct, pos)
  colnames(ct) <- c("Prior", "Likelihood", "BNH1", "BNH0", "POS")
  return(ct)
}

#####Practicing From Datacamp Course

####My scenario: number of made halfcourt shots
### Prior info 1: proportion made has mean=.15, SD=.01 from 1000 data points
### New Data 1: Made 15/100

proportion_made<-rnorm(1000,.15,.01)
n_made<-rbinom(1000,100,proportion_made)
prior<-data.frame(proportion_made,n_made)
posterior<-prior[prior$n_made==15,]
hist(posterior$proportion_made)
(sum(prior$n_made==15))/length(prior$n_made)

### Prior info 1: proportion made has mean=.06, SD=.005 from 1000 data points
### New Data 1: Made 10/100
proportion_made2<-rnorm(1000,.06, .005)
n_made2<-rbinom(1000,100,proportion_made2)
prior2<-data.frame(proportion_made2,n_made2)
posterior2<-prior2[prior2$n_made2==10,]
hist(posterior2$proportion_made2)
(sum(prior2$n_made2==10))/length(prior2$n_made2)

###Data and Exact Code From DataCamp
n_draws <- 100000
n_ads_shown <- 100
proportion_clicks <- runif(n_draws, min = 0.0, max = 0.2)
n_visitors <- rbinom(n = n_draws, size = n_ads_shown, 
                     prob = proportion_clicks)
prior <- data.frame(proportion_clicks, n_visitors)

# Create the posteriors for video and text ads
posterior_video <- prior[prior$n_visitors == 13, ]
posterior_text <- prior[prior$n_visitors == 6,]

# Visualize the posteriors
hist(posterior_video$proportion_clicks, xlim = c(0, 0.25))
hist(posterior_text$proportion_clicks, xlim=c(0,0.25))

##Getting more complicated
# The IQ of a bunch of zombies
iq <- c(55, 44, 34, 18, 51, 40, 40, 49, 48, 46)
# Defining the parameter grid
mu<-seq(0,150,length=100)
print(mu)
pars <- expand.grid(mu = seq(0, 150, length.out = 100), 
                    sigma = seq(0.1, 50, length.out = 100))
# Defining and calculating the prior density for each parameter combination
pars$mu_prior <- dnorm(pars$mu, mean = 100, sd = 100)
pars$sigma_prior <- dunif(pars$sigma, min = 0.1, max = 50)
pars$prior <- pars$mu_prior * pars$sigma_prior
# Calculating the likelihood for each parameter combination
for(i in 1:nrow(pars)) {
  likelihoods <- dnorm(iq, pars$mu[i], pars$sigma[i])
  pars$likelihood[i] <- prod(likelihoods)
}
# Calculate the probability of each parameter combination
pars$probability <- pars$likelihood*pars$prior
pars$probability<- pars$probability/sum(pars$probability)