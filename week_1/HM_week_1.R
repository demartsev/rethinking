#setting the scale of X for proportion of water
p_grid <- seq(from = 0, to = 1, length.out = 1000)

#prior probability for proportion f water
prob_p <- rep(1, 1000)

#the number of ways to get 6 water out of 9 samples given probability for each value of x

 #worked example
prob_data <- dbinom(6, size = 9, prob = p_grid)

# absolute  posterior probability for each value of x = probability of data * the prior probability
posterior <- prob_data*prob_p

#relative posterior plausibility for each value of x
posterior <- posterior/sum(posterior)

#plot posterior distribution
plot(posterior)

##




##1.Suppose the globe tossing data (Chapter 2) had turned out to be 4 water
#and 11 land. Construct the posterior distribution, using grid approximation.
#Use the same flat prior as in the book.
#setting the scale of X for proportion of water
p_grid <- seq(from = 0, to = 1, length.out = 1000)

#prior probability for proportion f water
prob_p <- rep(1, 1000)

#the number of ways to get 4 water out of 15 samples given probability for each value of x
prob_data <- dbinom(4, size = 15, prob = p_grid)

# absolute  posterior probability for each value of x = probability of data * the prior probability
posterior <- prob_data*prob_p

#relative posterior plausibility for each value of x
posterior <- posterior/sum(posterior)

#plot posterior distribution
plot(posterior)

##2. Now suppose the data are 4 water and 2 land. Compute the posterior
##again, but this time use a prior that is zero below p = 0.5 and a constant
##above p = 0.5. This corresponds to prior information that a majority of the
##Earth’s surface is water.

#setting the scale of X for proportion of water
p_grid <- seq(from = 0, to = 1, length.out = 1000)

#prior probability for proportion f water
prob_p <- c(rep(0, 500), rep(1, 500))

#the number of ways to get 4 water out of 15 samples given probability for each value of x
prob_data <- dbinom(4, size = 6, prob = p_grid)

# absolute  posterior probability for each value of x = probability of data * the prior probability
posterior <- prob_data*prob_p

#relative posterior plausibility for each value of x
posterior <- posterior/sum(posterior)

#plot posterior distribution
plot(posterior)



##3. For the posterior distribution from 2, compute 89% percentile and HPDI
##intervals. Compare the widths of these intervals. Which is wider? Why? If
##you had only the information in the interval, what might you misunderstand
##about the shape of the posterior distribution?

library(rethinking)
HPDI(posterior, 0.89)
PI(posterior,  0.89)


samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
plot( samples , ylim=c(0,1) , xlab="samples" , ylab="proportion water" )
HPDI(samples, 0.89)
PI(samples,  0.89)

##4. OPTIONAL CHALLENGE. Suppose there is bias in sampling so that Land
##is more likely than Water to be recorded. Specifically, assume that 1-in-5
##(20%) of Water samples are accidentally recorded instead as ”Land”. First,
##write a generative simulation of this sampling process. Assuming the true
##proportion of Water is 0.70, what proportion does your simulation tend to
##produce instead? Second, using a simulated sample of 20 tosses, compute
##the unbiased posterior distribution of the true proportion of water.

# Pr(W|W) = 0.8
# Pr(W|L) = 0.2
# Pr(W) = 0.7*0.8


# sim as double sampling process
N <- 1e5
trueW <- rbinom(N,size=20,prob=0.7)
obsW <- rbinom(N,size=trueW,prob=0.8)

# or as single sample
W <- rbinom(N,size=20,prob=0.7*0.8)

mean(obsW/20)
mean(W/20)

# now analyze
# Pr(p|W,N) = Pr(W|p,N)Pr(p) / Z
# Pr(W|N,p) = Pr(W)Pr(W|W)

W <- rbinom(1,size=20,prob=0.7*0.8)
grid_p <- seq(from=0,to=1,len=100)
pr_p <- dbeta(grid_p,1,1)
prW <- dbinom(W,20,grid_p*0.8)
post <- prW*pr_p

post_bad <- dbinom(W,20,grid_p)

plot(grid_p,post,type="l",lwd=4,xlab="proportion water",ylab="plausibility")
lines(grid_p,post_bad,col=2,lwd=4)