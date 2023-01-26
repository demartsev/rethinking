library(rethinking)
data(foxes)
library(ggplot2)

ggplot(data = foxes, aes(x = area, y = avgfood)) + geom_smooth() + geom_point()

 

dat <- list(
  Fo=standardize(foxes$avgfood),
  A=standardize(foxes$area),
  G=standardize(foxes$groupsize),
  W=standardize(foxes$weight)
)



fox.model <- quap(
  alist(
    Fo ~ dnorm(mu, sigma), #model
    mu <- a + bA*A, #model
    a ~ dnorm(0,0.5), #priors for the intercept, when area is 0, food is probably 0 also
    bA ~ dnorm(0,1), #priors for the slope, assume no relationship slope of 0
    sigma ~ dexp(1) #priors for standard deviation of Food
  ), data=dat)

ggplot(data = foxes, aes(x = area, y = avgfood)) + geom_smooth() + geom_point()
precis(fox.model)

#total effect of food on weight
fox.W.model <- quap(
  alist(
    W ~ dnorm(mu, sigma), #model
    mu <- a + bF*Fo, #model
    a ~ dnorm(0,0.5), #priors for the intercept, when area is 0, food is probably 0 also
    bF ~ dnorm(0,1), #priors for the slope, assume no relationship slope of 0
    sigma ~ dexp(1) #priors for standard deviation of Food
  ), data=dat)

ggplot(data = foxes, aes(y = weight, x = avgfood)) + geom_smooth() + geom_point()
precis(fox.W.model)

#direct effect of food on weight
fox.W1.model <- quap(
  alist(
    W ~ dnorm(mu, sigma), #model
    mu <- a + bF*Fo +bG*G, #model
    a ~ dnorm(0,0.2), #priors for the intercept, when area is 0, food is probably 0 also
    bF ~ dnorm(0,0.5),
    bG ~ dnorm(0,0.5) , #priors for the slope, assume no relationship slope of 0
    sigma ~ dexp(1) #priors for standard deviation of Food
  ), data=dat)

ggplot(data = foxes, aes(y = weight, x = avgfood, color = as.factor(groupsize))) + geom_point() + geom_smooth(method="lm")



precis(fox.W1.model)



