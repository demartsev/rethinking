library(rethinking)
#1. Construct a linear regression of weight as predicted by height, using the
#adults (age 18 or greater) from the Howell1 data set.

data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ] #get only ages > 18
Hbar <- mean(d2$height)  #get mean height
dat <- list(W=d2$weight,H=d2$height,Hbar=Hbar) #arrange the weight and height data in a list

#construct the model of weights and height
m1 <- quap(
  alist(
    W ~ dnorm( mu , sigma ) ,
    mu <- a + b*( H - Hbar ) , #the linear regression formula - expectation
    a ~ dnorm( 60 , 10 ) ,     #intercept under normal distribution, 
    b ~ dlnorm( 0 , 1 ) ,      #slope under log normal distribution
    sigma ~ dunif( 0 , 10 )    #standard deviation under uniform distribution
  ) , data=dat )

#The heights listed below
#were recorded in the !Kung census, but weights were not recorded for these
#individuals. Provide predicted weights and 89% compatibility intervals for
#each of these individuals. 

#prediction of weights of 4 additional heights
dat2 <- list( H=c(140,160,175) , Hbar=Hbar )
#dat3 <- list( H=c(140,160,175))
h_sim <- sim( m1 , data=dat2 ) #simulate dat2 according to m1 model
Ew <- apply(h_sim,2,mean)      #calculate means for the simulated weights
h_ci <- apply(h_sim,2,PI,prob=0.89)  #add 89% confidence

#construct the table of heights weights and CIs for the aditional individuals
datr <- cbind( H=c(140,160,175) , Ew , L89=h_ci[1,] , U89=h_ci[2,] )
round(datr,1)

# plot sample
col2 <- col.alpha(2,0.8)
plot( d2$height , d2$weight , col=col2 , lwd=3 ,
      cex=1.2 , xlab="height (cm)" , ylab="weight (kg)" )
# expectation with 99% compatibility interval
xseq <- seq(from=130,to=190,len=50)
mu <- link(m1,data=list(H=xseq,Hbar=mean(d2$height)))
lines( xseq , apply(mu,2,mean) , lwd=4 )
shade( apply(mu,2,PI,prob=0.99) , xseq ,
       col=col.alpha(2,0.5) )
# 89% prediction interval
W_sim <- sim(m1,data=list(H=xseq,Hbar=mean(d2$height)))
shade( apply(W_sim,2,PI,prob=0.89) , xseq ,
       col=col.alpha(1,0.3) )



#2. From the Howell1 dataset, consider only the people younger than 13 years old. 
#Estimate the causal association between age and weight. Assume that
#age influences weight through two paths. First, age influences height, and
#height influences weight. Second, age directly influences weight through age related changes in muscle 
#growth and body proportions. 
#All of this implies this causal model (DAG): 
#Use a linear regression to estimate the total (not just direct) causal effect of
#each year of growth on weight. Be sure to carefully consider the priors. Try
#using prior predictive simulation to assess what they imply

d3 <- d[d$age < 13 , ]
Abar <- mean(d3$age)
dat3 <- list(W=d3$weight,H=d3$age,  Hbar=Abar)


# sim from priors ????
n <- 10
a <- rnorm(n,5,1) #setting the intercept to the min weight and matching the SD so all the curves are positive
b <- rlnorm(n,0,1)
# blank(bty="n")
plot( NULL , xlim=range(d3$age) , ylim=range(d3$weight) )
for ( i in 1:n ) abline( a[i] , b[i] , lwd=3 , col=2 )


#construct the model of weights and height
m3 <- quap(
  alist(
    W ~ dnorm( mu , sigma ) ,
    mu <- a + b*( H - Hbar ) , #the linear regression formula - expectation
    a ~ dnorm( 5 , 1 ) ,     #intercept under normal distribution, 
    b ~ dlnorm( 0 , 1 ) ,      #slope under log normal distribution
    sigma ~ dunif( 0 , 10 )    #standard deviation under uniform distribution
  ) , data=dat3 )



# plot sample
col3 <- col.alpha(2,0.8)
plot( d3$age , d3$weight , col=col3 , lwd=3 ,
      cex=1.2 , ylab="weight (kg)" , xlab="age (years)" )
# expectation with 99% compatibility interval
xseq <- seq(from=0,to=15,len=50)
mu <- link(m3,data=list(H=xseq,Hbar=mean(d3$age)))
lines( xseq , apply(mu,2,mean) , lwd=4 )
shade( apply(mu,2,PI,prob=0.99) , xseq ,
       col=col.alpha(2,0.5) )
# 89% prediction interval
W_sim <- sim(m3,data=list(H=xseq,Hbar=mean(d3$age)))
shade( apply(W_sim,2,PI,prob=0.89) , xseq ,
       col=col.alpha(1,0.3) )

### 3. Now suppose the causal association between age and weight might be different for 
### boys and girls. Use a single linear regression, with a categorical
### variable for sex, to estimate the total causal effect of age on weight separately
### for boys and girls. 

d4 <- d[d$age < 13 , ]
Abar <- mean(d4$age)
dat4 <- list(W=d3$weight, H=d3$age,  Hbar=Abar, S = d4$male + 1 ) # S=1 female, S=2 male)


#construct the model of weights and height
m4 <- quap(
  alist(
    W ~ dnorm( mu , sigma ) ,
    mu <- a[S] + b[S]*( H - Abar ) , #the linear regression formula - expectation
    a[S] ~ dnorm( 5 , 1 ) ,     #intercept under normal distribution, 
    b[S] ~ dlnorm( 0 , 1 ) ,      #slope under log normal distribution
    sigma ~ dunif( 0 , 10 )    #standard deviation under uniform distribution
  ) , data=dat4 )




plot( d4$age , d4$weight , lwd=3, col=ifelse(d4$male==1,4,2) , xlab="age (years)" , ylab="weight (kg)" )

# plot sample
col3 <- col.alpha(2,0.8)
plot( d4$age , d4$weight , lwd=3, col=ifelse(d4$male==1,4,2) , xlab="age (years)" , ylab="weight (kg)" )
# expectation with 99% compatibility interval
xseq <- seq(from=0,to=15,len=50)

#males
mu_m <- link(m4,data=list(H=xseq,S=rep(2,13), Hbar=mean(d4[which(d4$male == 1), "age"])))
lines( xseq , apply(mu_m,2,mean) , lwd=4 )
shade( apply(mu_m,2,PI,prob=0.99) , xseq ,
       col=col.alpha(2,0.5) )

# 89% prediction interval
Wm_sim <- sim(m4,data=list(H=xseq,S=rep(2,13), Hbar=mean(d4[which(d4$male == 1), "age"])))
shade( apply(Wm_sim,2,PI,prob=0.89) , xseq ,
       col=col.alpha(2,0.3) )

#females
mu_f <- link(m4,data=list(H=xseq, S=rep(1,13), Hbar=mean(d3[which(d3$male == 0), "age"])))
lines( xseq , apply(mu_f,2,mean) , lwd=4 )
shade( apply(mu_f,2,PI,prob=0.99) , xseq ,
       col=col.alpha(4,0.5) )
# 89% prediction interval
Wf_sim <- sim(m4,data=list(H=xseq,S=rep(1,13), Hbar=mean(d4[which(d4$male == 0), "age"])))
shade( apply(Wf_sim,2,PI,prob=0.89) , xseq ,
       col=col.alpha(4,0.3) )


#How do girls and boys differ? Provide one or more posterior contrasts as a summary
# posterior mean W
post <- extract.samples(m4)
dens( post$a[,1] , xlim=c(5,30) , lwd=3 ,
      col=2 , xlab="posterior mean weight (kg)" )
dens( post$a[,2] , lwd=3 , col=4 , add=TRUE )

# posterior W distributions
W1 <- rnorm( 1000 , post$a[,1] , post$sigma )
W2 <- rnorm( 1000 , post$a[,2] , post$sigma )
dens( W1 , xlim=c(0,30) , ylim=c(0,0.2) ,
      lwd=3 , col=2 )
dens( W2 , lwd=3 , col=4 , add=TRUE )

# causal contrast (in means)
mu_contrast <- post$a[,2] - post$a[,1]
dens( mu_contrast , xlim=c(-1,3) , lwd=3 ,
      col=1 , xlab="posterior mean weight contrast
(kg)" )

# contrast at each age
Aseq <- 0:12
mu1 <- sim(m4,data=list(H=Aseq,S=rep(1,13)))
mu2 <- sim(m4,data=list(H=Aseq,S=rep(2,13)))
mu_contrast <- mu1
for ( i in 1:13 ) mu_contrast[,i] <- mu2[,i] - mu1[,i]
plot( NULL , xlim=c(0,13) , ylim=c(-15,15) , xlab="age" , ylab="weight difference (boys-girls)" )

for ( p in c(0.5,0.67,0.89,0.99) )
  shade( apply(mu_contrast,2,PI,prob=p) , Aseq )

abline(h=0,lty=2,lwd=2)

for ( i in 1:13 ) points( mu_contrast[1:1000,i] , col=ifelse(mu_contrast[1:1000,i]>0,4,2) , lwd=3 )