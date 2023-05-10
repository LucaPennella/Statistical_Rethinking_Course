#Cap 4 of Statistical Rethiking

#load data
library(rethinking)
data(Howell1)
d <- Howell1 #d stay for dataframe

#eda 
str( d ) #Inspect the structure of the data frame, the same way you can inspect the structure of any symbol in R
precis( d ) #We can also use rethinking’s precis summary function

d$height #symbol $ as extract, as in extract the column named height from the data frame d

# All we want for now are heights of adults in the sample. 
# The reason to filter out non- adults for now is that height is strongly correlated with age, before adulthood.

d2 <- d[ d$age >= 18 , ] #does is give you all of the rows in which d$age is greater-than- or-equal-to 18
dens(d2$height)
summary(d2)

# hi ∼ Normal(μ, σ) [likelihood]
# μ ∼ Normal(178, 20) [μ prior]
# σ ∼ Uniform(0, 50) [σ prior]

# Why 178 cm? Your author is 178 cm tall. And the range from 138 cm to 218 cm encompasses a huge range of plausible mean
# heights for human populations. So domain-specific information has gone into this prior. 
# Everyone knows something about human height and can set a reasonable and vague prior of this kind. 
# But in many regression problems, as you’ll see later, using prior information is more subtle, 
# because parameters don’t always have such clear physical meaning.

curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )
curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )

# A standard deviation like σ must be positive, so bounding it at zero makes sense.
# How should we pick the upper bound ?
# A standard deviation of 50cm would imply that 95% of individual heights lie within 100cm of the average height.
# That’s a very large range.

#prior predictive
sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )

#Grid approximation of the posterior distribution
mu.list <- seq( from=150, to=160 , length.out=100 )
sigma.list <- seq( from=7 , to=9 , length.out=100 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum(
  dnorm( d2$height , post$mu[i] , post$sigma[i] , log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )
post$prob

#The functions contour_xyz and image_xyz are both in the rethinking package.
contour_xyz( post$mu , post$sigma , post$prob )
image_xyz( post$mu , post$sigma , post$prob )

#Sampling from the posterior
sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                      prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]
plot( sample.mu , sample.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.1) )

#to characterize the shapes of the marginal posterior densities of μ and σ
dens( sample.mu )
dens( sample.sigma )

PI( sample.mu )
PI( sample.sigma )

#Sample size and the normality of σ’s posterior. 
d3 <- sample( d2$height , size=20 )

mu.list <- seq( from=150, to=170 , length.out=200 )
sigma.list <- seq( from=4 , to=20 , length.out=200 )
post2 <- expand.grid( mu=mu.list , sigma=sigma.list )
post2$LL <- sapply( 1:nrow(post2) , function(i)
  sum( dnorm( d3 , mean=post2$mu[i] , sd=post2$sigma[i] ,
              log=TRUE ) ) )
post2$prod <- post2$LL + dnorm( post2$mu , 178 , 20 , TRUE ) +
  dunif( post2$sigma , 0 , 50 , TRUE )
post2$prob <- exp( post2$prod - max(post2$prod) )
sample2.rows <- sample( 1:nrow(post2) , size=1e4 , replace=TRUE ,
                        prob=post2$prob )
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]
plot( sample2.mu , sample2.sigma , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="mu" , ylab="sigma" , pch=16 )

dens( sample2.sigma , norm.comp=TRUE )

#quadratic approximation

# height ~ dnorm(mu,sigma)
# mu ~ dnorm(156,10)
# sigma ~ dunif(0,50)

flist <- alist(
  height ~ dnorm( mu , sigma ) ,
  mu ~ dnorm( 178 , 20 ) ,
  sigma ~ dunif( 0 , 50 )
)

m4.1 <- quap( flist , data=d2 )
precis( m4.1 )

#narrow prior

m4.2 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu ~ dnorm( 178 , 0.1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d2 )
precis( m4.2 )

#matrix of variances and covariances
vcov( m4.1 )

#get samples from this multi-dimensional posterior
post <- extract.samples( m4.1 , n=1e4 )
head(post)

precis(post)


#Linear prediction
plot( d2$height ~ d2$weight )

#different prior for prediction
set.seed(2971)

N <- 100
a <- rnorm( N , 178 , 20 )
b <- rnorm( N , 0 , 10 )

plot( NULL , xlim=range(d2$weight) , ylim=c(-100,400) ,
      xlab="weight" , ylab="height" )
abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( "b ~ dnorm(0,10)" )
xbar <- mean(d2$weight)
for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
                        from=min(d2$weight) , to=max(d2$weight) , add=TRUE ,
                        col=col.alpha("black",0.2) )
# try log-Normal
b <- rlnorm( 1e4 , 0 , 1 )
dens( b , xlim=c(0,5) , adj=0.1 )

set.seed(2971)
N <- 100                   # 100 lines
a <- rnorm( N , 178 , 20 )
b <- rlnorm( N , 0 , 1 )

plot( NULL , xlim=range(d2$weight) , ylim=c(-100,400) ,
      xlab="weight" , ylab="height" )
abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( "b ~ dnorm(0,10)" )
xbar <- mean(d2$weight)
for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
                        from=min(d2$weight) , to=max(d2$weight) , add=TRUE ,
                        col=col.alpha("black",0.2) )

# Finding the posterior distribution

# hi ∼ Normal(μi, σ)   height ~ dnorm(mu,sigma)
# μi =α+β(xi − ̄x).     mu<-a+b*weight
# α ∼ Normal(178, 20)  a ~ dnorm(178,20)
# β ∼ Log-Normal(0, 1) b ~ dlnorm(0,1)
# σ ∼ Uniform(0, 50).  sigma ~ dunif(0,50)


# load data again, since it's a long way back
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]
# define the average weight, x-bar
xbar <- mean(d2$weight)
# fit model
m4.3 <- quap(
  alist( height ~ dnorm( mu , sigma ) ,
         mu <- a + b*( weight - xbar ) ,
         a ~ dnorm( 178 , 20 ) ,
         b ~ dlnorm( 0 , 1 ) ,
         sigma ~ dunif( 0 , 50 ) ),
  data=d2 )

precis( m4.3 )
round

plot( height ~ weight , data=d2 , col=rangi2 )
post <- extract.samples( m4.3 )
a_map <- mean(post$a)
b_map <- mean(post$b)
curve( a_map + b_map*(x - xbar) , add=TRUE )

post <- extract.samples( m4.3 )
post[1:5,]

#experiments

N <- 10
dN <- d2[ 1:N , ]
mN <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*( weight - mean(weight) ) ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=dN )

# extract 20 samples from the posterior
post <- extract.samples( mN , n=20 )
# display raw data and sample size
plot( dN$weight , dN$height ,
      xlim=range(d2$weight) , ylim=range(d2$height) ,
      col=rangi2 , xlab="weight" , ylab="height" )
mtext(concat("N = ",N))
# plot the lines, with transparency
for ( i in 1:20 )
  curve( post$a[i] + post$b[i]*(x-mean(dN$weight)) ,
         col=col.alpha("black",0.3) , add=TRUE )


post <- extract.samples( m4.3 )
mu_at_50 <- post$a + post$b * ( 50 - xbar )
dens( mu_at_50 , col=rangi2 , lwd=2 , xlab="mu|weight=50" )
PI( mu_at_50 , prob=0.89 )

mu <- link( m4.3 )
str(mu)

# define sequence of weights to compute predictions for
# these values will be on the horizontal axis
weight.seq <- seq( from=25 , to=70 , by=1 )
# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link( m4.3 , data=data.frame(weight=weight.seq) )
str(mu)

# use type="n" to hide raw data
plot( height ~ weight , d2 , type="n" )
# loop over samples and plot each mu value
for ( i in 1:100 )
  points( weight.seq , mu[i,] , pch=16 , col=col.alpha(rangi2,0.1) )

# summarize the distribution of mu
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )

# plot raw data
# fading out points to make line and interval more visible
plot( height ~ weight , data=d2 , col=col.alpha(rangi2,0.5) )
# plot the MAP line, aka the mean mu for each weight
lines( weight.seq , mu.mean )
# plot a shaded region for 89% PI
shade( mu.PI , weight.seq )

#Prediction intervals
sim.height <- sim( m4.3 , data=list(weight=weight.seq) )
str(sim.height)

height.PI <- apply( sim.height , 2 , PI , prob=0.89 )


# plot raw data
plot( height ~ weight , d2 , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( weight.seq , mu.mean )
# draw PI region for simulated heights
shade( mu.PI , weight.seq )
shade( height.PI , weight.seq )

# 89% prediction interval for height, as a function of weight. 
# The solid line is the average line for the mean height at each weight. T
# he two shaded regions show different 89% plausible regions. 
# The narrow shaded interval around the line is the distribution of μ. 



