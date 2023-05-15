# Chapter 6 

## (multicollinearity) ##

library(rethinking)
N <- 100         # number of individuals
set.seed(909)
height <- rnorm(N,10,2)      # sim total height of each
leg_prop <- runif(N,0.4,0.5) # leg as proportion of height
leg_left <- leg_prop*height + # sim left leg as proportion + error
  rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height + # sim right leg as proportion + error
  rnorm( N , 0 , 0.02 )
d <- data.frame(height,leg_left,leg_right) # combine into data frame


m6.1 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) ,
    sigma ~ dexp( 1 )
  ),
  data=d )

precis(m6.1)
plot(precis(m6.1))

post <- extract.samples(m6.1)
plot( bl ~ br , post , col=col.alpha(rangi2,0.1) , pch=16 )

sum_blbr <- post$bl + post$br
dens( sum_blbr , col=rangi2 , lwd=2 , xlab="sum of bl and br" )

#One way to think of this phenomenon is that you have approximated this model:
#.    yi ∼ Normal(μi, σ)
#.    μi = α + β1xi + β2xi

#The variable y is the outcome, like height in the example, and x is a single predictor, like the leg lengths in the example. 
# Here x is used twice, which is a perfect example of the problem caused by using the almost-identical leg lengths. 
# From the computer’s perspective, this model is simply:
#   yi ∼ Normal(μi, σ) 
#   μi = α + (β1 + β2)xi

m6.2 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    sigma ~ dexp( 1 ) ),
  data=d )

precis(m6.2)

#########

data(milk)
d <- milk
d$K <- scale( d$kcal.per.g )
d$F <- scale( d$perc.fat )
d$L <- scale( d$perc.lactose )


# kcal.per.g regressed on perc.fat
m6.3 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bF*F ,
    a ~ dnorm( 0 , 0.2 ) ,
    bF ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data=d )
# kcal.per.g regressed on perc.lactose
m6.4 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bL*L ,
    a ~ dnorm( 0 , 0.2 ) ,
    bL ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data=d )
precis( m6.3 )
precis( m6.4 )

m6.5 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bF*F + bL*L ,
    a ~ dnorm( 0 , 0.2 ) ,
    bF ~ dnorm( 0 , 0.5 ) ,
    bL ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ),
  data=d )
precis( m6.5 )

pairs( ~ kcal.per.g + perc.fat + perc.lactose , data=d , col=rangi2 )

