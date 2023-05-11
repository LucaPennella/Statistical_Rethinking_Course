# Chapter 5 (Categorical variables)

data(Howell1)
d <- Howell1
str(d)

# hi ∼ Normal(μi, σ) 
# μi = α + βm * mi
# α ∼ Normal(178, 20) 
# βm ∼ Normal(0, 10)
# σ ∼ Uniform(0, 50)

# when mi =1,the linearmodel is μi =α+βm. 
# And when mi = 0, the linear model is simply μi = α.

# priors
mu_female <- rnorm(1e4,178,20)
mu_male <- rnorm(1e4,178,20) + rnorm(1e4,0,10)
precis( data.frame( mu_female , mu_male ) )

#We aren’t actually more unsure about male height than female height, a priori. Is there another way?

## index variable ##

#An index variable contains integers that correspond to different categories.
d$sex <- ifelse( d$male==1 , 2 , 1 )
str( d$sex )

# hi ∼ Normal(μi, σ)
# μi = αsex[i]
# αj ∼ Normal(178, 20) , for j = 1..2
# σ ∼ Uniform(0, 50)

m5.8 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a[sex] ,
    a[sex] ~ dnorm( 178 , 20 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d )

precis( m5.8 , depth=2 )

post <- extract.samples(m5.8)
post
post$diff_fm <- post$a[,1] - post$a[,2]
precis( post , depth=2 )

## many categories ##
data(milk)
d <- milk
unique(d$clade)

d$clade_id <- as.integer( d$clade ) # generate an index
# Ki ∼ Normal(μi, σ)
# μi = αclade[i]
# αj ∼ Normal(0, 0.5) , for j = 1..4
# σ ∼ Exponential(1)

d$K <- scale( d$kcal.per.g )

m5.9 <- quap(
  alist(
    K ~ dnorm( mu , sigma ),
    mu <- a[clade_id],
    a[clade_id] ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp( 1 )
  ) , data=d )


labels <- paste( "a[" , 1:4 , "]:" , levels(d$clade) , sep="" )
plot( precis( m5.9 , depth=2 , pars="a" ) , labels=labels ,
      xlab="expected kcal (std)" )

set.seed(63)
d$house <- sample( rep(1:4,each=8) , size=nrow(d) )

m5.10 <- quap(
  alist(
    K ~ dnorm( mu , sigma ),
    mu <- a[clade_id] + h[house],
    a[clade_id] ~ dnorm( 0 , 0.5 ),
    h[house] ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp( 1 )
  ) , data=d )

precis(m5.1, depth = 4)
