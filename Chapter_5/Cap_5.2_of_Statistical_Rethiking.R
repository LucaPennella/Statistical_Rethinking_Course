# Chapter 5

library(rethinking)
data(milk)
d <- milk

str(d)
head(d,10)

# A popular hypothesis has it that primates with larger brains produce more energetic milk,
# so that brains can grow quickly.

# The variables we’ll consider for now are:
#     kcal.per.g : Kilocalories of energy per gram of milk.
#     mass : Average female body mass, in kilograms.
#     neocortex.perc : The percent of total brain mass that is neocortex mass.

#standardize these three variables:
d$K <- scale( d$kcal.per.g )
d$N <- scale( d$neocortex.perc )
d$M <- scale( log(d$mass) )

# The first model is:
#    Ki ∼ Normal(μi, σ)
#    μi =α+βNNi

#use only complete cases
dcc <- d[ complete.cases(d$K,d$N,d$M) , ]


m5.5_draft <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bN * N ,
    a ~ dnorm( 0 , 1 ) ,
    bN ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp( 1 )
  ) , data = dcc )

# Priors study
prior <- extract.prior( m5.5_draft )
xseq <- c(-2,2)
mu <- link( m5.5_draft , post=prior , data=list(N=xseq) )
plot( NULL , xlim=xseq , ylim=xseq )
for ( i in 1:50 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )

m5.5 <- quap(
alist(
  K ~ dnorm( mu , sigma ) ,
  mu <- a + bN*N ,
  a ~ dnorm( 0 , 0.2 ) ,
  bN ~ dnorm( 0 , 0.5 ) ,
  sigma ~ dexp( 1 )
) , data=dcc )

prior <- extract.prior( m5.5 )
xseq <- c(-2,2)
mu <- link( m5.5_draft , post=prior , data=list(N=xseq) )
plot( NULL , xlim=xseq , ylim=xseq )
for ( i in 1:50 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )

#posterior
precis(m5.5)

xseq <- seq( from=min(dcc$N)-0.15 , to=max(dcc$N)+0.15 , length.out=30 )
mu <- link( m5.5 , data=list(N=xseq) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( K ~ N , data=dcc )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

#other models with body mass
m5.6 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data=dcc )

precis(m5.6)

xseq <- seq( from=min(dcc$M)-0.15 , to=max(dcc$M)+0.15 , length.out=30 )
mu <- link( m5.6 , data=list(M=xseq))
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( K ~ M , data=dcc )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

#try a multivariate model
# Ki ∼ Normal(μi, σ)
# μi = α + βNNi + βMMi
# α ∼ Normal(0, 0.2) 
# βn ∼ Normal(0, 0.5) 
# βm ∼ Normal(0, 0.5) 
# σ ∼ Exponential(1)

m5.7 <- quap(
  alist(
    K  ~ dnorm( mu , sigma ) ,
    mu <- a + bN * N + bM * M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bN ~ dnorm( 0 , 0.5 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = dcc
)

precis(m5.7)

# Visually comparing this posterior to those of the previous two models helps:
plot( coeftab( m5.5 , m5.6 , m5.7 ) , pars=c("bM","bN") )

pairs( ~K + M + N , dcc )

xseq <- seq( from=min(dcc$M)-0.15 , to=max(dcc$M)+0.15 , length.out=30 )
mu <- link( m5.7 , data=data.frame( M=xseq , N=0 ) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( NULL , xlim=range(dcc$M) , ylim=range(dcc$K) )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

#

# M -> K <- N
# M -> N
n <- 100
M <- rnorm( n )
N <- rnorm( n , M )
K <- rnorm( n , N - M )
d_sim <- data.frame(K=K,N=N,M=M)


# M -> K <- N
# N -> M
n <- 100
N <- rnorm( n )
M <- rnorm( n , N )
K <- rnorm( n , N - M )
d_sim2 <- data.frame(K=K,N=N,M=M)
# M -> K <- N
# M <- U -> N
n <- 100
U <- rnorm( n )
N <- rnorm( n , U )
M <- rnorm( n , U )
K <- rnorm( n , N - M )
d_sim3 <- data.frame(K=K,N=N,M=M)

dag5.7 <- dagitty( "dag{
    M -> K <- N
    M -> N }" )
coordinates(dag5.7) <- list( x=c(M=0,K=1,N=2) , y=c(M=0.5,K=1,N=0.5) )
MElist <- equivalentDAGs(dag5.7)
MElist
drawdag(MElist)