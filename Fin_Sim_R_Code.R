rm(list = ls()) # fresh start


################################################################################

###############################
######### BASICS ##############
###############################

# you can use <- or = to assign objects
# I will use the former
x <- 1:5
x

# r has built-in objects, see e.g.,
letters

# can assign names to objects
names(x) <- letters[1:5] 

# call items using index or label
x[1:2]
x["c"]
x["z"]

# x has no dimension, even though it is a vector
dim(x)

# use length for vectors
length(x)

# load x as a martrix
X <- as.matrix(x)
dim(X) # dimesion, which is equivalent to size in MATLAB

# c() combined vectors and lists
x2 <- c(x,x,x,x,x)
x2

# put in a matrix
X2 <- matrix(x2,length(x))
X2

# R repeats elements
X3 <- matrix(x,length(x),length(x));
identical(X3,X2)

# also  when it comes to operations
x + 1:10
x + 1:9

# can assign labels to rows and columns
rownames(X3) <- letters[1:nrow(X3)]
colnames(X3) <- LETTERS[1:ncol(X3)]
X3.subset <- X3[c("a","c"),c("D","E")]
X3.subset

X3.subset2 <- X3[c(1,3),c(4,5)]
all(X3.subset == X3.subset2)

#### EXERCISE ###
M <- matrix(1:100, 10,byrow = T)
# or
M <- t(matrix(1:100,10))


#######################
#### FUNCTIONS ########
#######################

# need {} 
f1 <- function(x) {
  x1 <- x[1]
  x2 <- x[length(x)]
  return(c(x1,x2)) # functions must return something 
}

f1(x)

# assign function in one line
f2 <- function(x) c(x[1],x[length(x)]) 
f2(x)

# R reads outside the function
f3 <- function(x) mean((x - mean(x)) > a*sd(x))
a <- 0.5
f3(x)
a <- 1
f3(x)

####### EXERCISE #########
M.f <- function(M,a) mean(M > a)
M.f(M,-Inf)
M.f(M,Inf)


############################
######## LOOPS #############
############################
# similar to MATLAB, but need {} instead of the end command
M.mean2 <- numeric() # define an empty numeric object
for(i in 1:ncol(M) ) {
  mean.i <- mean(M[,i])
  M.mean2 <- c(M.mean2, mean.i) 
  }
M.mean2

# same for while loops
i <- 1
M.mean2 <- numeric() # define an empty numeric object
while (i <= ncol(M)) {
  mean.i <- mean(M[,i])
  M.mean2 <- c(M.mean2, mean.i) 
  i <- i + 1
}
M.mean2

############################
####### APPLY INSTEAD ######
############################

# the apply functions can be more efficient
apply(M, 1, mean) # for rows
apply(M, 2, mean) # for columns

# sapply can be applied on a set or a list of items
sapply(1:ncol(M), function(i) mean(M[,i])  )

apply(M, 2, function(x)  mean(((x - mean(x))/sd(x))^2)    ) 

####### EXERCISE #########

# R has built-in functions
ds <- EuStockMarkets

{
t1 <- system.time({
min.max <- numeric()
for(i in 1:ncol(ds)) {
  min.max <-  cbind(min.max, c(min(ds[,i]),max(ds[,i])))
  }
colnames(min.max) <- colnames(ds)
})

t2 <- system.time({
min.max <- apply(ds, 2, function(x)  c(min(x),max(x))  ) 
#min.max <- sapply(1:ncol(ds), function(i) c(min(ds[,i],max(ds[,i])) )      ) 
})

t1/t2

}

# increase the data in size and repeat the above
ds <- matrix(ds,nrow(ds),ncol(ds)*10^4 )


{
  
  t1 <- system.time({
    min.max <- numeric()
    for(i in 1:ncol(ds)) {
      min.max <-  cbind(min.max, c(min(ds[,i]),max(ds[,i])))
    }
    colnames(min.max) <- colnames(ds)
  })
  
  t2 <- system.time({
    min.max <- apply(ds, 2, function(x)  c(min(x),max(x))  ) 
    #min.max <- sapply(1:ncol(ds), function(i) c(min(ds[,i],max(ds[,i])) )      ) 
  })
}

t1/t2

####################################
######### DATA FRAMES ##############
####################################

i <- 1:26
l <- letters
L <- cbind(i,l) # combines columns
sapply(1:ncol(L),function(i) class(L[,i])  ) # convers all columns to characters

# data.frame keeps each
L2 <- data.frame(i,l, stringsAsFactors = F)
sapply(1:ncol(L2),function(i) class(L2[,i])  )

# look at the iris built-in data
sapply(1:ncol(iris),function(i) class(iris[,i])  )
summary(iris,digits = 2)

# use the plyr package,
# if not used before, run: install.packages("plyr")
library(plyr)
ddply(iris,"Species", function(ds)  mean(ds[,"Sepal.Length"]) )

# split the data into three lists based on species
iris.list <- dlply(iris,"Species",data.frame)
length(iris.list)
sapply(iris.list, nrow)


iris[,"Ratio"] <- iris[,"Petal.Length"]/iris[,"Petal.Width"]
ddply(iris,"Species", function(ds)  max(ds[,"Ratio"]) )


###################################3
########### plots ##################

y <- iris[,"Petal.Length"]
x <- iris[,"Petal.Width"]
plot(y~x)
# or
plot(Petal.Length ~ Petal.Width,data = iris)

plot(Petal.Length ~ Petal.Width,data = iris)
iris[,"Petal.Length.hat"] <- fitted(lm(Petal.Length ~ Petal.Width,data = iris))
lines(Petal.Length.hat~Petal.Width, data = iris, col = 2)

# boxplot applies to data
boxplot(iris)

# look at petal length
petal.list <- lapply(iris.list,function(x) x[,"Petal.Length"] )
boxplot(petal.list)

####### EXERCISE #########
lm.species <- function(v) {
  ds.i <- iris.list[[v]]
  plot(Petal.Length ~ Petal.Width,data = ds.i)
  ds.i[,"Petal.Length.hat"] <- fitted(lm(Petal.Length ~ Petal.Width,data = ds.i))
  lines(Petal.Length.hat~Petal.Width, data = ds.i, col = 2)
  }

names(iris.list)
lm.species("versicolor")


###################################################################################################

############################################
############# SIMULATIONS ##################
############################################

# basic sampling
sample(1:10,5)
sample(1:10,5)

sample(1:10,20) # more sample than set
sample(1:10,20,replace = T) # needs replacements

# you can set a seed
sapply(1:5, function(i) { set.seed(13); sample(1:10,5)    }     )

# normal distribution
norm.list <- lapply(1:5, function(i) rnorm(10^3,mean = i, sd = 1)  )
sapply(norm.list, mean)

# boxplot can be applied on list items and data.frames
boxplot(norm.list, col = "gray", pch = 20, cex = 0.5)

####### EXERCISE #########
n <- 10^3
norm.sim.f <- function(n) { 
  X1 <- rnorm(n,mean = 10,sd = 3)
  X2 <- rnorm(n,mean = 15,sd = 5)
  return(mean(X1 < X2))
  }

n.seq <- seq(100,n,by = 10)
p.n <- sapply(n.seq,norm.sim.f)
p <- pnorm(0,-5,sqrt(3^2 + 5^2))

plot(p.n ~ n.seq, type = "l", main = expression(P(X[1] < X[2] )), ylab = "", xlab = "n" )
abline(h = p, lty = 2)


############# MULTIVAIATE ###########3
library(MASS)

# let's go back to EU stock exchanges
ds <- EuStockMarkets
R <- (ds[-1,]/ds[-nrow(ds),] - 1)*100
Mu <- apply(R, 2, mean)
Sigma <- var(R)

n <- 10^3
X <- mvrnorm(n,Mu,Sigma)
dim(X)

kernel_emp <- density(R[,"CAC"])
kernel_sim <- density(X[,"CAC"])
plot(kernel_emp, main  = "CAC Kernel", lwd = 2)
lines(kernel_sim, col = 2, lwd = 2)
legend("topleft", c("Empirical","Simulated"), col = 1:2,lwd = 2)

# compare estimated parameters with original
n <- 10^6
X <- mvrnorm(n,Mu,Sigma)
plot(var(X) ~ Sigma, xlab = expression(Sigma), ylab = "Var(X)")
abline(a = 0, b = 1)
plot(apply(X,2,mean) ~ Mu, xlab = expression(mu), ylab = "E(X)")
abline(a = 0, b = 1)

####### EXERCISE #########
# equal weights portfolio
W <- rep(1/4,4)

SR_f <- function(i) {
  R.250 <-  mvrnorm(250,Mu,Sigma)
  Rp <- R.250%*%W
  SR <- (mean(Rp)/sd(Rp))
  return(SR)
  }

SR <- sapply(1:1000, function(i) SR_f(i)  )*sqrt(250) # scaled it to annual
kernel_SR <- density(SR)
plot(kernel_SR,main =  "Portfolio Sharpe-ratio Kernel")
abline(v = 0, lty = 2)

##########################################
######## Cholesky Decomposition ##########
##########################################
n <- 10^6
A <- chol(Sigma)
M <- matrix(Mu,n,length(Mu),byrow = T)
Z <- sapply( 1:length(Mu),function(i) rnorm(n) )
X_CD <- M + Z%*%A

plot(var(X_CD) ~ Sigma, xlab = expression(Sigma), ylab = "Var(X)")
abline(a = 0, b = 1)
plot(apply(X_CD,2,mean) ~ Mu, xlab = expression(mu), ylab = "E(X)")
abline(a = 0, b = 1)

plot(kernel_cd, lwd = 2)
lines(kernel_mc,col = 2, lty = 3, lwd = 2)


###################################################################################################

############################################
############# OPTIONS ######################
############################################


############################
#### SIMULATING PRICES ####

Time <-1
S0 <- 100
K <- 110
r <- 0.01
Mu <- 0.15
Sigma <- 0.2

# create a function to simulate terminal price
GMB_f <- function(n,S0,Mu,Sigma,Time) {
  EX <- Time*(Mu - (Sigma^2)/2)
  VX <- Time*(Sigma^2)
  r_t <- rnorm(n,EX,sqrt(VX))
  S_t <- S0*exp(r_t)
  list(S_t,r_t)
  }
  
GMB1 <- GMB_f(10^5,100,0.15,0.2,1) 
hist(GMB1[[1]], main = expression(S[t]), col = "gray", freq = F)
hist(GMB1[[2]], main = expression(r[t]), col = "gray",freq = F)



###############################
######## PRICING EU CALL ######
###############################

# a call option for EU
Call_f <- function(n,S0, K, r, Time, Mu, Sigma)  {
  GMB1 <- GMB_f(n,S0,Mu,Sigma,Time)
  S1 <- GMB1[[1]]
  r1 <- GMB1[[2]]

  Call1 <- S1-K
  Call1[Call1<0] <- 0
  Call <- mean(Call1 * exp(-r*Time))
  
  return(Call)
  }
  

n.seq <- seq(100,10^5,by = 1000)
# to compare with BS, we make the assumption that the stock
# grows with r rather than mu
Call_n <- sapply(n.seq, function(n)  Call_f(n,S0, K, r, Time, r, Sigma) )
plot(Call_n, type = "l")

blackscholes <- function(S, X, rf, T, sigma) {
  values <- c(2)
  d1 <- (log(S/X)+(r+sigma^2/2)*T)/(sigma*sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  values[1] <- S*pnorm(d1) - X*exp(-rf*T)*pnorm(d2)
  values[2] <- X*exp(-rf*T) * pnorm(-d2) - S*pnorm(-d1)
  
  values
}

abline(h = blackscholes(S0,K,r,Time,Sigma),lty = 2)


########################################################
############## PRACTICE FOR THREE STOCKS ###############
########################################################
Time <- 1
r <- 0.06
K <- 1
Mu <- rep(r,3)
D <- diag(c(0.2,0.3,0.25)) # diagonal of sigmas
R <- rbind(c(1,0.5,0.25), c(0.5,1,-0.25), c(0.25,-0.25,1))
Sigma <- D%*%R%*%D
S <- rep(100,3)
n <- 10^6

GMB_MV_f <- function(n,S,Mu,Sigma,Time) {
  EX <- Time*(Mu - (diag(D)^2)/2)
  VX <- Time*(Sigma)
  r_t <- mvrnorm(n,EX,VX)
  S_t <- sapply(1:ncol(r_t), function(i) exp(r_t[,i])*S[i])
  list(S_t,r_t)
}

GMB2 <- GMB_MV_f(n,S,Mu,Sigma,Time) 

# look at correlations, this should be consistent with R
cor(GMB2[[2]])

# look at final prices for each stock
S_T <- GMB2[[1]]
Spread <- S_T[,1] - S_T[,2] - K
Spread[Spread < 0] <- 0
mean(Spread)*exp(-r*Time)


boxplot(S_T, col = "gray", pch = 20, cex = 0.5)









