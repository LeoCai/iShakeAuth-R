#测试ICA
library(fastICA)
?fastICA


DEBUG = F

source("./readData.R")
source("./train.R")
source("./levelCrossing.R")
source("./extractRandomness.R")
source("./reconcilation.R")
source("./performance.R")
library(entropy)
library(zoo)


exp = 14
file_alice = paste("./datas/5_30/h1/aa",exp,"hh1.csv",sep = "")
file_bob = paste("./datas/5_30/h2/aa",exp,"hh2.csv",sep = "")
data_alice = readData(file_alice)
data_bob = readData(file_bob)
# data_alice = data_alice[250:500,];data_bob = data_bob[245:495,]
# matplot(cbind(data_alice[250:750,1],data_bob[250:750,1])[1:500,],type="l")
data_timealigned = cross_correlation(data_alice,data_bob)
data_alice = data_timealigned$alice; data_bob = data_timealigned$bob
da = as.matrix(data_alice[,1:3]);db = as.matrix(data_bob[,1:3])
a <- fastICA(da, 3,  method = "C", row.norm = FALSE, maxit = 200, 
             tol = 0.0001, verbose = TRUE)
b <- fastICA(db, 3,  method = "C", row.norm = FALSE, maxit = 200, 
             tol = 0.0001, verbose = TRUE)
par(mfrow = c(1, 3))
plot(a$X, main = "Pre-processed data")
plot(a$X %*% a$K, main = "PCA components")
plot(a$S, main = "ICA components")
plot(a$X[,3],type ="l")
plot(a$S[,3],type ="l")

par(mfrow = c(1, 3))
matplot(cbind(a$S[,1],b$S[,1]),type = "l")
matplot(cbind(a$S[,2],b$S[,2]),type = "l")
matplot(cbind(a$S[,3],b$S[,3]),type = "l")
cor(a$S[,1],b$S[,1])
cor(a$S[,2],b$S[,2])
cor(a$S[,3],b$S[,3])
cor(a$X[,1],b$X[,1])
cor(a$X[,2],b$X[,2])
cor(a$X[,3],b$X[,3])
#---------------------------------------------------
#Example 1: un-mixing two mixed independent uniforms
#---------------------------------------------------

S <- matrix(runif(10000), 5000, 2)
A <- matrix(c(1, 1, -1, 3), 2, 2, byrow = TRUE)
X <- S %*% A

a <- fastICA(X, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1, 
             method = "C", row.norm = FALSE, maxit = 200, 
             tol = 0.0001, verbose = TRUE)

par(mfrow = c(1, 3))
plot(a$X, main = "Pre-processed data")
plot(a$X %*% a$K, main = "PCA components")
plot(a$S, main = "ICA components")

#--------------------------------------------
#Example 2: un-mixing two independent signals
#--------------------------------------------

S <- cbind(sin((1:1000)/20), rep((((1:200)-100)/100), 5))
A <- matrix(c(0.291, 0.6557, -0.5439, 0.5572), 2, 2)
X <- S %*% A

a <- fastICA(X, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1, 
             method = "R", row.norm = FALSE, maxit = 200, 
             tol = 0.0001, verbose = TRUE)

par(mfcol = c(2, 3))
plot(1:1000, S[,1 ], type = "l", main = "Original Signals", 
     xlab = "", ylab = "")
plot(1:1000, S[,2 ], type = "l", xlab = "", ylab = "")
plot(1:1000, X[,1 ], type = "l", main = "Mixed Signals", 
     xlab = "", ylab = "")
plot(1:1000, X[,2 ], type = "l", xlab = "", ylab = "")
plot(1:1000, a$S[,1 ], type = "l", main = "ICA source estimates", 
     xlab = "", ylab = "")
plot(1:1000, a$S[, 2], type = "l", xlab = "", ylab = "")

#-----------------------------------------------------------
#Example 3: using FastICA to perform projection pursuit on a
#           mixture of bivariate normal distributions
#-----------------------------------------------------------

if(require(MASS)){
  x <- mvrnorm(n = 1000, mu = c(0, 0), Sigma = matrix(c(10, 3, 3, 1), 2, 2))
  x1 <- mvrnorm(n = 1000, mu = c(-1, 2), Sigma = matrix(c(10, 3, 3, 1), 2, 2))
  X <- rbind(x, x1)
  
  a <- fastICA(X, 2, alg.typ = "deflation", fun = "logcosh", alpha = 1,
               method = "R", row.norm = FALSE, maxit = 200, 
               tol = 0.0001, verbose = TRUE)
  
  par(mfrow = c(1, 3))
  plot(a$X, main = "Pre-processed data")
  plot(a$X %*% a$K, main = "PCA components")
  plot(a$S, main = "ICA components")
}
