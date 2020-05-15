#1.1 No vectorization
sim <- function(l) {
         c <- rep(0,l)
 hits <- 0
  pow2 <- function(x) {
             x2 <- sqrt( x[1]*x[1]+x[2]*x[2] )
    return(x2)
     }
  for(i in 1:l){
             x = runif(2,-1,1)
     if( pow2(x) <=1 ){
                  hits <- hits + 1
        }
        dens <- hits/i
        pi_partial = dens*4
           c[i] = pi_partial
         }
   #print(pi_partial)
   return(c)
}

size <- 100000
system.time(
   res <- sim(size)
)


#1.2 Using vectorization
#Source: https://helloacm.com/r-programming-tutorial-how-to-compute-pi-using-monte-carlo-in-r/

simv <- function(l) {
  set.seed(0.234234)
  x=runif(l)
  y=runif(l)
  
  z=sqrt(x^2 + y^2)
  resl <- length(which(z<=1))*4/length(z)
  return(resl)
}

size <- 100000
system.time(
  res <- simv(size)
)
print(res)

#plot(x[which(z<=1)],y[which(z<=1)], xlab = "X", ylab = "Y", main="Monte Carlo")
#points(x[which(z>1)],y[which(z>1)],col='blue')


#1.3 Memory allocation
N <- 1E5
data1 <- 1 
system.time({
    for (j in 2:N) {
      data1 <- c(data1, data1[j-1] + sample(-5:5, size=1))
    }
  })
#data1

data2 <- numeric(N)
data2[1] <- 1
system.time({
  for (j in 2:N) {
    data2[j] <- data2[j-1] + sample(-5:5, size=1)
  }
})
#data2

#1.4 Data Frames
data1 <- rnorm(1E4*1000)
dim(data1) <- c(1E4,1000)
system.time(data1 <- rowSums(data1))

dataf <- data.frame(data1)
system.time(data2 <- rowSums(dataf))

#1.6 Lookup tables
# data <- rnorm(1E6)
# data_ls <- as.list(data)
# names(data_ls) <- paste("V", c(1:1E6), sep="")
# ind_rand <- sample(1:1E6, size=100, replace=T)
# ind <- paste("V",ind_rand, sep="")
# 
# list_time <- sapply(ind, FUN=function(x) {system.time(data_ls[[x]])[3]})
# sum(list_time)
# 
# #install.packages("hash")
# library(hash)
# data_h <- hash(names(data_ls),data)
# hash_time <- sapply(ind, FUN=function(x) {system.time(data_h[[x]])[3]})
# sum(hash_time)

#1.7 Different implementations of functions
data <- rnorm(1E5*100)
dim(data) <- c(1E5,100)
system.time(prcomp_data <- prcomp(data))
system.time(princomp_data <- princomp(data))


#1.8 Compiling functions
sim <- function(l) {
  c <- rep(0,l)
  hits <- 0
  pow2 <- function(x) {
    x2 <- sqrt( x[1]*x[1]+x[2]*x[2] )
    return(x2)
  }
  for(i in 1:l){
    x = runif(2,-1,1)
    if( pow2(x) <=1 ){
      hits <- hits + 1
    }
    dens <- hits/i
    pi_partial = dens*4
    c[i] = pi_partial
  }
  #print(pi_partial)
  return(c)
}

library(microbenchmark)
library(compiler)
sim.comp0 <- cmpfun(sim, options=list(optimize=0))
sim.comp1 <- cmpfun(sim, options=list(optimize=1))
sim.comp2 <- cmpfun(sim, options=list(optimize=2))
sim.comp3 <- cmpfun(sim, options=list(optimize=3))

size <- 100000
bench <- microbenchmark(sim(size), sim.comp0(size), sim.comp1(size), sim.comp2(size), sim.comp3(size))
bench

library(ggplot2)
autoplot(bench)

#Just in time compilation
library(compiler)
enableJIT(level=3)

bench <- microbenchmark(sim(size))

#1.9 Calling external functions

sim <- function(l) {
  c <- rep(0,l)
  hits <- 0
  pow2 <- function(x) {
    x2 <- sqrt( x[1]*x[1]+x[2]*x[2] )
    return(x2)
  }
  for(i in 1:l){
    x = runif(2,-1,1)
    if( pow2(x) <=1 ){
      hits <- hits + 1
    }
    dens <- hits/i
    pi_partial = dens*4
    c[i] = pi_partial
  }
  #print(pi_partial)
  return(c)
}

size <- 100000
#gfortran -shared -fPIC -o picalc pi.f90
#dyn.load("picalc")
#is.loaded("pifunc")
#.Fortran("pifunc", n = as.integer(size))
#R CMD SHLIB pi.f90
dyn.load("pi.so")
is.loaded("pifunc")
.Fortran("pifunc", n = as.integer(size))

#In Windows
#using cygwin64 the folder is in: /cygdrive/c/Users/pedro/Desktop/TRABAJO_PROPIO/REPORTES/UMEA_DOCUMENTS/HPC2N_DOCUMENTS/CURSOS_FUTUROS/ML_in_R2019/Rscripts/1.Optimizing
#compile the code with gfortran -shared -o pi.dll pi.f90
#dyn.load("picalc.dll")
#is.loaded("pifunc")

library(microbenchmark)
bench <- microbenchmark(sim(size), .Fortran("pifunc", n = as.integer(size)))
bench

