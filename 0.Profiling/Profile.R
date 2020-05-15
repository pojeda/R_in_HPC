#0.Profiling
#Calculation of Pi using Monte Carlo 


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
 
 return(c)
}

#Plotting the function
size <- 100000
res <- sim(size)
plot(res[1:size],type='l')
lines(rep(pi,size)[1:size], col = 'red')


#0.1 Monitoring the execution time with system.time()
size <- 500000
system.time(
 res <- sim(size)
)

#0.2 Monitoring execution times with tic toc
#install.packages("tictoc")

library("tictoc")

size <- 1000000
sim2 <- function(l) {
   c <- rep(0,l)
   hits <- 0
   pow2 <- function(x) {
      x2 <- sqrt( x[1]*x[1]+x[2]*x[2] )
      return(x2)
   }
   tic("only for-loop")
   for(i in 1:l){
      x = runif(2,-1,1)
      if( pow2(x) <=1 ){
         hits <- hits + 1
      }
      dens <- hits/i
      pi_partial = dens*4
      c[i] = pi_partial
   }
   toc(log = TRUE)
   return(c)
}

tic("Total execution time")
res <- sim2(size)
toc(log = TRUE)
tic.log()
tic.clearlog()

#0.3 Using Rprof
###Installation 
#install.packages("proftools")
#for R<3.5
#source("http://bioconductor.org/biocLite.R")
#biocLite(c("graph","Rgraphviz"))
#for R>3.5
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install()
#BiocManager::install(c("graph","Rgraphviz"))

size <- 500000
Rprof("Rprof.out")
res <- sim(size)
Rprof(NULL)
summaryRprof("Rprof.out")

library(proftools)
p <- readProfileData(filename = "Rprof.out")
plotProfileCallGraph(p, style=google.style, score="total")

#0.4 Using rbenchmark
#install.packages("rbenchmark")
library(rbenchmark)
size <- 500000
bench <- benchmark(sim(size), replications=10)
bench

?benchmark

#0.5 Using microbenchmark
#install.packages("microbenchmark")
library(microbenchmark)
microbenchmark(sim(size), times=10)


#Exercise

#Given the matrix A of ones with a size of 5000x5000

A <- #initialize the matrix

#compare the profiling results of the following functions

# a) 
sumcol <- function(B) {
   l <-   #obtain the number of columns
   colsm <-  #create a vector to save the sums

   for(i in 1:l){  
      s <- 0  #collect partial sum
      for(j in 1:l){
          s <- s + #sum the columns of the matrix
      }
      colsm[i] <- s
   }
   return(colsm)
}


# b)
colSums(A)

#answer the points of the markdown document. 

