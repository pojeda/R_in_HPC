#2.1 Profiling memory
siml <- function(l) {
  c <- rep(0,l)
  hits <- 0
  listp <- as.list(seq(10000000))
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

#Using Rprof

Rprof("Rprof-mem.out", memory.profiling=TRUE)
res <- siml(size)
Rprof(NULL)
summaryRprof("Rprof-mem.out", memory="both")

#Using garbage collector gc

gc()
size <- 1000000
gcinfo(TRUE)
res <- siml(size)
gc()
gcinfo(FALSE)


gc(reset=TRUE)
listp <- as.list(seq(10000000))
gc()
rm(listp)
gc()
gc(reset=TRUE)

#Using Pryr
library(pryr)

sizes <- sapply(0:50, function(n) object_size(seq_len(n)))
plot(0:50, sizes, xlab = "Length", ylab = "Size (bytes)", 
     type = "s")


x <- 1:1e6
object_size(x)

y <- list(x, x, x)
object_size(y)

object_size(x, y)

y[[1]] <- x+1
object_size(y)


### Using Lineprof

#Installing package:  
#devtools::install_github("hadley/lineprof")
library(lineprof)
siml <- function(l) {
  c <- rep(0,l)
  hits <- 0
  listp <- as.list(seq(10000000))
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
res <- sim(10000)
prof <- lineprof(siml(10000))
summary(prof)
prof
shine(prof)


## Handling large arrays
#install.packages("bigmemory")
library(bigmemory)
bm <- big.matrix(1e8, 3, backingfile = "bm", backingpath = getwd())
bm

my.bm <- attach.big.matrix(file.path(getwd(), "bm.desc"))

#now, work with chunks of 10^7 rows,

chunksize <- 1e7
start <- 1
while (start <= nrow(bm)) {
  end <- min(start + chunksize -1, nrow(bm))
  chunksize <- end - start + 1
  bm[start:end, 1] <- rpois(chunksize, 1e3)
  bm[start:end, 2] <- sample(0:1, chunksize, TRUE, c(0.7,0.3))
  bm[start:end, 3] <- runif(chunksize, 0, 1e5)
  start <- start + chunksize
}

#Exercises: 

#1. Using the functions sumcol() and colSums() from the Profiling section, 
#obtain a memory profiling summary using Rprof


#2. Using the bm array above, compute the mean and standard deviation of each column

