library(doParallel)

x <- iris[which(iris[,5] != "setosa"), c(1,5)]
trials <- 10000

#Sequential version
stime <- system.time({ 
    r <- foreach(icount(trials), .combine=cbind) %do% {
        ind <- sample(100,100, replace=TRUE)
        result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
        coefficients(result1)
    }
})[3]

stime


#Parallel version
cl <- makeCluster(12)
registerDoParallel(cl)

ptime <- system.time({ 
    r <- foreach(icount(trials), .combine=cbind) %dopar% {
        ind <- sample(100,100, replace=TRUE)
        result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
        coefficients(result1)
    }
})[3]

ptime

stopCluster(cl)




#https://www.r-bloggers.com/the-wonders-of-foreach/
