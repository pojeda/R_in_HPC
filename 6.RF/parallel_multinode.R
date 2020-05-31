##### Chapter 11: Improving Model Performance -------------------
#Source: https://github.com/dataspelunking/MLwR
# load the credit dataset
#install.packages("caret")
#install.packages("doParallel")


credit <- read.csv("credit.csv")
library(Rmpi)
library(caret)
library(foreach)
library(doParallel)

## Random Forests ----
# random forest with default settings
#install.packages("randomForest")
#install.packages("e1071")
library(randomForest)
set.seed(300)
rf <- randomForest(default ~ ., data = credit)
rf

#using Caret
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 1000)

# auto-tune a random forest
grid_rf <- expand.grid(.mtry = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))

set.seed(300)
nuniv <- mpi.universe.size()-1
nuniv
cluster <- makeCluster(nuniv, type='MPI') # convention to leave 1 core for OS
registerDoParallel(cluster)
system.time( 
m_rf <- train(default ~ ., data = credit, method = "rf",
              metric = "Kappa", trControl = ctrl,
              tuneGrid = grid_rf)
)
stopCluster(cluster)
m_rf

