#Source Data: https://ourworldindata.org/coronavirus-source-data

covid19 <- read.csv("owid-covid-data_7.csv", stringsAsFactors = TRUE)
str(covid19)

#Clean the data
covid19_clean <- covid19[1:18863,c(1,4,5,6,7)]
str(covid19_clean)
head(covid19_clean)

#Column for Countries as numbers
covid19_clean$id <- c(as.factor(covid19_clean$iso_code))

#Separate data
set.seed(1234)
ind <- sample(2,nrow(covid19_clean), replace = TRUE, prob = c(0.7,0.3))
train <- covid19_clean[ind==1,]
test <- covid19_clean[ind==2,]

str(train)
str(test)

#Creating forest
library(randomForest)
set.seed(453)
system.time(
rf <- randomForest(new_deaths~total_cases+new_cases+id, data=train)
)
#Elapsed time: 27.82 sec
print(rf)

#Predictions
#p1 <- predict(rf,train)
#head(p1)
p2 <- predict(rf,test)


#Checking some resulting values
p2[5405:5410]
test[5405:5410,5]
#> p2[5405:5410]
#2091.216 1962.838 1794.194 1553.555 1697.809 1448.719 
#> test[5405:5410,5]
#3770 2172 2611 1252 2239 1703

#Plotting errors
plot(rf)



#####Using Caret
library(caret)
library(doParallel)

## Random Forests ----
# random forest with default settings
#install.packages("randomForest")
#install.packages("e1071")


#using Caret
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 4)
?trainControl

# auto-tune a random forest
grid_rf <- expand.grid(.mtry = c(2,3))

set.seed(300)

cluster <- makeCluster(4) # convention to leave 1 core for OS
registerDoParallel(cluster)
system.time( 
  m_rf <- train(new_deaths~total_cases+new_cases+total_deaths+id, data = train, method = "rf",
                trControl = ctrl,
                tuneGrid = grid_rf )
)
stopCluster(cluster)
m_rf

#Elapsed time: 1329.60 sec
#mtry  RMSE      Rsquared   MAE     
#2     40.47753  0.9047732  5.114806
#3     41.25076  0.9009545  5.175151

#Predictions
p3 <- predict(m_rf,test)

p3[5405:5410]
p2[5405:5410]
test[5405:5410,5]

#> p3[5405:5410]
#2423.206 2101.775 1903.255 1515.619 1663.170 1349.631 
#> p2[5405:5410]
#2091.216 1962.838 1794.194 1553.555 1697.809 1448.719 
#> test[5405:5410,5]
#3770 2172 2611 1252 2239 1703