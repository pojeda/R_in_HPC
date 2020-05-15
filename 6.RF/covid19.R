#Source Data: https://ourworldindata.org/coronavirus-source-data


covid19 <- read.csv("owid-covid-data_5.csv", stringsAsFactors = TRUE)
str(covid19)

#Clean the data
covid19_clean <- covid19[1:15146,c(1,4,5,6,7)]
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
rf <- randomForest(new_deaths~total_cases+new_cases+id, data=train)
print(rf)

#p1 <- predict(rf,train)
#head(p1)

p2 <- predict(rf,test)
head(p2)

print(p2)
print(test$new_deaths)
p2[2820]

#Plotting errors
plot(rf)



#Using Caret
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
grid_rf <- expand.grid(.mtry = c(2))

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

p3 <- predict(m_rf,test)
print(p3)
p2[2594]
p3[2594]
