data.music <- read.csv(file="Project_Changed.csv",head=TRUE,sep=",")

data.music$target <- as.character(data.music$target)
data.music$target <- as.factor(data.music$target)

#Divide data set in training and testing.
#In the previous method we had used split function, here we will split data by 

set.seed(2)
id <- sample(2,nrow(data.music),prob =c(0.7,0.3),replace = TRUE)



training.rf <- data.music[id==1,]
testing.rf <- data.music[id==2,]

#Random Forest
library(randomForest)
#optimised value of mtry

bestmrty <- tuneRF(training.rf,training.rf$target,
                   stepFactor = 1.2, 
                   improve = 0.01, trace = TRUE,plot = TRUE)



forest <- randomForest(target~.,data = training.rf,ntree=1001)
forest
# gives priority to variables
forest$importance
varImpPlot(forest)

probs.forest <- predict(forest, newdata = testing.rf, type = "class")
probs.forest


library(caret)
confusionMatrix(table(probs.forest,testing.rf$target))


   
