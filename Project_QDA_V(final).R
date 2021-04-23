data.music = read.csv("Project_Changed.csv")
library(MASS)
set.seed(2)
id <- sample(2,nrow(data.music),prob =c(0.7,0.3),replace = TRUE)



training.qda <- data.music[id==1,]
testing.qda <- data.music[id==2,]


qda.fit <- qda(target ~., data = training.qda)

qda.pred <- predict(qda.fit,testing.qda)
qda.class <- qda.pred$class
library(caret)
confusionMatrix(table(qda.class,testing.qda$target))

