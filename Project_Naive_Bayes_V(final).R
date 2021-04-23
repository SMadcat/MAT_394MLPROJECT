
data.music = read.csv("Project_Changed.csv")


music.data = data.frame(data.music)


set.seed(2)
id <- sample(2,nrow(music.data),prob =c(0.7,0.3),replace = TRUE)


training.nb <- music.data[id==1,]
testing.nb <- music.data[id==2,]

library(e1071)
library(caret) 

nb.fit <- naiveBayes(target~.,data = training.nb)
nb.fit

nb.pred <- predict(nb.fit,testing.nb)
nb.pred

confusionMatrix(table(nb.pred,testing.nb$target))


