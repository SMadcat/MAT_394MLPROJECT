data.music <- read.csv(file="Project_Changed.csv",head=TRUE,sep=",")

library(caTools)
split <- sample.split(data.music,SplitRatio = 0.8)
split 
training.glm <- subset(data.music,split =="TRUE")
testing.glm <- subset(data.music,split =="FALSE")

glm.fit <- glm(target~.,training.glm,family="binomial")
summary(glm.fit)




glm.probs <-predict(glm.fit,testing.glm,target= "response")
glm.probs 
glm.pred <- ifelse(glm.probs>0.2,1,0)
glm.pred


table(ActualValue=testing.glm$target, PredictedValue=glm.probs>0.2)

#to find the threshold we use 
glm.probs <-predict(glm.fit,training.glm,target="response")
library(ROCR)
ROCRPred <- prediction(glm.probs,training.glm$target)
ROCRPerf <- performance(ROCRPred,"tpr","fpr")
plot(ROCRPerf,
     colorize=TRUE,
     print.cutoffs.at=seq(0,1,by=0.1),
     text.adj=c(1.2,1.2),
     avg="threshold",
     lwd=3)


