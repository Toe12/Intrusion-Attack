##############################################################

library(caret)

data <- read.csv(file.choose())

col <- c("SameSrvRate", "LoggedIn",  "DstHostSameSrvRate", 
                 "DstHostSrvCount","Flag","Attack" )

inTrain <- createDataPartition(y=data$Attack, p=0.1, list=FALSE)

#str (data)

trainData <- data[inTrain,col]
testData <- data[-inTrain,col]

#dim <- nrow(trainData)
#dim(trainData)


##############################################################

#SVM 

library(e1071)

rbf.tune = tune.svm(Attack ~., data = trainData, kernel="radial",
                    gamma=c(0.1,0.5,1,2,3,4))

summary(rbf.tune)

#choose the best gamma value
best.rbf = rbf.tune$best.model

## plot the model

library(ggplot2)

tune.test = predict(best.rbf , testData) ## predic the model
t <- table(tune.test, testData$Attack) #c onfusion matrix

# plot the predicted model 

t1 <- as.numeric(tune.test) # # convert tune.test into numeric to plot the graph

# attacks after training data

qplot(t1,
      geom="histogram", 
      binwidth = 0.5, 
      main = "Histogram for Attack After training", 
      xlab = "Attack", 
      ylab = "Frequency", 
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2))


# attacks before traning data 

qplot(as.numeric(data$Attack),
      geom="histogram", 
      binwidth = 0.5, 
      main = "Histogram for Attack Before Tranining", 
      xlab = "Attack", 
      ylab = "Frequency", 
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),)

#overall accuracy
sum(diag(t)/sum(t))

#misclassification
1 - sum(diag(t)/sum(t))) 

## confusion matrix
confusionMatrix(tune.test , testData$Attack)
        

##############################################################

# https://rpubs.com/Kushan/296706

# https://www.youtube.com/watch?v=RNpglFQYwZI
