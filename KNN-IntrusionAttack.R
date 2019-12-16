# load package
library(caret)

data <- read.csv(file.choose(), header = T)

# shuffle the data
set.seed(9850)
g <- runif(nrow(data))
data1 <- data[order(g[1:70000]),]

indices <- sample(1:50000)
trainData <- data1[indices,]

inTest <- sample(50001:60000)
testData <- data1[inTest,]

# change data catagorial data into numeric 

trainData$Flag <- as.numeric(trainData$Flag)
trainData$ProtocolType <- as.numeric(trainData$ProtocolType)
trainData$Service <- as.numeric(trainData$Service)

testData$Flag <- as.numeric(testData$Flag)
testData$ProtocolType <- as.numeric(testData$ProtocolType)
testData$Service <- as.numeric(testData$Service)

normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

trainFeatures <- as.data.frame(lapply(trainData[1:41], normalize))
testFeatures <- as.data.frame(lapply(testData[1:41], normalize))

# apply NA to 0

trainFeatures[is.na(trainFeatures)] <- 0
testFeatures[is.na(testFeatures)] <- 0

# load package
library(class)

testPredict <- knn(train = trainFeatures, test = testFeatures, 
                   cl = trainData$Attack, k = 100) # Nearest Neighbors 100

# confusin matrix

tab <- table(testPredict4, testData$Attack)
tab

sum(diag(tab)/sum(tab))

confusionMatrix(tab)

## which k is the best

testPredict1 <- knn(train = trainFeatures, test = testFeatures, 
                    cl = trainData$Attack, k = 150) # Nearest Neighbors 150

testPredict2 <- knn(train = trainFeatures, test = testFeatures, 
                    cl = trainData$Attack, k = 200) # Nearest Neighbors 200

testPredict3 <- knn(train = trainFeatures, test = testFeatures, 
                    cl = trainData$Attack, k = 300) # # Nearest Neighbors 300

testPredict4 <- knn(train = trainFeatures, test = testFeatures, 
                    cl = trainData$Attack, k = 400) # # Nearest Neighbors 300


# accuarcy 

knn.mod <- 100 * sum(testData$Attack == testPredict)/NROW(testData$Attack)
knn.mod1 <- 100 * sum(testData$Attack == testPredict1)/NROW(testData$Attack)
knn.mod2 <- 100 * sum(testData$Attack == testPredict2)/NROW(testData$Attack)
knn.mod3 <- 100 * sum(testData$Attack == testPredict3)/NROW(testData$Attack)

# combine the result

kValues <- rbind(100, 150, 200, 300) # kvalues 
accuarcy <- rbind(knn.mod, knn.mod1, knn.mod2, knn.mod3) # combine the result


# plot the result 
plot(kValues, accuarcy, type = 'b', xlab = 'K-values', ylab = 'Accuracy', 
     main = 'choosing KNN values', col = 'brown')

############################################


