# Packages
library(caret)
library(randomForest)

# setting paths
trainnet <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testnet <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

#  loading datasets
train <- read.csv(file = trainnet, header = TRUE, sep = ",", na.strings = c("NA", "#DIV/0!", ""))
test <- read.csv(file = testnet, header = TRUE, sep = ",", na.strings = c("NA", "#DIV/0!", ""))

## Data Preparation
dim(train)
head(train)

# Let's examine the low variance vaiables and take them out.
fixedtrain <- nearZeroVar(train, saveMetrics = TRUE)
train1 <- train[,fixedtrain$nzv == FALSE]

# Now let's remove columns with too many missing values
# set removal threshold at 80% or greater
naCount <-sapply(train1, function(y) sum(length(which(is.na(y)))))
threshold <- .8
naList <- naCount[naCount >= threshold * dim(train1[1])]
namesNA <- names(train1) %in% names(naList)
train2 <- train1[!namesNA]
train3 <- train2[, -c(1:5)]

#  split data into training and test sets
inTrain = createDataPartition(train3[[1]], p = 0.6, list = FALSE)
training = train3[ inTrain,]
testing = train3[-inTrain,]
dim(training)
dim(testing)

## Model Development
set.seed(457)
modelFit <- train( classe ~ .,
                   data = training,
                   nodesize=10,
                   allowParallel=TRUE, do.trace=FALSE,
                   trControl = trainControl(method ='oob'),
                   method="rf")
print(modelFit)

ggplot(modelFit$result, aes(x = mtry, y = Accuracy)) +
        geom_point() +
        geom_line() +
        ggtitle('Accuracy versus mtry')

#  make predictions
prediction <- predict(modelFit, testing)
str(prediction)
str(as.factor(testing$classe))

## Accuracy Test
testing$classe <- as.factor(testing$classe)
confusionMatrix(factor(prediction), factor(testing$classe))