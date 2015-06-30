install.packages('e1071')
library(e1071)

install.packages('mlbench')
library(mlbench)

data(Glass, package = "mlbench")
head(Glass)

index <- 1:nrow(Glass)
table(Glass[, 10])
# Sample 1/3 from original dataset as test set
testindex <- sample(index, trunc(length(index) / 3))
testset <- Glass[testindex, ]
trainset <- Glass[-testindex, ]

# soft-margin gaussian kernal SVM
svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 1)
# column 10 is the classification output
svm.pred <- predict(svm.model, testset[, -10])

table(pred = svm.pred, actual = testset[, 10])

# soft-margin polynomial(d == 2) kernal SVM
svm.model <- svm(Type ~ ., data = trainset, cost = 10000, degree = 2, kernel = 'polynomial')
# column 10 is the classification output
svm.pred <- predict(svm.model, testset[, -10])

table(pred = svm.pred, actual = testset[, 10])

# Plot all data points in test set
plot(cmdscale(dist(testset[, -10])), col = as.integer(testset[, 10]), pch = c("o"))
# Plot SVs' indices in test set
plot(cmdscale(dist(testset[, -10])), col = as.integer(testset[, 10]), pch = c("o", "+")[1:length(testset) %in% svm.model$index + 1])
