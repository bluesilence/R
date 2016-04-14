library("caret")
library("mlbench")
library("pROC")

data(Sonar)
set.seed(107)

inTrain <- createDataPartition(y = Sonar$Class, p = .75, list = FALSE)
training <- Sonar[inTrain, ]
testing <- Sonar[-inTrain, ]

my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(training$Class, 25),
  summaryFunction=twoClassSummary
)

### Caret List
library("rpart")
library("caretEnsemble")
model_list <- caretList(
  Class ~ .,
  data=training,
  trControl=my_control,
  methodList=c("glm", "rpart")
)

model_list
# Plot ROC for each resample for the 2 models in model_list
xyplot(resamples(model_list))
modelCor(resamples(model_list))

p <- as.data.frame(predict(model_list, newdata=head(testing)))
print(p)

library("mlbench")
library("randomForest")
library("nnet")

# models in both methodList and tuneList are used as one model in the caretList
model_list_big <- caretList(
  Class~., data=training,
  trControl=my_control,
  metric="ROC",
  methodList=c("glm", "rpart"),
  tuneList=list(
    rf1=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=2)),
    rf2=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=10), preProcess="pca"),
    nn=caretModelSpec(method="nnet", tuneLength=2, trace=FALSE)
  )
)

model_list_big

xyplot(resamples(model_list_big))
modelCor(resamples(model_list_big))

### Caret Ensemble
greedy_ensemble <- caretEnsemble(
  model_list, 
  metric="ROC",
  trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  ))

summary(greedy_ensemble)

greedy_ensemble_big <- caretEnsemble(
  model_list_big, 
  metric="ROC",
  trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  ))

summary(greedy_ensemble_big)
