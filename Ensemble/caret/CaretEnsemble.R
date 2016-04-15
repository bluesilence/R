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
  methodList=c("glm", "rpart", "rf", "nnet")#,
#   tuneList=list(
#     rf1=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=2)),
#     rf2=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=10), preProcess="pca"),
#     nn=caretModelSpec(method="nnet", tuneLength=2, trace=FALSE)
#   )
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
  )
)

summary(greedy_ensemble_big)
varImp(greedy_ensemble_big)

library("caTools")
## Predict with a list of models
model_preds <- lapply(model_list_big, predict, newdata=testing, type="prob")
model_preds <- lapply(model_preds, function(x) x[,"M"])
model_preds <- data.frame(model_preds)

## Predict with the ensemble of the models in the list above
ens_preds <- predict(greedy_ensemble_big, newdata=testing, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, testing$Class)


### Caret Stack
glm_stacking <- caretStack(
  model_list_big,
  method="glm",
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
model_preds2 <- model_preds
model_preds2$stacking <- predict(glm_stacking, newdata=testing, type="prob")
colAUC(model_preds2, testing$Class)
CF <- coef(glm_stacking$ens_model$finalModel)[-1]
CF/sum(CF)


library("gbm")
gbm_ensemble <- caretStack(
  model_list_big,
  method="gbm",
  verbose=FALSE,
  tuneLength=10,
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
model_preds3 <- model_preds
model_preds3$stacking <- predict(gbm_ensemble, newdata=testing, type="prob")
colAUC(model_preds3, testing$Class)
