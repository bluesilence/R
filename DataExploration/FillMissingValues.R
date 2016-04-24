setwd('D:/Kaggle/BNPParibasCardifClaimsManagement')

train <- read.table("data/raw/train.csv", header = T, sep = ",", stringsAsFactors = FALSE) 

###
# select random sample for analysis using caret createDataPartition() function
###
set.seed(123)
idx <- createDataPartition(train$target, p = 0.01, list = FALSE)
sample.df <- train[idx, ]
## A lot of columns have NA values
colSums(is.na(sample.df))

###
# segregate numeric vs character data types
###
# get names of the explanatory variables
explanatory.attributes <- setdiff(names(sample.df), c("ID", "target"))

# determine data type for each explanatory variable
data.classes <- sapply(explanatory.attributes, function(x){class(sample.df[, x])})

# segregate explanatory variables by data type, eg. character, numeric, integer
unique.classes <- unique(data.classes)
attr.by.data.types <- lapply(unique.classes, function(x){names(data.classes[data.classes == x])})
names(attr.by.data.types) <- unique.classes
comment(attr.by.data.types) <- "list that categorize training data types"


# for numeric attributes use caret preProcess() and predict() functions to impute missing values
library(caret)
pp <- preProcess(sample.df[c(attr.by.data.types$numeric, attr.by.data.types$integer)],
                 method = c("medianImpute"))
# Fulfilled numeric & integer features
pp.sample.df <- predict(pp, sample.df[c(attr.by.data.types$numeric, attr.by.data.types$integer)])

# combine numeric data with character data
sample.df.imputed <- cbind(pp.sample.df, sample.df[attr.by.data.types$character])

# No column has NA now
colSums(is.na(sample.df.imputed))