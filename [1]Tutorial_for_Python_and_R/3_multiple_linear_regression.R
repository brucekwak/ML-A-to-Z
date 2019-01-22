# Multiple Linear Regression

# Set the directory
setwd('C:/Users/KYY/Desktop/ML_Ato_Z/[0]Data')

# Importing the dataset
dataset = read.csv('50_Startups.csv')


# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)   # short cut: F1 for help
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Feature Scaling - included in Linear regression package


# Fitting Multiple Linear Regression to the Training set
# 'F1': short cut for help
regressor = lm(formula = Profit ~ .,  # use all the independent variable 
               data = training_set)
summary(regressor)


# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)



# Building the optimal model using Backward Elimination
# use total dataset (not only trainig set)
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,  # use all the independent variable 
               data = dataset)
summary(regressor)
# => remove all the state (highest p-value over significant level)
#    (because State2: 0.990, State3: 0.943 => state variables seems not to have relationship with the dependent variable)
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,  # use all the independent variable 
               data = dataset)
summary(regressor)
# => remove Administration (highest p-value over significant level)
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,  # use all the independent variable 
               data = dataset)
summary(regressor)
# => remove Marketing.Spend (highest p-value over significant level)
regressor = lm(formula = Profit ~ R.D.Spend,  # use all the independent variable 
               data = dataset)
summary(regressor)


# [Automatic implementation] Backward elimination
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    # find that variable
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)
