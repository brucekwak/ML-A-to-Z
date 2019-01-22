# Regression Template

# Set the directory
setwd('C:/Users/KYY/Desktop/ML_Ato_Z/[0]Data')

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]



# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)   # short cut: F1 for help
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Feature Scaling - included in Linear regression package
# training_set = scale(training_set)
# test_set = scale(test_set)


# Fitting Regression Model to the dataset
# Create your regressor here

# Predicting a new result with Linear Regression
y_pred = predict(regressor, newdata = data.frame(Level = 6.5))
y_pred


# Visualizing the Regression Model results
# instsall.packages('ggplot2')
install.packages('ggplot2')
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') + 
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') + 
  ggtitle('Truth or Bluff (Regression Model)') + 
  xlab('Level') + 
  ylab('Salary')


# Visualizing the Regression Model results (for higher resolution and smoother curve)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)

ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') + 
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') + 
  ggtitle('Truth or Bluff (Regression Model)') + 
  xlab('Level') + 
  ylab('Salary')

