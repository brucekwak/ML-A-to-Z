# Decision Tree Regression

# Set the directory
setwd('C:/Users/KYY/Desktop/ML_Ato_Z/[0]Data')

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]


# Feature Scaling - included in Linear regression package
# training_set = scale(training_set)
# test_set = scale(test_set)


# Fitting Decision Tree Regression to the dataset
install.packages('rpart')
library(rpart)
# press F1 for short cut to 'help'
regressor = rpart(formula = Salary ~ ., 
                data = dataset,
                control = rpart.control(minsplit = 3))


# Predicting a new result with Decision Tree Regression
y_pred = predict(regressor, newdata = data.frame(Level = 6.5))
y_pred



# Visualizing the Decision Tree Regression results (for higher resolution and smoother curve)
install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)

ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') + 
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') + 
  ggtitle('Truth or Bluff (Decision Tree Regression)') + 
  xlab('Level') + 
  ylab('Salary')