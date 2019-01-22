# Random Forest Regression

# Set the directory
setwd('C:/Users/KYY/Desktop/ML_Ato_Z/[0]Data')

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]


# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library('randomForest')
set.seed(1234)
regressor = randomForest(x = dataset[1],
                         y = dataset$Salary,
                         ntree = 300)


# Visualizing the Random Forest Regression results
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') + 
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') + 
  ggtitle('Truth or Bluff (Random Forest Regression)') + 
  xlab('Level') + 
  ylab('Salary')


# Predicting a new result with Random Forest Regression
data.frame(Level = 6.5)
y_pred = predict(regressor, newdata = data.frame(Level = 6.5))
y_pred
?predict