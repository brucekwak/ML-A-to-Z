# SVR

# Suppor Vector Regression


# Set the directory
setwd('C:/Users/KYY/Desktop/ML_Ato_Z/[0]Data')

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]


# Feature Scaling - included in Linear regression package
# training_set = scale(training_set)
# test_set = scale(test_set)


# Fitting SVR to the dataset
install.packages('e1071')
library(e1071)
?svm
regressor = svm(formula = Salary ~ ., 
                data = dataset,
                type = 'eps-regression',
                kernel = 'radial')


# Predicting a new result with Support Vector Regression
y_pred = predict(regressor, newdata = data.frame(Level = 6.5))
y_pred



# Visualizing the SVR results (for higher resolution and smoother curve)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)

ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') + 
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') + 
  ggtitle('Truth or Bluff (SVR)') + 
  xlab('Level') + 
  ylab('Salary')