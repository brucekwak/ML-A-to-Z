# Polynomial Regression

# Set the directory
setwd('C:/Users/KYY/Desktop/ML_Ato_Z/[0]Data')

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

# Feature Scaling - included in Linear regression package


# Fitting Linear Regression to the dataset
?lm
lin_reg = lm(formula = Salary ~ ., 
             data = dataset)
summary(lin_reg)


# Fitting Polynomial Regression to the dataset
dataset$Level2 = dataset$Level^2 # create new column
dataset$Level3 = dataset$Level^3 # create new column
dataset$Level4 = dataset$Level^4 # create new column
poly_reg = lm(formula = Salary ~ ., 
              data = dataset)
summary(poly_reg)



# Visualizing the Linear Regression results
install.packages('ggplot2')
library(ggplot2)
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') + 
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') + 
  ggtitle('Truth or Bluff (Linear Regression)') + 
  xlab('Level') + 
  ylab('Salary')


# Visualizing the Polynomial Regression results
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') + 
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') + 
  ggtitle('Truth or Bluff (Polynomial Regression)') + 
  xlab('Level') + 
  ylab('Salary')


# Predicting a new result with Linear Regression
data.frame(Level = 6.5)
y_pred = predict(lin_reg, newdata = data.frame(Level = 6.5))
y_pred
?predict

# Predicting a new result with Polynomial Regression
y_pred_poly = predict(poly_reg, newdata = data.frame(Level = 6.5, Level2 = 6.5^2, Level3 = 6.5^3, Level4 = 6.5^4))
y_pred_poly
