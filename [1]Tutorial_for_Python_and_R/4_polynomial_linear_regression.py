# -*- coding: utf-8 -*-

# Polynomial Regression

# Importing the libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

import os

# Set the directory
currentPath = os.getcwd()
print(currentPath)
os.chdir('C:\\Users\\KYY\\Desktop\\ML_Ato_Z\\[0]Data')


# Importing the dataset
dataset = pd.read_csv('Position_Salaries.csv')
# make X -> matrix, y -> vector
X = dataset.iloc[:, 1:2].values   # don't need 'Position' column  # vector -> matrix
y = dataset.iloc[:, -1].values


# No - split data (-> very small data )

# No - feature scaling
#    the library we'll use do feature scaling


# Fitting Linear Regression to the dataset
from sklearn.linear_model import LinearRegression
lin_reg = LinearRegression()
lin_reg.fit(X, y)

# Fitting Polynomial Regression to the dataset
from sklearn.preprocessing import PolynomialFeatures
poly_reg = PolynomialFeatures(degree = 4)   # transform X metrix feature to polynomial
X_poly = poly_reg.fit_transform(X)   # matrix X -> create columns of polynomial
lin_reg_2 = LinearRegression()
lin_reg_2.fit(X_poly, y)


# Visualizing the Linear Regression results
plt.scatter(X, y, color = 'red')
plt.plot(X, lin_reg.predict(X), color = 'blue')
plt.title('Linear Regression')
plt.xlabel('Position level')
plt.ylabel('Salary')
plt.show()

# Visualizing the Polynomial Regression results
X_grid = np.arange(min(X), max(X), 0.1)
X_grid = X_grid.reshape(len(X_grid), 1)

plt.scatter(X, y, color = 'red')
plt.plot(X_grid, lin_reg_2.predict(poly_reg.fit_transform(X_grid)), color = 'blue')
plt.title('Polynomial Regression')
plt.xlabel('Position level')
plt.ylabel('Salary')
plt.show()


# Predicting a new result with Linear Regression
lin_reg.predict(6.5)   # salary or level 6.5

# Predicting a new result with polynomial Regression
lin_reg_2.predict(poly_reg.fit_transform(6.5))  # salary or level 6.5

