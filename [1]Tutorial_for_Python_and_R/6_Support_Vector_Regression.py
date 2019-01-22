# -*- coding: utf-8 -*-

# Support Vector Regression

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


# Splitting the dataset into the Training set and Test set
"""
from sklearn.cross_validation import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2,
                                                    random_state = 0)
"""

# Feature Scaling
# SVR library we use doesn't do feature scaling
from sklearn.preprocessing import StandardScaler
sc_X = StandardScaler()
sc_Y = StandardScaler()
X= sc_X.fit_transform(X)
y = sc_Y.fit_transform(y)



# Fitting the SVR to the dataset
from sklearn.svm import SVR
regressor = SVR(kernel = 'rbf')   # dno't use kernel 'linearl' in this data because we know that this data follows non-linear
regressor.fit(X, y)  # Fit the SVM model according to the given training data

# Predicting a new result with Regression model
y_pred = sc_Y.inverse_transform( regressor.predict( sc_X.inverse_transform(np.array([[6.5]])) ) )
# [6.5] = vector, not array.  [[6.5]]: an array of one line in one column


# Visualizing the SVR results (for higher resolution and smotther curve)
X_grid = np.arange(min(X), max(X), 0.1)
X_grid = X_grid.reshape(len(X_grid), 1)

plt.scatter(X, y, color = 'red')
plt.plot(X_grid, regressor.predict(X_grid), color = 'blue')
plt.title('Truth or Bluff (Regression Model)')
plt.xlabel('Position level')
plt.ylabel('Salary')
plt.show()

