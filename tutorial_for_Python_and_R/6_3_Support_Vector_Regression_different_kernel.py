# -*- coding: utf-8 -*-

# SVR using linear and non-linear kernels
# [Reference] https://scikit-learn.org/stable/auto_examples/svm/plot_svm_regression.html

import numpy as np
from sklearn.svm import SVR
import matplotlib.pyplot as plt

# #############################################################################
# Generate sample data
X = np.sort(5 * np.random.rand(40, 1), axis=0)
y = np.sin(X).ravel()

# Check the data plot
plt.scatter(X, y)
plt.show()

# #############################################################################
# Add noise to targets
y[::5] += 3 * (0.5 - np.random.rand(8))   # x[startAt:endBefore:skip]

# Check the data plot
plt.scatter(X, y)
plt.show()


# #############################################################################
# Fit regression model
svr_rbf = SVR(kernel='rbf', C=1e3, gamma=0.1, epsilon = 0.1)  # C: Penalty parameter C of the error term. 
svr_lin = SVR(kernel='linear', C=1e3, epsilon = 0.1)
svr_poly = SVR(kernel='poly', C=1e3, degree=2, epsilon = 0.1)
y_rbf = svr_rbf.fit(X, y).predict(X)
y_lin = svr_lin.fit(X, y).predict(X)
y_poly = svr_poly.fit(X, y).predict(X)

# #############################################################################
# Look at the results
lw = 2
plt.scatter(X, y, color='darkorange', label='data')
plt.plot(X, y_rbf, color='navy', lw=lw, label='RBF model')
plt.plot(X, y_lin, color='c', lw=lw, label='Linear model')
plt.plot(X, y_poly, color='cornflowerblue', lw=lw, label='Polynomial model')
plt.xlabel('data')
plt.ylabel('target')
plt.title('Support Vector Regression')
plt.legend()
plt.show()
