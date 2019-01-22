# -*- coding: utf-8 -*-

# Data Preprocessing

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
dataset = pd.read_csv('Data.csv')
X = dataset.iloc[:, :-1].values   # all the line, all the column except the last one
y = dataset.iloc[:, -1].values


# Taking care of missing data
from sklearn.preprocessing import Imputer
imputer = Imputer(missing_values='NaN', strategy='mean', axis=0, )   # ctrl + i: object inspector for help
imputer = imputer.fit(X[:, 1:3])  # all the lines, specific column
X[:, 1:3] = imputer.transform(X[:, 1:3])


# Encoding categorical data
from sklearn.preprocessing import LabelEncoder, OneHotEncoder
labelencoder_X = LabelEncoder()
X[:, 0] = labelencoder_X.fit_transform(X[:, 0])

onehotencoder = OneHotEncoder(categorical_features = [0])  # 0: index of categorical column
X = onehotencoder.fit_transform(X).toarray()

labelencoder_y = LabelEncoder()
y = labelencoder_y.fit_transform(y)


# Splitting the dataset into the Training set and Test set
from sklearn.cross_validation import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2,
                                                    random_state = 0)


# Feature Scaling
from sklearn.preprocessing import StandardScaler
sc_X = StandardScaler()
sc_X = sc_X.fit(X_train[:, 3:])
X_train[:, 3:] = sc_X.transform(X_train[:, 3:])
X_test[:, 3:] = sc_X.transform(X_test[:, 3:])








