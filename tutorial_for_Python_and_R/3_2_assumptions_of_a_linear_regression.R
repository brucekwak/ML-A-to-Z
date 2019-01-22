# Assumptions of a linear regression


# import library
# install.packages("moments")   # check the kurtosis, skewness
library(moments)

# Set the directory
setwd('C:/Users/KYY/Desktop/ML_Ato_Z/[0]Data')

# Importing the dataset
dataset = read.csv('50_Startups.csv')


# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))


mlr_startups <- lm(Profit ~ ., data = dataset)  # lm = linear model


# check some of the assumptions of linear regression
# 1. check homoscedasticity -----------------------------------------------------
plot(mlr_startups)
# 1st plot - check homoscedasticity
#     _red line is the center line
# 2nd plot - check normality (<-> whether the residuals are normaliy distributed or not)


# 2. normality test of residuals -----------------------------------------------------
#   2-1. draw histogram and compare with the normal distribution
startups_resid <- resid(mlr_startups)

m <- mean(startups_resid)  # mean
std <- sqrt(var(startups_resid))  # standard deviation

hist(startups_resid, breaks=20, density = 20, prob=TRUE, 
     xlab="x-variable",
     main="normal curve over histogram")
# breaks = a single number giving the number of cells for the histogram.
# density: cell's shadow

curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")
# dnorm(): make x values using mean & standard deviation


#    2-2. the quantitative indicator for checking the normality of residuals
# normal distribution's skewness = 0   => whether the center is biased to each side
# normal distribution's kurtosis = 0
skewness(startups_resid)
kurtosis(startups_resid)


#    2-3. Shapiro-Wilk normality test
shapiro.test(startups_resid)


# 3. check multicollinearity -----------------------------------------------------
# install.packages("car")
library("car")
vif(mlr_startups)
vif(mlr_startups) > 10


# 4. coefficient of determination & F-statistic -----------------------------------
summary(mlr_startups)
# p-value of F test is lower than significant level(0.05).
#   So, reject the null hypothesis. => this model is useful.
# Adjusted R-squared = 0.9452
#   So, this model explains the 94.52% of dependent variable's variability


# 5. Plot the predicted result  ---------------------------------------------------
plot(dataset$Profit, fitted(mlr_startups))
abline(0,1,lty=3)  # draw y = 0 + 1*x    # add straight lines to a plot, a = intercept, b = slope


