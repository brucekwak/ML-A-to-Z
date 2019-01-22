# Multiple Linear Regression

# ��Ű�� ��ġ
install.packages("party")  # CART
library(party)

install.packages("rpart")
library(rpart)

install.packages("caret")
library(caret)

install.packages("tree")  # tree, cv.tree
library(tree)

install.packages("e1071")  # confusion matrix
library(e1071)

## ����1. ------------------------------------------------------------------------------------------------
# ÷�ε� Playdata.csv���� ���� �� (Outlook, Temperature, Huminidity, Wind)�� 
# Information Gain�� ����Ͻÿ�. (Entropy, Gini-index ����)
setwd("C:/Users/EnglishKYY/Documents/Multivariative Statistical Analytics_data")
playData <- read.csv("Playdata.csv")

str(playData)
summary(playData)
head(playData)

# ����Ȯ��
table(playData$Outlook)
table(playData$Temperature)
table(playData$Humidity)
table(playData$Wind)
table(playData$Play)

# 1.2 ���� Ʈ�� �����
# Performance Evaluation Function -----------------------------------------
# 2���� �з������� ����
perf_eval <- function(cm){
  
  # True positive rate: TPR (Recall)
  TPR <- cm[2,2]/sum(cm[2,])
  # Precision
  PRE <- cm[2,2]/sum(cm[,2])
  # True negative rate: TNR
  TNR <- cm[1,1]/sum(cm[1,])
  # Simple Accuracy
  ACC <- (cm[1,1]+cm[2,2])/sum(cm)
  # Balanced Correction Rate
  BCR <- sqrt(TPR*TNR)
  # F1-Measure
  F1 <- 2*TPR*PRE/(TPR+PRE)
  
  return(c(TPR, PRE, TNR, ACC, BCR, F1))
}

set.seed(12345)  # �������� ����

playData_x <- playData[,-c(5)]  # 5��°�� ���Ӻ���(play)
playData_y <- as.data.frame(as.factor(playData[,5]))  # party ��Ű�������� �з��� �ϰ� ������, ���Ӻ����� factor���̾�� ��.
str(playData_y)
summary(playData)

##### gini_index, Entropy ����ϴ� �Լ� ���� ---------------------------------------
# input: �����ͼ�(data.frame), ����ȣ 
# output: �ش� �� ��ȣ �������� gini index ���ؼ� ���
##### �Լ���_one�� �����ͼ��� split�Ǳ� ���� ��ǥ��
##### �Լ���_all�� �����ͼ��� split�� ��, ��ü�� ���� ��ǥ��

gini_index_one <- function(dataset, col_idx) {  # input: �����ͼ�(data.frame), ����ȣ
  tmp_table <- table(dataset[,col_idx])  # Outlook
  total <- sum(tmp_table)
  
  variable_level <- length(tmp_table)  # �ش� ������ level
  
  gini_index <- 0
  # pi^2 ���� �����ֱ�
  for(i in 1:variable_level) {  
    gini_index <- gini_index + (tmp_table[i]/total)^2
  }
  
  gini_index <- 1 - gini_index
  # answer_gini <- paste( names(dataset)[col_idx], "������ gini index =", as.character(gini_index) )
  # print(answer_gini)
  gini_index <- as.numeric(gini_index)
  return(gini_index)
}


Entropy_one <- function(dataset, col_idx) {  # input: �����ͼ�(data.frame), ����ȣ
  tmp_table <- table(dataset[,col_idx])  # Outlook
  total <- sum(tmp_table)
  variable_level <- length(tmp_table)  # �ش� ������ level
  
  Entropy <- 0
  for(i in 1:variable_level) {  
    tmp_prob <- tmp_table[i]/total
    if (tmp_prob != 0) {
      # [�����] �� Ȯ���� 0�� ���� ���ؼ��� ��� ���ϰ� �Ѿ�� ������ ó��
      # log2(0) = -Inf �̹Ƿ�, ���� ��Ʈ���� ��꿡�� 0 * (-Inf) = 0���� ó���Ѵ�
      # ������ R������ �׷��� ����� ���, 0�� �ƴ϶� NaN�� ��ȯ���ֹǷ� ���� ó���Ѵ�.
      Entropy <- Entropy - ( (tmp_table[i]/total) * log2(tmp_table[i]/total) )      
    }
  }
  
  # answer_entropy <- paste( names(dataset)[col_idx], "������ Entropy =", as.character(Entropy) )
  # print(answer_entropy)
  Entropy <- as.numeric(Entropy)
  return(Entropy)
}


gini_index_after_split <- function(dataset, col_idx) {  # �ش� col_idx �������� split ��, ��ü�� ���� gini index
  # dataset�� �ش� col_idx�� level���� �ɰ� ��
  # ������ ���� gini_index_one �Լ��� �ְ�
  # ������ ũ�⺰�� Ri�� ���ؼ�
  # ���� ���ϸ�, �� �Լ��� ��ȯ���� ���� �� �ִ�.
  
  # level���� Ri * gini index ���ϱ�
  tmp_table <- table(dataset[,col_idx])
  total <- sum(tmp_table)
  tmp_levels <- levels(dataset[,col_idx])  # ������ level
  
  gini_index_total <- 0
  for(i in 1:length(tmp_levels)) {  # ������ ���� ����
    # Ri ���ϱ�
    tmp_r <- tmp_table[i] / total
    
    # tmp_p ���ϱ�
    subset_data <- subset(dataset, dataset[,col_idx] == tmp_levels[i])  # �ش� �����ͼ¸� ����
    tmp_gini <- gini_index_one(subset_data, 5)  # ������ �����ͼ� ������ ���Ӻ����� 'Play' ���� gini_index ���
    gini_index_total <- gini_index_total + ( tmp_r * tmp_gini)
  }
  
  # answer_entropy <- paste("split ��", names(dataset)[5], "������ gini index =", as.character(gini_index_total) )
  # print(answer_entropy)
  gini_index_total <- as.numeric(gini_index_total)
  return(gini_index_total)
}


Entropy_after_split <- function(dataset, col_idx) {  # �ش� col_idx �������� split ��, ��ü�� ���� Entropy
  # dataset�� �ش� col_idx�� level���� �ɰ� ��
  # ������ ���� Entropy_one �Լ��� �ְ�
  # ������ ũ�⺰�� Ri�� ���ؼ�
  # ���� ���ϸ�, �� �Լ��� ��ȯ���� ���� �� �ִ�.
  
  # level���� Ri * (������ Entropy) ���ϱ�
  tmp_table <- table(dataset[,col_idx])
  total <- sum(tmp_table)
  tmp_levels <- levels(dataset[,col_idx])  # ������ level
  
  Entropy_total <- 0
  for(i in 1:length(tmp_levels)) {  # ������ ���� ����
    # Ri ���ϱ�
    tmp_r <- tmp_table[i] / total
    
    # tmp_p ���ϱ�
    subset_data <- subset(dataset, dataset[,col_idx] == tmp_levels[i])  # �ش� �����ͼ¸� ����
    tmp_entropy <- Entropy_one(subset_data, 5)  # ������ �����ͼ� ������ ���Ӻ����� 'Play' ���� gini_index ���
    Entropy_total <- Entropy_total + ( tmp_r * tmp_entropy)
  }
  
  # answer_entropy <- paste("split ��", names(dataset)[5], "������ gini index =", as.character(gini_index_total) )
  # print(answer_entropy)
  Entropy_total <- as.numeric(Entropy_total)
  return(Entropy_total)
}

### information_gain ���ϴ� �Լ�
info_gain_gini <- function(dataset, col_idx) {
  before <- gini_index_one(dataset, 5)   # ���Ӻ��� ����
  after <- gini_index_after_split(dataset, col_idx)
  info_gain <- before - after
  return(info_gain)
}

info_gain_Entropy <- function(dataset, col_idx) {
  before <- Entropy_one(dataset, 5)    # ���Ӻ��� ����
  after <- Entropy_after_split(dataset, col_idx)
  info_gain <- before - after
  return(info_gain)
}


##### --------------------------------------------------------------------------
# infomation gain ���ϱ�
names(playData)
info_gain_mat <- matrix(0, 4, 2)
colnames(info_gain_mat) <- c("gini index", "Entropy")
rownames(info_gain_mat) <- c("Outlook", "Temperature", "Humidity", "Wind")
info_gain_mat

# ���� �� information gain ���ϱ�
for(i in 1:4) {
  info_gini_i <- info_gain_gini(playData, i)
  info_Entropy_i <- info_gain_Entropy(playData, i)
  info_gain_mat[i, ] <- c(info_gini_i, info_Entropy_i)
}

info_gain_mat

# parent Ȯ��
gini_index_one(playData, 5)
Entropy_one(playData, 5)

# child Ȯ��
Entropy_after_split(playData, 1)
gini_index_after_split(playData, 1)


## ����2. ------------------------------------------------------------------------------------------------
## 2.1 Heart �����ͷ� classification
# �ڵ� ������ �Ʒ� ��ũ�� �۵��� �����Ͽ����ϴ�.
# [����] http://www.dodomira.com/2016/07/19/r-%EC%9D%98%EC%82%AC%EA%B2%B0%EC%A0%95%EB%82%98%EB%AC%B4-%EA%B9%94%EB%81%94%ED%95%98%EA%B2%8C-plotting-%ED%95%98%EA%B8%B0-fancyrpartplot-r/
# [����] https://thebook.io/006723/ch09/03/03/06/
HeartData <- read.csv('http://www-bcf.usc.edu/~gareth/ISL/Heart.csv')
str(HeartData)
summary(HeartData)

# �з��� ���� �� ��, confusion matrix�� ����µ� �� �� ���庴�� �ִ� "Yes"�� �� �߿��ϰ� 
# �ؼ��ϱ� ������, factor���� level�� ������ �ٲ��ش�.
AHD <- factor(as.vector(HeartData$AHD), levels=c("Yes", "No"))
HeartData$AHD <- AHD
str(HeartData)

# caret ��Ű�� ����ؼ�, Ʈ���̴� & �׽�Ʈ ������ ����
# Y ���� ������ �Ʒ� �����Ϳ� �׽�Ʈ �������� �и��� �����ϸ� 
# �̵� �Լ��� ����� �и��� �����ʹ� Y ���� ������ ���� �����Ϳ� ���� �����ȴ�.
set.seed(12345) # �������� ���� seed ����
train_idx <- createDataPartition(y = HeartData$AHD, p=0.7, list=FALSE)  # y: a vector of outcomes
trnHeart <- HeartData[train_idx, ]
tstHeart <- HeartData[-train_idx, ]

# rpart ��Ű�� Ȱ���Ͽ� �ǻ�������� ���� �� plotting (Full tree)
set.seed(12345)
fullTree_Heart <- rpart(AHD ~ . , data = trnHeart, method = "class", minsplit = 2, minbucket = 1, cp=-1)
plot(fullTree_Heart)
text(fullTree_Heart)
fullTree_Heart  # leaf nodes�� ����, Full tree���� Ȯ���� �� �ִ�.

# fullTree���� Ȯ���ϴ� ��� 2��°_ confusion matrix Ȯ��
fullTree_predict_trnData <- predict(fullTree_Heart, trnHeart, type='class')
confusionMatrix(fullTree_predict_trnData, trnHeart$AHD)


# pruning
# k-fold Cross-validation ����� ����ؼ� train���� ������ �ɰ���
# �׽�Ʈ �� ���� �л��� ���� ���� ������ ���� �����ϸ� �ȴ�
# rpart��Ű�������� cv.tree�� �����ϰ� cross-validation�� ����� �ִ� �Լ��� print.cp�� �������ش�.
# [����] http://www.rpubs.com/jmhome/DecisionTree
set.seed(12345)
printcp(fullTree_Heart)
plotcp(fullTree_Heart)
?printcp
fullTree_Heart$cptable

xerror_min_idx <- which.min(fullTree_Heart$cptable[,"xerror"])  # xerror

# xerror�� ���� ���� split ������ ����
pruneTree_Heart <- prune(fullTree_Heart, cp = fullTree_Heart$cptable[xerror_min_idx,"CP"])
# cp = complexity parameter

plot(pruneTree_Heart)
text(pruneTree_Heart, pretty=0)
pruneTree_Heart

# ����, �� ��
# full tree
fullTree_predict <- predict(fullTree_Heart, tstHeart, type='class')
confusionMatrix(fullTree_predict, tstHeart$AHD)

# prunce tree
pruneTree_predict <- predict(pruneTree_Heart, tstHeart, type='class')
confusionMatrix(pruneTree_predict, tstHeart$AHD)


## �߰�_Party package�� Ʈ�� ������ ���� ��
partyTree <- ctree(AHD~., data = trnHeart)
plot(partyTree)

# party��Ű���� ����ġ�⸦ significance�� ����ؼ� �ϱ� ������ ������ pruning ������ �ʿ����.
partypred <- predict(partyTree, tstHeart)
confusionMatrix(partypred, tstHeart$AHD) 




## 2.2 ----- �����ͷ� Regression
# Performance evaluation function for regression --------------------------
perf_eval_reg <- function(tgt_y, pre_y){  # input: ���� ����, ���� ����
  
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
  
}

perf_mat <- matrix(0, nrow = 3, ncol = 3)
rownames(perf_mat) <- c("Full tree(rpart package)", "Pruned tree(rpart package)", "MLR")
colnames(perf_mat) <- c("RMSE", "MAE", "MAPE")
perf_mat

data(iris)
str(iris)

set.seed(12345) # �������� ���� seed ����
train_idx <- createDataPartition(y = iris$Sepal.Length, p=0.7, list=FALSE)
iris_trn_data <- iris[train_idx, ]
iris_tst_data <- iris[-train_idx, ]

## full tree(rpart package)
set.seed(12345)
fullTree_iris <- rpart(Sepal.Length ~ . , data = iris_trn_data, method = "anova", minsplit = 2, minbucket = 1, cp=-1)

plot(fullTree_iris)
text(fullTree_iris)
fullTree_iris


# Performance Measure
fullTree_sepalLength <- predict(fullTree_iris, newdata = iris_tst_data)
perf_mat[1,] <- perf_eval_reg(iris_tst_data$Sepal.Length, fullTree_sepalLength)
perf_mat


## Pruned tree(rpart package)
set.seed(12345)
printcp(fullTree_iris)
plotcp(fullTree_iris)
?printcp

fullTree_iris$cptable

xerror_min_idx <- which.min(fullTree_iris$cptable[,"xerror"])  # xerror

# xerror�� ���� ���� split ������ ����
pruneTree_iris <- prune(fullTree_iris, cp = fullTree_iris$cptable[xerror_min_idx,"CP"])
# cp = complexity parameter

plot(pruneTree_iris)
text(pruneTree_iris, pretty=0)
pruneTree_iris

# Performance Measure
prunedTree_sepalLength <- predict(pruneTree_iris, newdata = iris_tst_data)
perf_mat[2,] <- perf_eval_reg(iris_tst_data$Sepal.Length, prunedTree_sepalLength)
perf_mat



## MLR�� ��  -------------------------------
mlr_iris <- lm(Sepal.Length ~ ., data = iris_trn_data)  # lm = linear model / �������� = ����
mlr_iris
summary(mlr_iris)

# Performance Measure
mlr_iris_sepalLength <- predict(mlr_iris, newdata = iris_tst_data)  # hat y�� ������ �����Ϳ� ���� ������

perf_mat[3,] <- perf_eval_reg(iris_tst_data$Sepal.Length, mlr_iris_sepalLength)
perf_mat




## ����4. ------------------------------------------------------------------------------------------------
# ���� ������Ʈ_ ����Ʈ���� �ӻ��
install.packages("randomForest")
library(randomForest)

## ���� ������Ʈ_ classification ------------------------
set.seed(12345)
randomForest_iris <- randomForest(Sepal.Length ~ ., data = iris_trn_data)

# ��� Ȯ��
print(randomForest_iris)

# performance �� ��� �����
perf_mat_RF <- matrix(0, nrow = 3, ncol = 3)
rownames(perf_mat_RF) <- c("Random Forest", "Pruned tree(rpart package)", "MLR")
colnames(perf_mat_RF) <- c("RMSE", "MAE", "MAPE")
perf_mat_RF

# ���� �� ��� ��
RF_iris_sepalLength <- predict(randomForest_iris, newdata = iris_tst_data)

perf_mat_RF[1,] <- perf_eval_reg(iris_tst_data$Sepal.Length, RF_iris_sepalLength)
perf_mat_RF[2,] <- perf_mat[2,]  # Pruned tree(rpart package)
perf_mat_RF[3,] <- perf_mat[3,]  # MLR
perf_mat_RF


## ���� ������Ʈ_ regression ------------------------
# trnHeart���� NA�ִ� �� �����ϱ�_ randomForest() �Լ������� NA�� �ִ� ���� ������, ���� �߻�
row.has.na <- apply(trnHeart, 1, function(x){any(is.na(x))})
trnHeart_no_NA <- trnHeart[!row.has.na,]
nrow(trnHeart)
nrow(trnHeart_no_NA)

set.seed(12345)
randomForest_Heart <- randomForest(AHD ~ ., data = trnHeart_no_NA)

# ��� Ȯ��
print(randomForest_Heart)   # confisuion Matrix �� �� ����

# ����
RF_Heart_AHD <- predict(randomForest_Heart, newdata = tstHeart)

# performance ��
confusionMatrix(RF_Heart_AHD, tstHeart$AHD)
