# Multiple Linear Regression

# 패키지 설치
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

## 문제1. ------------------------------------------------------------------------------------------------
# 첨부된 Playdata.csv에서 변수 별 (Outlook, Temperature, Huminidity, Wind)로 
# Information Gain을 계산하시오. (Entropy, Gini-index 각각)
setwd("C:/Users/EnglishKYY/Documents/Multivariative Statistical Analytics_data")
playData <- read.csv("Playdata.csv")

str(playData)
summary(playData)
head(playData)

# 분포확인
table(playData$Outlook)
table(playData$Temperature)
table(playData$Humidity)
table(playData$Wind)
table(playData$Play)

# 1.2 결정 트리 만들기
# Performance Evaluation Function -----------------------------------------
# 2범주 분류에서의 계산식
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

set.seed(12345)  # 재현성을 위해

playData_x <- playData[,-c(5)]  # 5번째는 종속변수(play)
playData_y <- as.data.frame(as.factor(playData[,5]))  # party 패키지에서는 분류를 하고 싶으면, 종속변수가 factor형이어야 함.
str(playData_y)
summary(playData)

##### gini_index, Entropy 계산하는 함수 구현 ---------------------------------------
# input: 데이터셋(data.frame), 열번호 
# output: 해당 열 번호 기준으로 gini index 구해서 출력
##### 함수명_one은 데이터셋이 split되기 전의 지표값
##### 함수명_all은 데이터셋이 split된 후, 전체에 대한 지표값

gini_index_one <- function(dataset, col_idx) {  # input: 데이터셋(data.frame), 열번호
  tmp_table <- table(dataset[,col_idx])  # Outlook
  total <- sum(tmp_table)
  
  variable_level <- length(tmp_table)  # 해당 변수의 level
  
  gini_index <- 0
  # pi^2 전부 더해주기
  for(i in 1:variable_level) {  
    gini_index <- gini_index + (tmp_table[i]/total)^2
  }
  
  gini_index <- 1 - gini_index
  # answer_gini <- paste( names(dataset)[col_idx], "기준의 gini index =", as.character(gini_index) )
  # print(answer_gini)
  gini_index <- as.numeric(gini_index)
  return(gini_index)
}


Entropy_one <- function(dataset, col_idx) {  # input: 데이터셋(data.frame), 열번호
  tmp_table <- table(dataset[,col_idx])  # Outlook
  total <- sum(tmp_table)
  variable_level <- length(tmp_table)  # 해당 변수의 level
  
  Entropy <- 0
  for(i in 1:variable_level) {  
    tmp_prob <- tmp_table[i]/total
    if (tmp_prob != 0) {
      # [디버깅] 이 확률이 0일 때에 대해서는 계산 안하고 넘어가는 것으로 처리
      # log2(0) = -Inf 이므로, 실제 엔트로피 계산에서 0 * (-Inf) = 0으로 처리한다
      # 하지만 R에서는 그렇게 계산할 경우, 0이 아니라 NaN을 반환해주므로 따로 처리한다.
      Entropy <- Entropy - ( (tmp_table[i]/total) * log2(tmp_table[i]/total) )      
    }
  }
  
  # answer_entropy <- paste( names(dataset)[col_idx], "기준의 Entropy =", as.character(Entropy) )
  # print(answer_entropy)
  Entropy <- as.numeric(Entropy)
  return(Entropy)
}


gini_index_after_split <- function(dataset, col_idx) {  # 해당 col_idx 기준으로 split 후, 전체에 대한 gini index
  # dataset을 해당 col_idx의 level별로 쪼갠 후
  # 각각에 대해 gini_index_one 함수에 넣고
  # 각각의 크기별로 Ri값 곱해서
  # 전부 합하면, 이 함수의 반환값을 구할 수 있다.
  
  # level마다 Ri * gini index 구하기
  tmp_table <- table(dataset[,col_idx])
  total <- sum(tmp_table)
  tmp_levels <- levels(dataset[,col_idx])  # 변수의 level
  
  gini_index_total <- 0
  for(i in 1:length(tmp_levels)) {  # 변수의 레벨 별로
    # Ri 구하기
    tmp_r <- tmp_table[i] / total
    
    # tmp_p 구하기
    subset_data <- subset(dataset, dataset[,col_idx] == tmp_levels[i])  # 해당 데이터셋만 추출
    tmp_gini <- gini_index_one(subset_data, 5)  # 추출한 데이터셋 내에서 종속변수인 'Play' 기준 gini_index 계산
    gini_index_total <- gini_index_total + ( tmp_r * tmp_gini)
  }
  
  # answer_entropy <- paste("split 후", names(dataset)[5], "기준의 gini index =", as.character(gini_index_total) )
  # print(answer_entropy)
  gini_index_total <- as.numeric(gini_index_total)
  return(gini_index_total)
}


Entropy_after_split <- function(dataset, col_idx) {  # 해당 col_idx 기준으로 split 후, 전체에 대한 Entropy
  # dataset을 해당 col_idx의 level별로 쪼갠 후
  # 각각에 대해 Entropy_one 함수에 넣고
  # 각각의 크기별로 Ri값 곱해서
  # 전부 합하면, 이 함수의 반환값을 구할 수 있다.
  
  # level마다 Ri * (각각의 Entropy) 구하기
  tmp_table <- table(dataset[,col_idx])
  total <- sum(tmp_table)
  tmp_levels <- levels(dataset[,col_idx])  # 변수의 level
  
  Entropy_total <- 0
  for(i in 1:length(tmp_levels)) {  # 변수의 레벨 별로
    # Ri 구하기
    tmp_r <- tmp_table[i] / total
    
    # tmp_p 구하기
    subset_data <- subset(dataset, dataset[,col_idx] == tmp_levels[i])  # 해당 데이터셋만 추출
    tmp_entropy <- Entropy_one(subset_data, 5)  # 추출한 데이터셋 내에서 종속변수인 'Play' 기준 gini_index 계산
    Entropy_total <- Entropy_total + ( tmp_r * tmp_entropy)
  }
  
  # answer_entropy <- paste("split 후", names(dataset)[5], "기준의 gini index =", as.character(gini_index_total) )
  # print(answer_entropy)
  Entropy_total <- as.numeric(Entropy_total)
  return(Entropy_total)
}

### information_gain 구하는 함수
info_gain_gini <- function(dataset, col_idx) {
  before <- gini_index_one(dataset, 5)   # 종속변수 기준
  after <- gini_index_after_split(dataset, col_idx)
  info_gain <- before - after
  return(info_gain)
}

info_gain_Entropy <- function(dataset, col_idx) {
  before <- Entropy_one(dataset, 5)    # 종속변수 기준
  after <- Entropy_after_split(dataset, col_idx)
  info_gain <- before - after
  return(info_gain)
}


##### --------------------------------------------------------------------------
# infomation gain 구하기
names(playData)
info_gain_mat <- matrix(0, 4, 2)
colnames(info_gain_mat) <- c("gini index", "Entropy")
rownames(info_gain_mat) <- c("Outlook", "Temperature", "Humidity", "Wind")
info_gain_mat

# 변수 별 information gain 구하기
for(i in 1:4) {
  info_gini_i <- info_gain_gini(playData, i)
  info_Entropy_i <- info_gain_Entropy(playData, i)
  info_gain_mat[i, ] <- c(info_gini_i, info_Entropy_i)
}

info_gain_mat

# parent 확인
gini_index_one(playData, 5)
Entropy_one(playData, 5)

# child 확인
Entropy_after_split(playData, 1)
gini_index_after_split(playData, 1)


## 문제2. ------------------------------------------------------------------------------------------------
## 2.1 Heart 데이터로 classification
# 코드 진행은 아래 링크의 글들을 참고하였습니다.
# [참고] http://www.dodomira.com/2016/07/19/r-%EC%9D%98%EC%82%AC%EA%B2%B0%EC%A0%95%EB%82%98%EB%AC%B4-%EA%B9%94%EB%81%94%ED%95%98%EA%B2%8C-plotting-%ED%95%98%EA%B8%B0-fancyrpartplot-r/
# [참고] https://thebook.io/006723/ch09/03/03/06/
HeartData <- read.csv('http://www-bcf.usc.edu/~gareth/ISL/Heart.csv')
str(HeartData)
summary(HeartData)

# 분류모델 성능 평가 시, confusion matrix를 만드는데 이 때 심장병이 있는 "Yes"를 더 중요하게 
# 해석하기 때문에, factor에서 level의 순서를 바꿔준다.
AHD <- factor(as.vector(HeartData$AHD), levels=c("Yes", "No"))
HeartData$AHD <- AHD
str(HeartData)

# caret 패키지 사용해서, 트레이닝 & 테스트 데이터 구분
# Y 값을 고려한 훈련 데이터와 테스트 데이터의 분리를 지원하며 
# 이들 함수를 사용해 분리한 데이터는 Y 값의 비율이 원본 데이터와 같게 유지된다.
set.seed(12345) # 재현성을 위해 seed 설정
train_idx <- createDataPartition(y = HeartData$AHD, p=0.7, list=FALSE)  # y: a vector of outcomes
trnHeart <- HeartData[train_idx, ]
tstHeart <- HeartData[-train_idx, ]

# rpart 패키지 활용하여 의사결정나무 만든 후 plotting (Full tree)
set.seed(12345)
fullTree_Heart <- rpart(AHD ~ . , data = trnHeart, method = "class", minsplit = 2, minbucket = 1, cp=-1)
plot(fullTree_Heart)
text(fullTree_Heart)
fullTree_Heart  # leaf nodes을 보면, Full tree임을 확인할 수 있다.

# fullTree인지 확인하는 방법 2번째_ confusion matrix 확인
fullTree_predict_trnData <- predict(fullTree_Heart, trnHeart, type='class')
confusionMatrix(fullTree_predict_trnData, trnHeart$AHD)


# pruning
# k-fold Cross-validation 방법을 사용해서 train셋을 여러번 쪼개서
# 테스트 한 다음 분산이 가장 낮은 가지의 수를 선택하면 된다
# rpart패키지에서는 cv.tree와 유사하게 cross-validation을 계산해 주는 함수로 print.cp를 제공해준다.
# [참고] http://www.rpubs.com/jmhome/DecisionTree
set.seed(12345)
printcp(fullTree_Heart)
plotcp(fullTree_Heart)
?printcp
fullTree_Heart$cptable

xerror_min_idx <- which.min(fullTree_Heart$cptable[,"xerror"])  # xerror

# xerror이 가장 낮은 split 개수를 선택
pruneTree_Heart <- prune(fullTree_Heart, cp = fullTree_Heart$cptable[xerror_min_idx,"CP"])
# cp = complexity parameter

plot(pruneTree_Heart)
text(pruneTree_Heart, pretty=0)
pruneTree_Heart

# 예측, 모델 평가
# full tree
fullTree_predict <- predict(fullTree_Heart, tstHeart, type='class')
confusionMatrix(fullTree_predict, tstHeart$AHD)

# prunce tree
pruneTree_predict <- predict(pruneTree_Heart, tstHeart, type='class')
confusionMatrix(pruneTree_predict, tstHeart$AHD)


## 추가_Party package로 트리 만들어보고 성능 비교
partyTree <- ctree(AHD~., data = trnHeart)
plot(partyTree)

# party패키지는 가지치기를 significance를 사용해서 하기 때문에 별도의 pruning 과정이 필요없다.
partypred <- predict(partyTree, tstHeart)
confusionMatrix(partypred, tstHeart$AHD) 




## 2.2 ----- 데이터로 Regression
# Performance evaluation function for regression --------------------------
perf_eval_reg <- function(tgt_y, pre_y){  # input: 정답 벡터, 추정 벡터
  
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

set.seed(12345) # 재현성을 위해 seed 설정
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

# xerror이 가장 낮은 split 개수를 선택
pruneTree_iris <- prune(fullTree_iris, cp = fullTree_iris$cptable[xerror_min_idx,"CP"])
# cp = complexity parameter

plot(pruneTree_iris)
text(pruneTree_iris, pretty=0)
pruneTree_iris

# Performance Measure
prunedTree_sepalLength <- predict(pruneTree_iris, newdata = iris_tst_data)
perf_mat[2,] <- perf_eval_reg(iris_tst_data$Sepal.Length, prunedTree_sepalLength)
perf_mat



## MLR과 비교  -------------------------------
mlr_iris <- lm(Sepal.Length ~ ., data = iris_trn_data)  # lm = linear model / 설명변수 = 종속
mlr_iris
summary(mlr_iris)

# Performance Measure
mlr_iris_sepalLength <- predict(mlr_iris, newdata = iris_tst_data)  # hat y는 검증용 데이터에 대한 추정값

perf_mat[3,] <- perf_eval_reg(iris_tst_data$Sepal.Length, mlr_iris_sepalLength)
perf_mat




## 문제4. ------------------------------------------------------------------------------------------------
# 랜덤 포레스트_ 결정트리의 앙상블
install.packages("randomForest")
library(randomForest)

## 랜덤 포레스트_ classification ------------------------
set.seed(12345)
randomForest_iris <- randomForest(Sepal.Length ~ ., data = iris_trn_data)

# 결과 확인
print(randomForest_iris)

# performance 비교 행렬 만들기
perf_mat_RF <- matrix(0, nrow = 3, ncol = 3)
rownames(perf_mat_RF) <- c("Random Forest", "Pruned tree(rpart package)", "MLR")
colnames(perf_mat_RF) <- c("RMSE", "MAE", "MAPE")
perf_mat_RF

# 예측 후 결과 비교
RF_iris_sepalLength <- predict(randomForest_iris, newdata = iris_tst_data)

perf_mat_RF[1,] <- perf_eval_reg(iris_tst_data$Sepal.Length, RF_iris_sepalLength)
perf_mat_RF[2,] <- perf_mat[2,]  # Pruned tree(rpart package)
perf_mat_RF[3,] <- perf_mat[3,]  # MLR
perf_mat_RF


## 랜덤 포레스트_ regression ------------------------
# trnHeart에서 NA있는 행 제거하기_ randomForest() 함수에서는 NA가 있는 행이 있으면, 에러 발생
row.has.na <- apply(trnHeart, 1, function(x){any(is.na(x))})
trnHeart_no_NA <- trnHeart[!row.has.na,]
nrow(trnHeart)
nrow(trnHeart_no_NA)

set.seed(12345)
randomForest_Heart <- randomForest(AHD ~ ., data = trnHeart_no_NA)

# 결과 확인
print(randomForest_Heart)   # confisuion Matrix 볼 수 잇음

# 예측
RF_Heart_AHD <- predict(randomForest_Heart, newdata = tstHeart)

# performance 비교
confusionMatrix(RF_Heart_AHD, tstHeart$AHD)

