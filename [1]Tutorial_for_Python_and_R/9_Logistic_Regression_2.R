# Performance Evaluation Function -----------------------------------------
perf_eval2 <- function(cm){
  
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


# Initialize the performance matrix
perf_mat <- matrix(0, 1, 6)
colnames(perf_mat) <- c("TPR (Recall)", "Precision", "TNR", "ACC", "BCR", "F1")
rownames(perf_mat) <- "Logstic Regression"


# Load dataset
setwd('C:/Users/KYY/Desktop/ML_Ato_Z/[0]Data')
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

input_idx <- c(1, 2)
target_idx <- 3


# Conduct the normalization
dataset_input <- dataset[,input_idx]
dataset_input <- scale(dataset_input, center = TRUE, scale = TRUE)
dataset_target <- dataset[,target_idx]
dataset <- data.frame(dataset_input, dataset_target)


# Split the data into the training/validation sets
set.seed(12345)
trn_idx <- sample(1:nrow(dataset), round(0.7*nrow(dataset)))
dataset_trn <- dataset[trn_idx,]
dataset_tst <- dataset[-trn_idx,]


# Train the Logistic Regression Model with all variables
# glm: generalized linear model
# 로지스틱 회귀분석은 왜 선형분석인가?
# 분류를 선형으로 하기 때문에
full_lr <- glm(dataset_target ~ ., family=binomial, dataset)
# family=binomial 로지스틱 회귀분석을 하는 옵션
# 사용자의 자유도가 개입된 옵션이 없어서 validation set이 없다. -- 지정해주어야하는 옵션값이 없다.
summary(full_lr)

lr_response <- predict(full_lr, type = "response", newdata = dataset_tst)  # response: 성공범주로 속할 확률값 계산
lr_target <- dataset_tst$dataset_target
lr_predicted <- rep(0, length(lr_target))
lr_predicted[which(lr_response >= 0.5)] <- 1
cm_full <- table(lr_target, lr_predicted)
cm_full

perf_mat[1,] <- perf_eval2(cm_full)
perf_mat
