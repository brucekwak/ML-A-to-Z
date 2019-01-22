# Linear Regression

## -------------------------------------------------------------------------------------------------------
# 필요한 패키지 설치
install.packages("MASS")
library(MASS)


## 1. 다중선형회귀------------------------------------------------------------------------------------

#----- 데이터 준비 및 탐색적 데이터 분석 -----#
# birthwt 데이터 불러오기
data(birthwt)

### 1.1 범주형 변수에 대한 전처리 진행
## 1.1.1 데이터 확인
str(birthwt)
summary(birthwt)
head(birthwt)

nObject <- nrow(birthwt)  # 관측치 개수
nVar <- ncol(birthwt)   # 변수 개수


## 1.1.2 결측치(missing value) 확인
apply(birthwt, 2, function(x) sum(is.na(x)))   # 2: 열 단위로 연산_ NA가 있을경우, sum 연산이 안되므로 NA가 출력된다

###########################################################################################
## 1.1.3 변수별 데이터 분포 확인
names(birthwt)
table(birthwt$low)

hist(birthwt$age, main="birthwt_Age", xlab="age", col="#80fd3d",
     ylim=c(0, 70))

hist(birthwt$lwt, main="birthwt_lwt", xlab="mother_weight(pound)", col="#80fd3d",
     xlim=c(70, 270), ylim=c(0, 70), breaks=seq(70, 270, 20))

table(birthwt$race)   # 1: White, 2: Black, 3: Other

table(birthwt$smoke)

table(birthwt$ptl)  # 산모의 조산 경험 수

table(birthwt$ht)

table(birthwt$ui)

table(birthwt$ftv)   # 임신 초기 3개월 동안 방문한 의사의 수

hist(birthwt$bwt, main="bwt", xlab="birth_weight(gram)", col="#80fd3d")


## 1.1.4 범주형 변수 전처리
# 범주형 변수를 binary 변수로 바꿔주기_ race(4)
dummy_White <- rep(0,nObject)
dummy_Black <- rep(0,nObject)
dummy_Other <- rep(0,nObject)

White_idx <- which(birthwt[,4] == "1")
Black_idx <- which(birthwt[,4] == "2")
Other_idx <- which(birthwt[,4] == "3")

dummy_White[White_idx] <- 1
dummy_Black[Black_idx] <- 1
dummy_Other[Other_idx] <- 1

race <- data.frame(dummy_White, dummy_Black, dummy_Other) # 열이 3개인 데이터프레임
names(race) <- c("White", "Black", "Other")
str(race)

# 범주형 데이터 전처리한 것 넣어주기
birthwt<- cbind(birthwt[, -c(4)], race)
str(birthwt)

### 1.2 다중공선성 파악
## 1.2.1 VIF 확인
install.packages("car")
library(car)

model <- lm(bwt ~ . , data=birthwt)
summary(model)
vif(model)   # 에러: there are aliased coefficients in the model
# aliased coefficients 에러는 범주형 변수인 race를 가변수 함수로 바꿔주었을 때, 완벽한 선형관계(x1 + x2+ x3 = 1를 이루기 때문이다.
# 한 변수는 다른 두 변수에 의해 완벽하게 설명될 수 있으므로, 하나의 열을 삭제한 후 진행하도록 한다.
birthwt_no_Other <- birthwt[,-c(12)]
str(birthwt_no_Other)

model <- lm(bwt ~ . , data=birthwt_no_Other)
vif(model)   # 에러: there are aliased coefficients in the model
vif(model) > 10  # 다중 공선성 확인
summary(model)


### 1.3 데이터가 정규분포 따르는지 파악
# 예측하고자하는 변수인 birthwt$bwt
m <- mean(birthwt$bwt)  # 잔차의 평균
std <- sqrt(var(birthwt$bwt))  # 잔차의 표준편차 ( 분산에 루트씌우면 표준편차 )

hist(birthwt$bwt, density=20, breaks=50, prob=TRUE, 
     xlab="bwt", main="normal curve over histogram")
# breaks = a single number giving the number of cells for the histogram. 막대 개수
# density: 막대 채우는 음영

# dnorm() 함수로 x값을 만들어 냄. 앞서 구한 평균과 표준편차로 정규분포 곡선을 만들어냄
curve(dnorm(x, mean=m, sd=std), col="darkblue", lwd=2, add=TRUE, yaxt="n")

# 잔차의 정규성 판단하는 정량적인 지표 (왜도와 첨도)
# 정규분포의 skewness = 0   => 중심이 좌우로 치우치진 않았는지
# 정규분포의 kurtosis = 3   => 분포가 중앙에 몰려있지 않은지
install.packages("moments")   # 정규분포 정규성 판단하는. 첨도
library(moments)

skewness(birthwt$bwt)  # 왜도
kurtosis(birthwt$bwt)   # 첨도

# QQplot
qqnorm(birthwt$bwt)
qqline(birthwt$bwt)

### 1.4 모델을 피팅하고 (bwt를 y로 둘 것), 각자 결과에 대하여 서술하시오.
# training data(80%)와 validation data(20%)로 나누기
set.seed(1000)  # 재현성을 위해 seed 설정
birthwt_trn_idx <- sample(1:nObject, round(0.8*nObject))   # 80%만 뽑는다
birthwt_trn_data <- birthwt[birthwt_trn_idx,]  # training 데이터
birthwt_val_data <- birthwt[-birthwt_trn_idx,]   # validation 데이터

names(birthwt_trn_data)
# Train the MLR
# Formula_   Y (target variable) ~ X (Input variables)
# 회귀 계수 구하기. Price = Target variable / . = 종속 변수 제외하고 나머지 모든 변수를 input variable로
mlr_birthwt <- lm(bwt ~ ., data = birthwt_trn_data)  # lm = linear model / 설명변수 = 종속
mlr_birthwt
summary(mlr_birthwt)

# 잔차의 정규성 검정
birthwt_resid <- resid(mlr_birthwt)

m <- mean(birthwt_resid)  # 잔차의 평균
std <- sqrt(var(birthwt_resid))  # 잔차의 표준편차 ( 분산에 루트씌우면 표준편차 )

hist(birthwt_resid, density=20, breaks=50, prob=TRUE, 
     xlab="x-variable", main="normal curve over histogram")
# breaks = a single number giving the number of cells for the histogram. 막대 개수
# density: 막대 채우는 음영

# dnorm() 함수로 x값을 만들어 냄. 앞서 구한 평균과 표준편차로 정규분포 곡선을 만들어냄
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n") 

# 샤피로 윌크 검정Shapiro-Wilk Test은 표본이 정규 분포로부터 추출된 것인지 테스트하기 위한 방법이다. 
# 검정은 shapiro.test( ) 함수를 사용하며 이때 귀무가설은 주어진 데이터가 정규 분포로부터의 표본이라는 것이다.
shapiro.test(birthwt_resid)


### 1.5 실제 y값(y축)과 예측 y값(x축)을 plotting하세요.
plot(fitted(mlr_birthwt), birthwt_trn_data$bwt,
     xlim = c(1000,5000), ylim = c(1000,5000))
abline(0,1,lty=3) # x = y 직선


### 1.6 피팅한 모델의 RMSE, MAE, MAPE를 산출하세요
# Performance evaluation function for regression
perf_eval_reg <- function(tgt_y, pre_y){  # input: 정답 벡터, 추정 벡터
  
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
}

# performance matrix 만들기
perf_mat <- matrix(0, nrow = 1, ncol = 3)

# Initialize a performance summary
rownames(perf_mat) <- c("birthwt")
colnames(perf_mat) <- c("RMSE", "MAE", "MAPE")
perf_mat

mlr_birthwt_predict <- predict(mlr_birthwt, newdata = birthwt_val_data)

perf_mat[1,] <- perf_eval_reg(birthwt_val_data$bwt, mlr_birthwt_predict)
perf_mat



## 2. 로지스틱 회귀------------------------------------------------------------------------------------
install.packages('functional')
install.packages('ROCR')

library(functional)
library(ROCR)

### 2.0 데이터 전처리 및 간단한 데이터 탐색
# 전처리 코드 출처(https://m.blog.naver.com/PostView.nhn?blogId=hancury&logNo=220826864995&proxyReferer=&proxyReferer=https%3A%2F%2Fwww.google.co.kr%2F)
# 전처리 코드는 '출처'의 코드와 같으며, 주석은 일부 추가하였습니다.
data(Titanic)
str(Titanic)
dim(Titanic)
Titanic

pivot.titanic <- as.data.frame(Titanic)  # 피벗 요약 테이블 형태에서 1명당 1행을 가진 형태로 변형
summary(pivot.titanic)

# 총 관측치 개수
sum(pivot.titanic$Freq)

# 데이터셋을 피벗 요약 테이블 형태에서 1명당 1행을 가진 형태로 변형
temp <- rep(as.character(pivot.titanic[1,]$Class), 5)  # 참고

titanic.class <- c()
titanic.sex <- c()
titanic.age <- c()
titanic.survived <- c()

for(i in 1:nrow(pivot.titanic)) {
  freq <- pivot.titanic[i,]$Freq
  temp_class <- rep( as.character(pivot.titanic[i,]$Class), freq )
  temp_sex <- rep( as.character(pivot.titanic[i,]$Sex), freq )
  temp_age <- rep( as.character(pivot.titanic[i,]$Age), freq )
  temp_survived <- rep( as.character(pivot.titanic[i,]$Survived), freq )
  
  titanic.class <- c(titanic.class, temp_class)
  titanic.sex <- c(titanic.sex, temp_sex)
  titanic.age <- c(titanic.age, temp_age)
  titanic.survived <- c(titanic.survived, temp_survived)
}

# 데이터 프레임 형태로 합치기
titanic <- data.frame(
  Class=titanic.class, 
  Sex=titanic.sex, 
  Age=titanic.age, 
  Survived=titanic.survived)

head(titanic)
tail(titanic)

# 탐색적 데이터 분석
summary(titanic)   # factor형이어서 범주별 분포 분석이 용이하다

## 범주형 데이터 -> dummy variable(가변수)로 변환해주기
# factor -> String
# titanic$Class <- as.character(titanic$Class)
# titanic$Sex <- as.character(titanic$Sex)
# titanic$Age <- as.character(titanic$Age)
# titanic$Survived <- as.character(titanic$Survived)

head(titanic)

# Sex, Age, Survived를 binary variable로 변환
nObject <- nrow(titanic)
SexMale <- rep(0,nObject)
AgeChild <- rep(0,nObject)
SurvivedYes <- rep(0,nObject)

male_idx <- which(titanic[,2] == "Male")
child_idx <- which(titanic[,3] == "Child")
survived_yes_idx <- which(titanic[,4] == "Yes")

SexMale[male_idx] <- 1
AgeChild[child_idx] <- 1
SurvivedYes[survived_yes_idx] <- 1
sexAgeSurvived <- data.frame(SexMale, AgeChild, SurvivedYes)
names(sexAgeSurvived) <- c("SexMale", "AgeChild", "SurvivedYes")

# class를 dummy variable로 변환
dummy_1st <- rep(0,nObject)
dummy_2nd <- rep(0,nObject)
dummy_3rd <- rep(0,nObject)
dummy_Crew <- rep(0,nObject)

dummy_1st_idx <- which(titanic[,1] == "1st")
dummy_2nd_idx <- which(titanic[,1] == "2nd")
dummy_3rd_idx <- which(titanic[,1] == "3rd")
dummy_Crew_idx <- which(titanic[,1] == "Crew")

dummy_1st[dummy_1st_idx] <- 1
dummy_2nd[dummy_2nd_idx] <- 1
dummy_3rd[dummy_3rd_idx] <- 1
dummy_Crew[dummy_Crew_idx] <- 1

Class <- data.frame(dummy_1st, dummy_2nd, dummy_3rd, dummy_Crew) # 열이 4개인 데이터프레임
names(Class) <- c("Class_1st", "Class_2nd", "Class_3rd", "Class_Crew")

# 앞서 만든 두 데이터프레임 합쳐서 titanic 만들기
titanic <- cbind(sexAgeSurvived, Class)
names(titanic)
summary(titanic)
head(titanic)

### 2.1 변수 선택법을 이용하여 logistic regression을 수행하세요.
# performance evaluation function
perf_eval <- function(cm){  # confusion matrix를 인풋으로 받음
  
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

Perf_Table <- matrix(0, nrow = 8, ncol = 6)
rownames(Perf_Table) <- c("All", "Forward", "Backward", "Stepwise", "GA", "Ridge", "Lasso", "Elastic Net")
colnames(Perf_Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")
Perf_Table

names(titanic)
# 1: Idx, 5: Class_1st(Class_1st ~ Class_Crew는 완벽한 선형관계를 이루기 때문에, 다중공선성 문제가 있다. 따라서, 하나의 변수를 제거해줌)
titanic_input <- titanic[,-c(4)]
head(titanic_input)
summary(titanic_input)
names(titanic_input)


# training data(80%)와 validation data(20%)로 나누기
set.seed(1000)  # 재현성을 위해 seed 설정
nObject <- nrow(titanic_input)
titanic_trn_idx <- sample(1:nObject, round(0.8*nObject))   # 80%만 뽑는다
titanic_trn_data <- titanic_input[titanic_trn_idx,]  # training 데이터
titanic_val_data <- titanic_input[-titanic_trn_idx,]   # validation 데이터


## Variable selection method 1: Logistic Regression with all variables
# glm: generalized linear model, formula: Ploan_target이 종속변수, 나머지는 전부 , family=binomial -> 로지스틱 회귀분석
# full_model <- glm(Survived ~ ., family=binomial, titanic_trn_data)
full_model <- glm(SurvivedYes ~ ., family = binomial, maxit = 100, titanic_trn_data)
summary(full_model)  # glm() 함수 내에서 다중공선성을 제거해줌

full_model_coeff <- as.matrix(full_model$coefficients, 6, 1)  # full_model의 coefficients(회귀 계수)를 저장
full_model_coeff

# Make prediction
full_model_prob <- predict(full_model, type = "response", newdata = titanic_val_data) #  type = "response" -> 성공 확률을 내어줘라

# 실제 confusion은 이용하냐 아니냐를 반환해주어야하므로, 0 또는 1 값으로 바꿔줌
full_model_prey <- rep(0, nrow(titanic_val_data))
full_model_prey[which(full_model_prob >= 0.5)] <- 1   # 로지스틱 회귀분석 후, cut-off를 0.5로 정하겠다
full_model_cm <- table(titanic_val_data$Survived, full_model_prey)  # 정답의 0,1 정보와 모형의 0,1 정보로 confusion amtrix
full_model_cm

# Peformance evaluation
Perf_Table[1,] <- perf_eval(full_model_cm)
Perf_Table


## Variable selection method 2: Logistic Regression with Forward Selection
# formula를 만드는 과정
names(titanic_trn_data)
tmp_x <- paste(colnames(titanic_trn_data)[-3], collapse=" + ")  # 3열이 종속변수인 "SurvivedYes"
tmp_xy <- paste("SurvivedYes ~ ", tmp_x, collapse = "")
as.formula(tmp_xy)

# forward selection으로 변수 선택
forward_model <- step(glm(SurvivedYes ~ 1, data = titanic_trn_data), 
                      scope = list(upper = as.formula(tmp_xy), lower = SurvivedYes ~ 1), 
                      direction="forward", trace = 1)

summary(forward_model)
forward_model_coeff <- as.matrix(forward_model$coefficients, 6, 1)
forward_model_coeff

# Make prediction
forward_model_prob <- predict(forward_model, type = "response", newdata = titanic_val_data)
forward_model_prey <- rep(0, nrow(titanic_val_data))
forward_model_prey[which(forward_model_prob >= 0.5)] <- 1
forward_model_cm <- table(titanic_val_data$SurvivedYes, forward_model_prey)
forward_model_cm

# Peformance evaluation
Perf_Table[2,] <- perf_eval(forward_model_cm)
Perf_Table


# Variable selection method 3: Backward elimination
as.formula(tmp_xy)
backward_model <- step(full_model, 
                       scope = list(upper = as.formula(tmp_xy), lower = SurvivedYes ~ 1),
                       direction = "backward", trace = 1)
summary(backward_model)
backward_model_coeff <- as.matrix(backward_model$coefficients, 6, 1)
backward_model_coeff

# Make prediction
backward_model_prob <- predict(backward_model, type = "response", newdata = titanic_val_data)
backward_model_prey <- rep(0, nrow(titanic_val_data))
backward_model_prey[which(backward_model_prob >= 0.5)] <- 1
backward_model_cm <- table(titanic_val_data$SurvivedYes, backward_model_prey)
backward_model_cm

# Peformance evaluation
Perf_Table[3,] <- perf_eval(backward_model_cm)
Perf_Table

# Variable selection method 4: Stepwise selection
as.formula(tmp_xy)
stepwise_model <- step(glm(SurvivedYes ~ 1, data = titanic_trn_data), 
                       scope = list(upper = as.formula(tmp_xy), lower = SurvivedYes ~ 1), 
                       direction="both", trace = 1)
summary(stepwise_model)
stepwise_model_coeff <- as.matrix(stepwise_model$coefficients, 6, 1)
stepwise_model_coeff

# Make prediction
stepwise_model_prob <- predict(stepwise_model, type = "response", newdata = titanic_val_data)
stepwise_model_prey <- rep(0, nrow(titanic_val_data))
stepwise_model_prey[which(stepwise_model_prob >= 0.5)] <- 1
stepwise_model_cm <- table(titanic_val_data$SurvivedYes, stepwise_model_prey)
stepwise_model_cm

# Peformance evaluation
Perf_Table[4,] <- perf_eval(stepwise_model_cm)
Perf_Table



### 2.2 위의 변수들을 이용하여 logistic regression을 시행하기 위한 ACC(단순 정확도) 기준
# 최적의 cutoff(threshold)를 탐색하세요. (0.1에서 0.9까지 0.1 간격으로)
# 독립변수 전체 사용한 모델로 테스트.
Perf_Table_cutoff <- matrix(0, nrow = 9, ncol = 1)
rownames(Perf_Table_cutoff) <- c("cut-off = 0.1", "cut-off = 0.2", "cut-off = 0.3", "cut-off = 0.4", "cut-off = 0.5",
                                 "cut-off = 0.6", "cut-off = 0.7", "cut-off = 0.8", "cut-off = 0.9")
colnames(Perf_Table_cutoff) <- c("Accuracy")
Perf_Table_cutoff

# test 할 cut-off
test_cut_off <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

row_idx <- 1
for( i in test_cut_off) {  # cut-off 0.1 ~ 0.9까지
  full_model_prey <- rep(0, nrow(titanic_val_data))
  full_model_prey[which(full_model_prob >= i)] <- 1  # 로지스틱 회귀분석 후, cut-off = i 로 지정
  full_model_cm <- table(titanic_val_data$SurvivedYes, full_model_prey)  # 정답의 0,1 정보와 모형의 0,1 정보로 confusion amtrix
  full_model_cm
  
  # Peformance evaluation
  Perf_Table_cutoff[row_idx,] <- perf_eval(full_model_cm)[4]  # 4번째가 accuracy
  row_idx <- row_idx + 1
}

Perf_Table_cutoff
which(Perf_Table_cutoff == max(Perf_Table_cutoff))  # 6번
Perf_Table_cutoff[6]

max(Perf_Table_cutoff)  # 확인


### 2.3 최적의 파라미터를 이용하여 피팅된 모델의 TPR, precision, TNR, ACC, BCR, F1을 구하세요.
# 결과 테이블 형식 만들기
Perf_Table_final <- matrix(0, nrow = 1, ncol = 6)
rownames(Perf_Table_final) <- c("All")
colnames(Perf_Table_final) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")
Perf_Table_final

# Make prediction_ 위에서 만들어진 full_model을 활용함
full_model_prob <- predict(full_model, type = "response", newdata = titanic_val_data) #  type = "response" -> 성공 확률을 내어줘라

# 실제 confusion은 이용하냐 아니냐를 반환해주어야하므로, 0 또는 1 값으로 바꿔줌
full_model_prey <- rep(0, nrow(titanic_val_data))
full_model_prey[which(full_model_prob >= 0.6)] <- 1   # 로지스틱 회귀분석 후, cut-off를 0.5로 정하겠다
full_model_cm <- table(titanic_val_data$Survived, full_model_prey)  # 정답의 0,1 정보와 모형의 0,1 정보로 confusion amtrix
full_model_cm

# Peformance evaluation
Perf_Table_final[1,] <- perf_eval(full_model_cm)
Perf_Table_final


### 2.3 추가_ F1-measure 기준 최적의 cut-off 찾기(전체 독립변수 선택)
# 최적의 cutoff(threshold)를 탐색하세요. (0.1에서 0.9까지 0.1 간격으로)
# 독립변수 전체 사용한 모델로 테스트.
Perf_Table_cutoff_F1 <- matrix(0, nrow = 9, ncol = 1)
rownames(Perf_Table_cutoff_F1) <- c("cut-off = 0.1", "cut-off = 0.2", "cut-off = 0.3", "cut-off = 0.4", "cut-off = 0.5",
                                    "cut-off = 0.6", "cut-off = 0.7", "cut-off = 0.8", "cut-off = 0.9")
colnames(Perf_Table_cutoff_F1) <- c("F1-measure")
Perf_Table_cutoff_F1

# test 할 cut-off
test_cut_off <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

row_idx <- 1
for( i in test_cut_off) {  # cut-off 0.1 ~ 0.9까지
  full_model_prey <- rep(0, nrow(titanic_val_data))
  full_model_prey[which(full_model_prob >= i)] <- 1  # 로지스틱 회귀분석 후, cut-off = i 로 지정
  full_model_cm <- table(titanic_val_data$SurvivedYes, full_model_prey)  # 정답의 0,1 정보와 모형의 0,1 정보로 confusion amtrix
  full_model_cm
  
  # Peformance evaluation
  Perf_Table_cutoff_F1[row_idx,] <- perf_eval(full_model_cm)[6]  # 6번째가 F1-measure
  row_idx <- row_idx + 1
}

Perf_Table_cutoff_F1
which(Perf_Table_cutoff_F1 == max(Perf_Table_cutoff_F1))
Perf_Table_cutoff_F1[3]

max(Perf_Table_cutoff_F1)  # 확인