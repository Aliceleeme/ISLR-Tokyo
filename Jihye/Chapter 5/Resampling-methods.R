
#### ISLR Chapter 5#### 

getwd()
setwd("/Users/jihye/Downloads")

library(ISLR)
set.seed(1)  # 난수선택에 관여 
train = sample(392,196) #sample 392개 중 196개만 선택하여 트레인 데이터 생성

lm.fit = lm(mpg~horsepower, data=Auto, subset=train)  #train data에만 선형회귀 적합 

attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2)


#### 다항식 및 삼차회귀에 대한 검정오차 추정 ####
lm.fit2 = lm(mpg~poly(horsepower, 2), data=Auto, subset=train) 
mean((mpg-predict(lm.fit2, AUto))[-train]^2)

lm.fit3 = lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto)[-train]^2)
     

#### 난수 선정을 다르게 해서 결과를 달리한 경우 #### 
          
set.seed(2)
train = sample(392,196)

lm.fit = lm(mpg~horsepower, subset=train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

lm.fit2 = lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)

lm.fit3 = lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)


 
#### LOOCV #### 

glm.fit = glm(mpg~horsepower, data = Auto)
coef(glm.fit)

lm.fit = lm(mpg~horsepower, data = Auto)
coef(glm.fit)

library(boot)
glm.fit = glm(mpg~horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta

cv.error = rep(0,5)
for (i in 1:5){
  glm.fit = glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error


#### k-fold ####
set.seed(17)
cv.error.10 = rep(0,10)
for (i in 1:10){
  glm.fit = glm(mpg~poly(horsepower, i), data=Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[i] #일반적으로 K=10 선택 
}
cv.error.10 


#### bootstrap #### 

alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100) #ISLR의 포트폴리오가 뭘 말하는 것인지... 

set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T))

boot(Portfolio, alpha.fn, R=1000) # ORDINARY NONPARAMETRIC BOOTSTRAP


#### Bootstrap; 선형회귀모델의 정확도 추정 #### 

boot.fn = function(data, index)
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T)) 
boot.fn(Auto, sample(392, 392, replace = T))
#boot.fn() 함수는 관측치들을 랜덤으로 복원추출하여 절편과 기울기에 대한 부트스트랩 추정치를 생성하는데 사용 가능 

boot(Auto, boot.fn, 1000) #boot 함수로 절편 기울기에 대한 1000개 부트스트랩 추정치의 표준오차 계산 

summary(lm(mpg~horsepower, data=Auto))$coef #표준오차 



# 데이터에 이차모델을 적합하여 부트스트랩 표준오차 추정치와 표준 선형회귀추정치를 계산 
boot.fn = function(data, index)
  coefficients(lm(mpg~horsepower+I(horsepower^2), data=data, subset=index))
set.seed(1)
boot(Auto, boot.fn, 1000)

summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef 


#### 수치해석을 어떻게 해야하는지 내용 보강할 것



#### 
