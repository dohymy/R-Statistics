### 1. 이항분포
# 1. 베르누이 시행인지 판단하시오
# 1) F, 2) T, 3) F, 4) T
# 2. 패널티킥 5번 중 4번 성공, 10번 차서 7번 성공할 확률은?
dbinom(7,10,0.8)  # 0.2013266
# 3. 불량률 5%, 20개 중 2개 이상 불량일 확률?
pbinom(2, 20, 0.05)   # 0.9245163
# 4. 치료율 20%, 환자 20명 중 최소 2명 이상 치료될 확률?
1-pbinom(1,20,0.2)    # 0.9308247
# 5. 2개의 주사위 눈의 합이 6이 될 확률?


### 2. 정규분포
# 1. 전구의 수명 800일, 표준편차 40일, 수명이 750일 이하일 확률?
prob = round(pnorm(750, 800, 40),4)
prob # 0.1056
# 2. 근무기간 평균 11년, 분산 16년
# 1) 20년 이상 근무한 직원의 비율
prob = round(pnorm(20, 11, 4),4)
1-prob # 0.0122
# 2) 근무연수가 가장 오래된 10%는 몇 년 근무?
qnorm(0.9, 11 ,4) # 16.12621
# 3. 성적 평균 70, 표준편차 8일 때, 80점 이상 90점 미만
pnorm(90, 70, 8) - pnorm(80, 70, 8) # 0.09944011
# 4. 평균 1.5, 표준편차 2이고 H(t)= P(t ≤ X ≤ t+1) 일 때, H(0)+H(2)의 값
h = function(t){
  return(pnorm(t+1, 1.5, 2)-pnorm(t, 1.5, 2))
}
h(0)+h(2) # 0.3493326

### 3. 1-sample T test
# 1. 1000시간 일 때, 무작위로 뽑은 10개의 건전지에 대한
# 수명은 다음과 같다.
# 980, 1008, 968, 1032, 1012, 996, 1021, 1002, 996, 1017. 샘플이 모집단과 같다고 할 수 있는가?
dt = c(980, 1008, 968, 1032, 1012, 996, 1021, 1002, 996, 1017)
t.test(dt, mu=1000)
  # p-value = 0.611로 0.05보다 크므로 샘플은 모집단과 같다.
# 2.
dt = c(58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39)
t.test(dt, mu=55, alternative = 'greater')
  # p-value = 0.4046로 0.05보다 0교시 수업 시행 후 차이가 없다.
# 3. 
dt = c(15.50, 11.21, 12.67, 8.87, 12.15, 9.88, 2.06, 14.50, 0, 4.97)
t.test(dt, mu=8.1)
  # 0.5301로 0.05보다 크므로 평균 알코올 섭취량은 차이가 없다.

### 2. 2-sample T test
# 1. 
head(mtcars)
str(mtcars)
var.test(mpg ~ am, data=mtcars)   # p-value = 0.06691
t.test(mpg ~ am, data = mtcars)   # p-value = 0.001374 차이가 있다.
# 2.
library(MASS)
head(Cars93)
str(Cars93)
var.test(Price ~ Origin, data=Cars93) # p-value = 0.01387
t.test(Price ~ Origin, data=Cars93)   # p-value = 0.3428 차이가 없다.
# 3.
library(ggplot2)
head(mpg)
### 3. Paired sample T
# 1.
placebo = c(51.4, 52.0, 45.5, 54.5, 52.3, 50.9, 52.7, 50.3, 53.8, 53.1)
new_medicine = c(50.1, 51.5, 45.9, 53.1, 51.8, 50.3, 52.0, 49.9, 52.5,53.0)
t.test(placebo, new_medicine, paired=T, alternative='greater')
# p-value = 0.003105, 차이 있음

# 2. 
material_A = c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3)
material_B = c(14.0, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6)
t.test(material_A, material_B, paired=T, alternative='greater')
# p-value = 0.9957, 차이 없음

### 4. 모비율 추정
# 1.
phat = 0.5
n = 100
ll = phat - 1.96 * sqrt(phat * (1-phat)/n)
ul = phat + 1.96 * sqrt(phat * (1-phat)/n)
c(ll,ul)  # 0.402, 0.598
# 2.
phat = 0.8
n = 100
ll = phat - 1.96 * sqrt(phat * (1-phat)/n)
ul = phat + 1.96 * sqrt(phat * (1-phat)/n)
c(ll,ul)  # 0.7216, 0.8784

# 3.
phat = 0.43
n = 1000
ll = phat - 1.645 * sqrt(phat * (1-phat)/n)
ul = phat + 1.645 * sqrt(phat * (1-phat)/n)
c(ll,ul)  # 0.4042464, 0.4557536
