##### 7. 회귀분석
# 1.
x1=c(150,160,170,180,190)
y1=c(176,179,182,181,185)
lm(y1~x1) # 아들 = 146.6 + (아빠 * 0.2) --> 아빠 165일 때, 아들은 179.6
test1 = lm(y1~x1)
summary(test1)

# 2.
x2=c(100,200,300,400,500)
y2=c(30,70,85,140,197)
lm(y2~x2) # y = -16.800 + (0.404 * x) --> 250만원일 때, 84.2
test2 = lm(y2~x2)
summary(test2)

# 3.
head(mtcars)
lm(hp ~ disp, data=mtcars) # y = 45.7345 + (x * 0.4376)
test3 = lm(hp ~ disp, data=mtcars)
summary(test3)
