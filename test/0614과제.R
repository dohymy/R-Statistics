# 문제.1 
options(digit = 4)
mu = 11
sigma = 4
ll = mu - 3 * sigma
ul = mu + 3 * sigma
x = seq(ll, ul, by=0.01)
px = dnorm(x, mean=mu, sd=sigma)
plot(x, px, type='l', xlab = '근무기간', ylab = 'P[X=x]',
     lwd=2, col='red', main='N(11,4^2)')
prob = round(pnorm(14, 11, 4),4)
print(paste('14년 이상 근무한 종업원의 비율은', 1-prob, '입니다.'))

# 문제.2
mu = 800
sigma = 30
ll = mu - 3 * sigma
ul = mu + 3 * sigma
x = seq(ll, ul, by=0.01)
px = dnorm(x, mean=mu, sd=sigma)
plot(x, px, type='l', xlab = '전구의 수명', ylab = 'P[X=x]',
     lwd=2, col='red', main='N(800,30^2)')
prob = round(pnorm(760, 800, 30),4)
print(paste('전구의 수명이 760일 이하일 확률은', prob, '입니다.'))

# 문제.3
pnorm(90, 70, 8) - pnorm(80, 70, 8)
print(paste('점수가 80점 이상, 90점 이하일 확률은',round(pnorm(90, 70, 8) - pnorm(80, 70, 8),4),'입니다.'))
