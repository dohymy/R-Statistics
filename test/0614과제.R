# 문제.1 
prob = round(pnorm(14, 11, 4),4)
print(paste('14년 이상 근무한 종업원의 비율은', 1-prob, '입니다.'))

# 문제.2
prob = round(pnorm(760, 800, 30),4)
print(paste('전구의 수명이 760일 이하일 확률은', prob, '입니다.'))

# 문제.3
pnorm(90, 70, 8) - pnorm(80, 70, 8)
print(paste('점수가 80점 이상, 90점 이하일 확률은',round(pnorm(90, 70, 8) - pnorm(80, 70, 8),4),'입니다.'))
