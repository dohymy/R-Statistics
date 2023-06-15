##### 1. t분포
x = seq(-3, 3, by=0.01)
y = dnorm(x)
t2 = dt(x, df=1)
t8 = dt(x, df=8)
t32 = dt(x, df=32)
t64 = dt(x, df=64)

plot(x, y, type='l', lty=1, axes=F, xlab='x', ylab='', col='red')
axis(1)
lines(x, t2, lty=4, col='black')
lines(x, t8, lty=3, col='magenta')
lines(x, t32, lty=2, col='blue')
lines(x, t64, lty=6, col='green')
legend('topright', paste('df :', c(2,8,32,64)), lty=c(4,3,2,6),
       col=c('black','magenta','blue','green'), cex=0.7)

##### 2. x2 분포
x = seq(0, 20, by=0.01)
ch2 = dchisq(x, df=2)
ch8 = dchisq(x, df=8)    
ch32 = dchisq(x, df=32)    
ch64 = dchisq(x, df=64)    

plot(x, type='n', xlim=c(0,20), ylim=c(0,0.3), main='',
     xlab='x', ylab='', axes=F)
axis(1); axis(2)
lines(x, ch2, lwd=2, lty=1, col='black')
lines(x, ch8, lwd=2, lty=2, col='red')
lines(x, ch32, lwd=2, lty=3, col='blue')
lines(x, ch64, lwd=2, lty=4, col='green')
legend('topright', paste('df :', c(2,8,32,64)), lty=c(1,2,3,4),
       col=c('black','red','blue','green'), cex=0.7)

##### 3. F 분포
x = seq(0, 2, by=0.01)
f2.8 = df(x, df1=2, df2=8)
f2.32 = df(x, df1=2, df2=32)
f2.64 = df(x, df1=2, df2=64)
f8.32 = df(x, df1=8, df2=32)
f8.64 = df(x, df1=8, df2=64)
f32.64 = df(x, df1=32, df2=64)

plot(x, f2.8, type='l', ylim=c(0,0.9), lwd=2, axes=F, xlab='x', ylab='', col='red')
axis(1); axis(2)
lines(x, f2.8, lty=2, lwd=2, col='blue')
lines(x, f2.32, lty=3, lwd=2, col='black')
lines(x, f2.64, lty=4, lwd=2, col='orange')
lines(x, f8.32, lty=4, lwd=2, col='brown')
lines(x, f8.64, lty=4, lwd=2, col='yellow')
lines(x, f32.64, lty=4, lwd=2, col='green')
legend('topright', paste('df :', c('2, 8','2, 32','2, 64','8, 32','8, 64','32, 64')),
       col=c('red','blue','black','magenta'), lty=1:4, cex=0.7)

