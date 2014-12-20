#week three class notes ###
n<-100;x<-rnorm(n);x2<-rnorm(n)
y<-x+x2+rnorm(n,sd=.1)

#regresses 'a' on 'b'... 
#aka removing linear effects of 'b'
e<-function(a,b) a- sum(a*b) / sum(b^2) *b

ey<-e(e(y,x2), e(x3,x2))
ex<-e(e(x,x2), e(x3,x2))

#take the residual of y taking out x3, take the 
#residual of x taking out x3, then taking out the 
#residual of y taking out x (second e wrapper call)
ey<-e(e(y,x3), e(x,x3))
ex<-e(e(x2,x3), e(x,x3))
#regression through origin slope
sum(ey*ex)/sum(ex^2)


#do the above for only two coefficients
ey<-e(y,x)
ex<-e(x2,x)
sum(ey*ex)/sum(ex^2)
fit<-lm(y~x+x2 -1)
fit$coefficients

data(InsectSprays)
summary(lm(count~spray,data=InsectSprays))$coef

str(InsectSprays)

#t is the "treatment" effect
#tau also is treatment effect
#x is a uniformally distributed numeric variable
n<-100; t<-rep(c(0,1), c(n/2,n/2));x<- c(runif(n/2), runif(n/2));
beta0<-0; beta1<-2; tau <-1; sigma<- .2

#t is a (0,1) variable
y<-beta0+x*beta1+t*tau+rnorm(n,sd = sigma)
