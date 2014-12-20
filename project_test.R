?mtcars
str(mtcars)
summary(mtcars) #proves there are no missing values
#look for correlation
cor(mtcars)
pairs(mtcars)

#identify potential confounders... those variables
#which have a strong correlation to other predictive
#variables and the response variable (mpg)

#b.s. linear model with all variables
fake<-lm(mpg ~ . , data=mtcars)
summary(fake)

#an application of stepwise regression, after fitting
#a model with all possible variables, which removes one
#by one:
fake.1<-update(fake, .~.-vs)

#use an algorithm:
#direction = backward provides for wt, qsec and am
#as significant... adj. R2=.8336
b.fake<-step(fake,direction="backward")

f.fake<-step(fake,direction="forward")

#the faster a car completes a qtr mile, the better
#its mpg... cylinder is neg. correlated to 'qsec'
cor(mtcars$cyl,mtcars$qsec)

#cyclinders highly correlated to hp... but ditch
#hp from the model
cor(mtcars$hp,mtcars$cyl)

#horsepower = RPM x Torque/5252

#possible interactions to study:
#cyl with wt, qsec and am... may want to code cyl as
#a factor - there are only 3 cylinder types (4,6,8)
#... I may want to flip the model and use
#'am' as an interaction variable

#we move the x variables so that the intercept 
#has meaning
adj.wt<-mtcars$wt-min(mtcars$wt)

#first force a model with my chosen variables:
mine<-lm(mpg~cyl+wt, data=mtcars) #adj. R2=.8185
mine.f<-lm(mpg~as.factor(cyl)+wt, data=mtcars) #adj. R2=.8185
#a clean display of the summary data points
display(mine)
#run the above but with adjusted 'wt'... my best model
#thus far... adj.R2=.82
mine.a<-lm(mpg~adj.wt+as.factor(cyl), data=mtcars)

#display the fitted model
install.packages('arm')
library(arm)

#testing of moving the x-variables in slm
just.wt<-lm(mpg~wt,data=mtcars)
display(just.wt)
 

range(mtcars$wt)
adj<-lm(mpg~adj.wt,data=mtcars)
display(adj)

plot(adj.wt,mtcars$mpg)
lines(adj.wt,adj$fitted)
lines()

#run anova for each group manual-automatic classed
#on cylinder and weight class
anova(aov(mine.a))
