require(ISLR)
names(Smarket)
summary(Smarket)
?Smarket
pairs(Smarket,col=Smarket$Direction)
head(Smarket)

# Logistic regression
?glm
glm.fit1=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Smarket,family = binomial)
summary(glm.fit1)
glm.probs1=predict(glm.fit1,type = "response")
glm.probs1[1:5]
glm.pred1=ifelse(glm.probs1>0.5,"Up","Down")
attach(Smarket)
table(glm.pred1,Direction)
mean(glm.pred1==Direction)

# Making test and train sets
train=Year<2005
glm.fit2=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Smarket,family = binomial,subset = train)
glm.probs_test=predict(glm.fit2,newdata = Smarket[!train,],type = "response")
glm.pred_test=ifelse(glm.probs_test>0.5,"Up","Down")
Direction.2005=Smarket$Direction[!train]
table(glm.pred_test,Direction.2005)
mean(glm.pred_test==Direction.2005)

# Making test and train sets
glm.fit3=glm(Direction~Lag1+Lag2,data = Smarket,family = binomial,subset = train)
glm.probs_test2=predict(glm.fit3,newdata = Smarket[!train,],type = "response")
glm.pred_test2=ifelse(glm.probs_test2>0.5,"Up","Down")
table(glm.pred_test2,Direction.2005)
mean(glm.pred_test2==Direction.2005)

# Linear Discriminant Analysis
require(ISLR)
require(MASS)

lda.fit1=lda(Direction~Lag1+Lag2,data=Smarket,subset=Year<2005)
lda.fit1
plot(lda.fit1)
Smarket.2005=subset(Smarket,Year==2005)
lda.pred1=predict(lda.fit1,Smarket.2005)
lda.pred1[1:5]
class(lda.pred1)
data.frame(lda.pred1)[1:5,]
table(lda.pred1$class,Smarket.2005$Direction)
mean(lda.pred1$class==Smarket.2005$Direction)

# K-nearest Neighbours
library(class)
?knn
attach(Smarket)
objects(2)
xlag=cbind(Lag1,Lag2)
train=Year<2005
knn.pred=knn(xlag[train,],xlag[!train,],Direction[train],k=1)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])
