require(ISLR)
# Question 10
?Weekly
# 10.a
pairs(Weekly,col=Weekly$Direction)
# 10.b - None of the predictors look strong
glm.fit1=glm(Direction~.-Year-Today,data = Weekly,family = binomial)
summary(glm.fit1)
# 10.c - Very high rate of false positives
glm.probs1=predict(glm.fit1,type = "response")
levels(Weekly$Direction) # To know the levels 0,1 of the data
glm.pred1=ifelse(glm.probs1<0.5,"Down","Up")
confusionmatrix1=table(glm.pred1,Weekly$Direction)
confusionmatrix1
(confusionmatrix1[1,2]+confusionmatrix1[2,1])/sum(confusionmatrix1)
# 10.d
train1=Weekly$Year<2009
glm.fit2=glm(Direction~Lag2,data = Weekly[train1,],family = binomial)
summary(glm.fit2)
glm.probs2=predict(glm.fit2,newdata = Weekly[!train1,],type = "response")
glm.pred2=ifelse(glm.probs2<0.5,"Down","Up")
confusionmatrix2=table(glm.pred2,Weekly[!train1,]$Direction)
confusionmatrix2
(confusionmatrix1[1,1]+confusionmatrix1[2,2])/sum(confusionmatrix1)
