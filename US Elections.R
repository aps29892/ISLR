# US elections example
?read.csv
USdata = read.csv("E:/placements/career/ISLR/Chapter 4/US Statistics.csv",row.names = 1)
colnames(USdata)
head(USdata)

# Logistic regression
glm.fit1=glm(Winner~.,data = USdata,family = binomial)
summary(glm.fit1)
glm.probs1=predict(glm.fit1,type = "response")
glm.probs1[1:5]
glm.pred1=ifelse(glm.probs1<0.5,"Hillary","Trump")
confusion1=table(glm.pred1,USdata$Winner) # First element will be in rows and the second element will be in columns
sensitivity1=confusion1[2,2]/sum(confusion1[,2])
Specificity1=confusion1[1,1]/sum(confusion1[,1])

# Linear Discriminant Analysis
require(MASS)
lda.fit1=lda(Winner~.,data = USdata)
lda.fit1
plot(lda.fit1)
lda.pred1=predict(lda.fit1,USdata)
data.frame(lda.pred1)[1:5,]
table(lda.pred1$class,USdata$Winner)
mean(lda.pred1$class==USdata$Winner)

# KNN
require(class)
variables=USdata[,!(names(USdata) %in% "Winner")]
knn.pred1=knn(variables,variables,USdata$Winner,k=1)
table(knn.pred1,USdata$Winner)
knn.pred2=knn(variables,variables,USdata$Winner,k=2)
table(knn.pred2,USdata$Winner)


# Combinations - Logistic regression
# Choose the predicted value
predictor="Winner"
variables=colnames(USdata[,!(names(USdata) %in% predictor)])
results=data.frame()

for(i in 1:length(variables)){
        combinations=combn(variables,i)
        numofpossib=ncol(combinations)
        for(j in 1:numofpossib){
                combination=combinations[,j]
                formulaa=paste(predictor,paste(combination,collapse = "+"),sep = "~")
                classifier=glm(formulaa,data = USdata,family = binomial)
                probabilities=predict(classifier,type = "response")
                predictions=ifelse(probabilities<0.5,"Hillary","Trump")
                confusionmatrix=table(predictions,USdata$Winner)
                sensitivity=confusionmatrix[2,2]/sum(confusionmatrix[,2])
                specificity=confusionmatrix[1,1]/sum(confusionmatrix[,1])
                variable=paste(combination,collapse = "+")
                insertingrow=data.frame(0)
                insertingrow[1,1]=variable
                insertingrow[1,2]=sensitivity
                insertingrow[1,3]=1-specificity
                results=rbind(results,insertingrow)
        }
}

colnames(results)=c("Variables","True Positive","False Positive")
results
