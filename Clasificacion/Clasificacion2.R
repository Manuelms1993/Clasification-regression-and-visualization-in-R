# Clasificacion2

rm(list=ls())
library(MASS)
library(ISLR)
library(klaR)
library(class)
require(caret)

# lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=Year<2005)
# print(lda.fit)
# par(mar = rep(2, 4))
# plot(lda.fit, type="both")
# 
# Smarket.2005=subset(Smarket,Year==2005)
# lda.pred=predict(lda.fit,Smarket.2005)
# lda.pred[1:5]
# class(lda.pred)
# data.frame(lda.pred)[1:5,]
# table(lda.pred$class,Smarket.2005$Direction)
# mean(lda.pred$class==Smarket.2005$Direction)
# partimat(Direction~Lag1+Lag2, data=Smarket ,method="lda")


## QDA
# qda.fit=qda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
# print(qda.fit)
# print(qda.fit$scaling)
# # plot(qda.fit, type="both")
# Smarket.2005=subset(Smarket,Year==2005)
# qda.pred=predict(qda.fit,Smarket.2005)
# class(qda.pred)
# data.frame(qda.pred)[1:5,]
# table(qda.pred$class,Smarket.2005$Direction)
# mean(qda.pred$class==Smarket.2005$Direction)
# partimat(Direction~Lag1+Lag2, data=Smarket ,method="qda")


# iris.lda=lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
# print(iris.lda)
# partimat(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris, method="lda")

## LDA & QDA caret
# data(iris)
# TrainData <- iris[,1:4]
# TrainClasses <- iris[,5]
# ldaFit = train(TrainData, TrainClasses,
#                 method = "lda",
#                 preProcess = c("center", "scale"),
#                 tuneLength = 10,
#                 trControl = trainControl(method = "cv"))
# print(confusionMatrix(ldaFit))
# 
# qdaFit = train(TrainData, TrainClasses,method = "qda",
#                 preProcess = c("center", "scale"),
#                 tuneLength = 10,
#                 trControl = trainControl(method = "cv"))
# print(confusionMatrix(qdaFit))



# • Try lda with all Lag variables
# • Make a quick comparison between logistic regression and lda.
# • Try with qda and compare all three methods. Plot theresults.
lda.fit=lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5,data=Smarket,subset=Year<2005)
Smarket.2005=subset(Smarket,Year==2005)
lda.pred=predict(lda.fit,Smarket.2005)
print(mean(lda.pred$class==Smarket.2005$Direction))

qda.fit=qda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5,data=Smarket,subset=Year<2005)
qda.pred=predict(qda.fit,Smarket.2005)
print(mean(qda.pred$class==Smarket.2005$Direction))

glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5,data=Smarket, family=binomial, subset=Year<2005)
glm.pred=predict(glm.fit,Smarket.2005, type = "response")
glm.pred = ifelse(glm.pred >0.5,"Up","Down")
print(mean(glm.pred==Smarket.2005$Direction))


# Using only the information in file clasif_train_alumnos.csv:
#leemos la tabla con los errores medios de train
resultados = read.csv("/home/manuelmontero/Escritorio/R Projects/TablasAlumnos/clasif_train_alumnos.csv")
tablatra = cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatra) = names(resultados)[2:dim(resultados)[2]]
rownames(tablatra) = resultados[,1]
# • Compare lda and qda using Wilcoxon.
difs = (tablatra[,2] - tablatra[,3]) / tablatra[,2]
wilc_1_2 = cbind(ifelse (difs<0, abs(difs)+0.1, 0.1), ifelse (difs>0, abs(difs)+0.1, 0.1))
colnames(wilc_1_2) = c(colnames(tablatra)[2], colnames(tablatra)[3])
rownames(wilc_1_2) = c(rownames(tablatra))
print(wilc_1_2)
LMvsKNNtst = wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas = LMvsKNNtst$statistic
pvalue = LMvsKNNtst$p.value
LMvsKNNtst = wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos = LMvsKNNtst$statistic
cat("Rmas:",Rmas,"\n")
cat("Rmenos:",Rmenos,"\n")
cat("pvalue:",pvalue,"# No hay diferencias significativas entre ambos, pvalue>0.05\n")
# • Perform a multiple comparison using Friedman. Using Holm see if there is a winning algorithm
test_friedman = friedman.test(as.matrix(tablatra))
print(test_friedman)
print("No hay diferencias significativas entre el knn, lda y qda por tanto Holm no tendria sentido")
print("Aun asi lo mostrare para verlo en la tabla")
tam = dim(tablatra)
groups = rep(1:tam[2], each=tam[1])
values = pairwise.wilcox.test(as.matrix(tablatra), groups, p.adjust = "holm", paired = TRUE)
print(values)
