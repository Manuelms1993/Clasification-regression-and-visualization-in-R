# California
# Manuel Montero
rm(list=ls())
require(kknn)

california = read.csv("/home/manuelmontero/Escritorio/R Projects/california/california.dat", comment.char="@")
names(california) = c("Longitude", "Latitude", "HousingMedianAge",
                 "TotalRooms", "TotalBedrooms", "Population", "Households",
                 "MedianIncome", "MedianHouseValue")

attach(california)

# Visualizacion global de los datos
temp = california
plotY = function (x,y) {
  plot(temp[,y]~temp[,x], xlab=paste(names(temp)[x]," X",x,sep=""),
  ylab=names(temp)[y])
}

par(mfrow=c(3,3))
# x = sapply(1:(dim(temp)[2]-1), plotY, dim(temp)[2])
par(mfrow=c(1,1))


# Las mas interesantes, Quizas el 8 y 5 son los mas relevantes
par(mfrow=c(1,2))
# x = sapply(c(5,8), plotY, dim(temp)[2])
par(mfrow=c(1,1))


# Calculo de Modelos (Pruebas)
# model1 = lm(MedianHouseValue~MedianIncome, data=california)
# print(summary(model1))
# model2 = lm(MedianHouseValue~TotalBedrooms, data=california)
#print(summary(model2)) # Este modelo es muy malo, r-squared 0.0025
# model3 = lm(MedianHouseValue~., data=california)
# print(summary(model3)) # Este es "bueno"

# Voy a intentar mejorar a partir del 3
# model4 = lm(MedianHouseValue~.^5, data=california)
# print(summary(model4)) # bastante mejor


# # Parte 2
# model4kknn = kknn(MedianHouseValue~.^5, california, california)
# model5kknn = kknn(MedianHouseValue~MedianIncome, california, california)
# model6kknn = kknn(MedianHouseValue~TotalBedrooms, california, california)
# print(summary(model4kknn))
# 
# yprime = model4kknn$fitted.values
# RMSE = sqrt(sum((MedianHouseValue-yprime)^2)/length(yprime)) #RMSE
# print(RMSE)
# yprime = model5kknn$fitted.values
# RMSE = sqrt(sum((MedianHouseValue-yprime)^2)/length(yprime)) #RMSE
# print(RMSE)
# yprime = model6kknn$fitted.values
# RMSE = sqrt(sum((MedianHouseValue-yprime)^2)/length(yprime)) #RMSE
# print(RMSE)
# tambien es mejor el mejor de LM

name = "/home/manuelmontero/Escritorio/R Projects/california/california"
run_lm_fold = function(i, x, tt = "test", knn = FALSE) {
  file = paste(x, "-5-", i, "tra.dat", sep="")
  x_tra = read.csv(file, comment.char="@")
  file = paste(x, "-5-", i, "tst.dat", sep="")
  x_tst = read.csv(file, comment.char="@")
  In = length(names(x_tra)) - 1
  names(x_tra)[1:In] = paste ("X", 1:In, sep="")
  names(x_tra)[In+1] = "Y"
  names(x_tst)[1:In] = paste ("X", 1:In, sep="")
  names(x_tst)[In+1] = "Y"
  
  if (tt == "train") {test = x_tra}
  else {test = x_tst}
  if(knn) {
    fitMulti = kknn(Y~.,x_tra,test)
    yprime = fitMulti$fitted.values
  }
  else {
    fitMulti = lm(Y~.^3,x_tra)
    yprime = predict(fitMulti,test)
  }
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}

# lmMSEtrain = mean(sapply(1:5,run_lm_fold,name,"train"))
# lmMSEtest = mean(sapply(1:5,run_lm_fold,name,"test"))
# cat("lmMSEtrain: ",lmMSEtrain,"\n")
# cat("lmMSEtest: ",lmMSEtest,"\n")
# 
# kknnMSEtrain = mean(sapply(1:5,run_lm_fold,name,"train",knn=TRUE))
# kknnMSEtest = mean(sapply(1:5,run_lm_fold,name,"test",knn=TRUE))
# cat("kknnMSEtrain: ",kknnMSEtrain,"\n")
# cat("kknnMSEtest: ",kknnMSEtest,"\n")

#leemos la tabla con los errores medios de test
resultados = read.csv("/home/manuelmontero/Escritorio/R Projects/TablasAlumnos/regr_test_alumnos.csv")
tablatst = cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatst) = names(resultados)[2:dim(resultados)[2]]
rownames(tablatst) = resultados[,1]

#leemos la tabla con los errores medios de train
resultados = read.csv("/home/manuelmontero/Escritorio/R Projects/TablasAlumnos/regr_train_alumnos.csv")
tablatra = cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatra) = names(resultados)[2:dim(resultados)[2]]
rownames(tablatra) = resultados[,1]



# (Wilcoxon’s test)
##lm (other) vs knn (ref)
# + 0.1 porque wilcox R falla para valores == 0 en la tabla
difs = (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2 = cbind(ifelse (difs<0, abs(difs)+0.1, 0.1), ifelse (difs>0, abs(difs)+0.1, 0.1))
colnames(wilc_1_2) = c(colnames(tablatst)[1], colnames(tablatst)[2])
rownames(wilc_1_2) = c(rownames(tablatst))
# print(wilc_1_2)

# Se aplica el test y se interpretan los resultados
LMvsKNNtst = wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas = LMvsKNNtst$statistic
pvalue = LMvsKNNtst$p.value
LMvsKNNtst = wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos = LMvsKNNtst$statistic
cat("Rmas:",Rmas,"\n")
cat("Rmenos:",Rmenos,"\n")
cat("pvalue:",pvalue,"# No hay diferencias significativas entre ambos\n")

# Comparativas Múltiples – Usaremos Friedman y como post-
# hoc Holm (los rankings se calculan por posiciones de los
# algoritmos y hace falta normalización)
test_friedman = friedman.test(as.matrix(tablatst))
print(test_friedman)
tam = dim(tablatst)
groups = rep(1:tam[2], each=tam[1])
values = pairwise.wilcox.test(as.matrix(tablatst), groups, p.adjust = "holm", paired = TRUE)
print(values) # Los pvalues de 3vs1 y 3vs2 son cercanos al 0,1 por lo que se puede considerar al
# algoritmo 3 mejor si observamos que 1vs2 es muy parecido


######## PRUEBAS ADICIONALES #########
########## Division en train y test, (Solo probando cosas)
# pTrain = 80
# train = california[0:round(dim(california)[1]*(pTrain/100)),]
# test = california[(dim(train)[1]+1):dim(california)[1],]
# bestlm =lm(train$MedianHouseValue~.^5,train)
# yprime = predict(bestlm,test[,-9])
# MSE = sum(abs(test$MedianHouseValue-yprime)^2)/length(yprime) ##MSE
# cat("MSE LM: ",MSE,"\n")
# 
# bestknn = kknn(train$MedianHouseValue~.,train,test,k=15)
# yprime = bestknn$fitted.values
# MSE2 = sum(abs(test$MedianHouseValue-yprime)^2)/length(yprime) ##MSE
# cat("MSE KNN: ",MSE2,"\n")

detach(california)