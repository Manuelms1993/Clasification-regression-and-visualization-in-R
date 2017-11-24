rm(list=ls())
require(kknn)

treasury = read.csv("/home/manuelmontero/Escritorio/R Projects/treasury/treasury.dat", comment.char="@")

names(treasury) = c("Y1CMaturityRate", "Y30CMortgageRate", "M3RateAuctionAverage",
                      "M3RateSecondaryMarket", "Y3CMaturityRate", "Y5CMaturityRate",
                      "bankCredit", "currency","demandDeposits","federalFunds",
                    "moneyStock","checkableDeposits","loansLeases","savingsDeposits",
                    "tradeCurrencies","OneMonthCDRate")


# # Visualizacion global de los datos
# temp = treasury
# plotY = function (x,y) {
#   plot(temp[,y]~temp[,x], xlab=paste(names(temp)[x]," X",x,sep=""),
#   ylab=names(temp)[y])
# }
# 
# par(mfrow=c(2,2))
# x = sapply(1:(dim(temp)[2]-1), plotY, dim(temp)[2])
# par(mfrow=c(1,1))

# hist(treasury$OneMonthCDRate, xlab = "OneMonthCDRate",main = "Histogram of OneMonthCDRate")


# Calculo de Modelos (Pruebas)
# model1 = lm(treasury$OneMonthCDRate~treasury$Y30CMortgageRate)
# print(summary(model1))
# model2 = lm(treasury$OneMonthCDRate~treasury$M3RateSecondaryMarket)
# print(summary(model2))
# model3 = lm(treasury$OneMonthCDRate~treasury$Y3CMaturityRate)
# print(summary(model3))
# model4 = lm(treasury$OneMonthCDRate~treasury$Y5CMaturityRate)
# print(summary(model4))
# model5 = lm(treasury$OneMonthCDRate~treasury$moneyStock)
# print(summary(model5))

# modelMult = lm(treasury$OneMonthCDRate~.
#   -federalFunds-M3RateAuctionAverage-M3RateSecondaryMarket-Y5CMaturityRate-loansLeases-demandDeposits, data=treasury)
# print(summary(modelMult))

# modelMult = lm(treasury$OneMonthCDRate~.^3, data=treasury)
# print(summary(modelMult))


name = "/home/manuelmontero/Escritorio/R Projects/treasury/treasury"
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
    fitMulti = lm(Y~.,x_tra)
    # fitMulti = lm(Y~.^3,x_tra)
    # fitMulti = lm(Y~.-X3-X4-X6-X8-X12,x_tra)
    # fitMulti = lm(Y~X14+X10+I(X10^2)+I(X10^3)+X5+I(X5^2),x_tra)
    yprime = predict(fitMulti,test)
  }
  sum(abs(test$Y-yprime)^2)/length(yprime)
}

lmMSEtrain = mean(sapply(1:5,run_lm_fold,name,"train"))
lmMSEtest = mean(sapply(1:5,run_lm_fold,name,"test"))
cat("lmMSEtrain: ",lmMSEtrain,"\n")
cat("lmMSEtest: ",lmMSEtest,"\n")

kknnMSEtrain = mean(sapply(1:5,run_lm_fold,name,"train",knn=TRUE))
kknnMSEtest = mean(sapply(1:5,run_lm_fold,name,"test",knn=TRUE))
cat("kknnMSEtrain: ",kknnMSEtrain,"\n")
cat("kknnMSEtest: ",kknnMSEtest,"\n")


#leemos la tabla con los errores medios de test
resultados = read.csv("/home/manuelmontero/Escritorio/R Projects/TablasAlumnos/regr_test_alumnos.csv")
tablatst = cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatst) = names(resultados)[2:dim(resultados)[2]]
rownames(tablatst) = resultados[,1]
tablatst[16,1]=lmMSEtest
tablatst[16,2]=kknnMSEtest

#leemos la tabla con los errores medios de train
resultados = read.csv("/home/manuelmontero/Escritorio/R Projects/TablasAlumnos/regr_train_alumnos.csv")
tablatra = cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatra) = names(resultados)[2:dim(resultados)[2]]
rownames(tablatra) = resultados[,1]
tablatra[16,1]=lmMSEtrain
tablatra[16,2]=kknnMSEtrain

# (Wilcoxon’s test)
##lm (other) vs knn (ref)
# + 0.1 porque wilcox R falla para valores == 0 en la tabla
difs = (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2 = cbind(ifelse (difs<0, abs(difs)+0.1, 0.1), ifelse (difs>0, abs(difs)+0.1, 0.1))
colnames(wilc_1_2) = c(colnames(tablatst)[1], colnames(tablatst)[2])
rownames(wilc_1_2) = c(rownames(tablatst))
print(wilc_1_2)

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
# # hoc Holm (los rankings se calculan por posiciones de los
# # algoritmos y hace falta normalización)
test_friedman = friedman.test(as.matrix(tablatst))
print(test_friedman)
tam = dim(tablatst)
groups = rep(1:tam[2], each=tam[1])
values = pairwise.wilcox.test(as.matrix(tablatst), groups, p.adjust = "holm", paired = TRUE)
print(values)


