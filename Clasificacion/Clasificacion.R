rm(list=ls())
par(mfrow=c(1,1))
require(caret)
library(MASS)
library(ISLR)
library(klaR)
library(class)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

vehicle = read.csv("/home/manuelmontero/Escritorio/R Projects/vehicle/vehicle.dat", comment.char="@")

names(vehicle) = c("Compactness", "Circularity", "Distance_circularity",
                    "Radius_ratio", "Praxis_aspect_ratio", "Max_length_aspect_ratio",
                    "Scatter_ratio", "Elongatedness","Praxis_rectangular","Length_rectangular",
                    "Major_variance","Minor_variance","Gyration_radius","Major_skewness",
                    "Minor_skewness","Minor_kurtosis","Major_kurtosis","Hollows_ratio","Class")

# print(round(prop.table(table(vehicle$Class)) * 100, digits = 1))
# plot(vehicle[,1:18], col=vehicle[,19], xlab = "Value", ylab = "Hollows_ratio", pch = 16)
# print(prop.table(table(cor(vehicle[,1:18])>0.7)))
a = table(cor(vehicle[,1:18])>0.7)
a["TRUE"] = (a["TRUE"] - 18)/2
a["FALSE"] = a["FALSE"]/2
prop.table(a)

# Dividir datos en train y test, primero desordenamos y cogemos el 80% train y 20% test
vehicle = vehicle[sample(nrow(vehicle)),]
lapply(vehicle[,1:18], normalize)
percentage = 80
cut = round(nrow(vehicle)*percentage/100)
vehicle_train = vehicle[1:cut, 1:18]
vehicle_test = vehicle[cut:nrow(vehicle), 1:18]
vehicle_train_labels = vehicle[1:cut, 19]
vehicle_test_labels = vehicle[cut:nrow(vehicle), 19]

# Utilizar el algoritmo k-NN probando con diferentes valores de k. 
# Elegir el que considere más adecuado para su conjunto de datos.
# kNum = 1 # change to compare
# tst = 1:kNum
# for (i in 1:kNum){
#   knnFit = train(vehicle_train, vehicle_train_labels, method="knn",
#                  preProc = c("center","scale"),
#                  metric="Accuracy", tuneGrid = data.frame(.k=i),
#                  tuneLength = 10)
#   knnPred = predict(knnFit, newdata = vehicle_test)
#   a = postResample(pred = knnPred, obs = vehicle_test_labels)
#   tst[i] = a["Accuracy"]
# }
# plot(tst, col="blue", type="l", xlab = "K", lwd=c(2.5), ylab = "Accuracy", xlim = c(1,kNum), ylim = c(0.5,0.8))
# legend("topright",c("Test"), col = c("blue"), lwd=c(2.5))

knnFit = train(vehicle[,1:18], vehicle[,19], method="knn", 
                 preProc = c("center","scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"),
                 tuneGrid = data.frame(.k=6))
print(confusionMatrix(knnFit))

# # Utilizar el algoritmo LDA para clasificar.
ldaFit = train(vehicle[,1:18], vehicle[,19],
                method = "lda",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))
print(confusionMatrix(ldaFit))

# Utilizar el algoritmo QDA para clasificar.
qdaFit = train(vehicle[,1:18], vehicle[,19], method = "qda",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))
print(confusionMatrix(qdaFit))

# Comparar los resultados de los tres algoritmos.
resultados = read.csv("/home/manuelmontero/Escritorio/R Projects/TablasAlumnos/clasif_test_alumnos.csv")
tablatra = cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatra) = names(resultados)[2:dim(resultados)[2]]
rownames(tablatra) = resultados[,1]
tablatra[17,1] = knnFit$results$Accuracy
tablatra[17,2] = ldaFit$results$Accuracy
tablatra[17,3] = qdaFit$results$Accuracy

# • Friedman. Using Holm see a winning algorithm
test_friedman = friedman.test(as.matrix(tablatra))
print(test_friedman)

tam = dim(tablatra)
groups = rep(1:tam[2], each=tam[1])
values = pairwise.wilcox.test(as.matrix(tablatra), groups, p.adjust = "holm", paired = TRUE)
print(values)

