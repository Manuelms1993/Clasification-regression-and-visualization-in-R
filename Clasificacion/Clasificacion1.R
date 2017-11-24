rm(list=ls())
library(class)

wbcd = read.csv("~/Escritorio/R Projects/clasificacion/wisc_bc_data.csv",stringsAsFactors = FALSE)
wbcd = wbcd[,-1]
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),labels = c("Benign", "Malignant"))

# str(wbcd)
# print(table(wbcd$diagnosis))
# print(round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1))
# print(summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")]))

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
wbcd_n <- as.data.frame(lapply(wbcd[,2:31], normalize))

# plot(wbcd[,2:5])
# plot(wbcd_n[,1:4], col=wbcd[,1])
# print(cor(wbcd[,2:5]))

# wbcd_n = wbcd_n[sample(nrow(wbcd_n)),]
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

# wbcd_test_pred = knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
# print(prop.table(table(wbcd_test_pred,wbcd_test_labels)))

require(caret)
# knnModel = train(x = wbcd[1:469,-1],
#                  y = wbcd[1:469,1], method = "knn", preProc = c("center","scale"))
# print(knnModel)

# knnFit = train(wbcd_train, wbcd_train_labels, method="knn",
#                 metric="Accuracy", tuneGrid = data.frame(k=1:15))

# knnPred = predict(knnModel, newdata = wbcd_test)
# print(postResample(pred = knnPred, obs = wbcd_test_labels))
# knnPred = predict(knnFit, newdata = wbcd_test)
# a = postResample(pred = knnPred, obs = wbcd_test_labels)
# print(a["Accuracy"])



# Try with different k choices and do a quick comparison.
# You can draw a plot to show the results.
# kNum = 10
# trn = 1:kNum
# tst = 1:kNum
# for (i in 1:kNum){
#   knnFit = train(wbcd_train, wbcd_train_labels, method="knn", preProc = c("center","scale"),
#                  metric="Accuracy", tuneGrid = data.frame(.k=i))
#   knnPred = predict(knnFit, newdata = wbcd_test)
#   a = postResample(pred = knnPred, obs = wbcd_test_labels)
#   trn[i] = as.data.frame(knnFit[4])$results.Accuracy
#   tst[i] = a["Accuracy"]
# }
# 
# plot(trn,col="blue",type="l",xlab = "K", ylab = "Accuracy", xlim = c(1,kNum), ylim = c(0.9,1))
# lines(tst,col="red",pch=13)







require(ISLR)
# print(names(Smarket))
# print(summary(Smarket))
# print(cor(Smarket[,-9]))
# boxplot(Smarket$Volume~Smarket$Year)
# print(cor(as.numeric(Smarket$Direction),Smarket$Today))
# glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
# print(summary(glm.fit))
# glm.probs = predict(glm.fit,type="response")
# glm.probs[1:5]
# glm.pred = ifelse(glm.probs>0.5,"Up","Down")
# attach(Smarket)
# print(prop.table(table(glm.pred,Direction)))
# print(mean(glm.pred==Direction))


# train = Year<2005
# glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial, subset=train)
# glm.probs = predict(glm.fit,newdata=Smarket[!train,],type="response")
# glm.pred = ifelse(glm.probs >0.5,"Up","Down")
# Direction.2005=Smarket$Direction[!train]
# print(prop.table(table(glm.pred,Direction.2005)))
# print(mean(glm.pred==Direction.2005))

# glmFit = train(Smarket[,-9], y = Smarket[,9],
#                 method = "glm", preProcess = c("center", "scale"),
#                 tuneLength = 10, trControl = trainControl(method = "cv"))
# print(glmFit)


# Using the Smarket dataset:
#   Perform 10 fold-cv with logistic regression.
glmFit = train(Smarket[,-9], y = Smarket[,9],
               method = "glm", preProcess = c("center", "scale"),
               tuneLength = 10, trControl = trainControl(method = "cv", repeats = 10))
print(glmFit)




