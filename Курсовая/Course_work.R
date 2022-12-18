path <- "C://Users//Софья//Desktop//1_курс_1_семестр//Машинное обучение//Курсовая//"

# Задние 1 -------------------------------------------------------------
require(e1071)
require(tree)
require(maptree)
require(mltools)

diabetes <- read.csv(paste(path, "pima-indians-diabetes.data", sep = ""), sep = ",")

colnames(diabetes) <- c("Pregnant_times", 
                        "Glucose_concentration",
                        "Blood_pressure",
                        "Skin_thickness",
                        "Insulin",
                        "BMI",
                        "Pegigree", 
                        "Age",
                        "Class")

diabetes$Class <- as.factor(diabetes$Class)

install.packages("Rtsne")
library(Rtsne)
data_plot <- Rtsne(diabetes)
png(paste(path, "diabetes_plot.png"), width = 720)
plot(data_plot$Y, pch = 19, col = c("green", "red")[diabetes$Class],  xlab = "", ylab = "")
legend(x="topright", legend = levels(diabetes$Class), col=c("green","red"), pch=19)
dev.off()

# предварительный анализ SVM

n <- nrow(diabetes)
ratio <- 0.8
nt <- as.integer(n * ratio)

kernels <- c("radial", "sigmoid", "poly")
result <- vector()

for (k in kernels)
{
  res <- vector()
  for (c in c(1, 10, 50, 100, 500, 1000))
  {
    tmp_res <- vector()
    
    for (i in 1:10)
    {
      diabetes_rand <- diabetes[order(runif(n)), ]
      diabetes_train <- diabetes_rand[1: nt, ]
      diabetes_test <- diabetes_rand[(nt + 1): n, ]
      
      class <- diabetes_test$Class
      diabetes_test$Class <- NULL
      
      model <- svm(Class ~ ., data=diabetes_train, type = "C-classification", cost = c, kernel = k)
      predicted <- predict(model, diabetes_test)
      tbl <- table(predicted, class)
      tmp_res <- append(tmp_res, (tbl[1 , 2] + tbl[2, 1])/ length(class))
    }
    res <- append(res, mean(tmp_res))
  }
  print(res)
  result <- append(result, res)
}

M <- matrix(result, ncol = 3, nrow = 6)
a<-t(M)

png(paste(path, "Result.png"))
x<-c(1, 10, 50, 100, 500, 1000)
plot(x, a[1,], type = "l", col="red", xlab="Значение штрафного параметра", ylab="Доля ошибочных предсказаний", ylim=c(0.20,0.35))
lines(x, a[2,], type = "l", col="green")
lines(x, a[3,], type = "l", col="blue")
dev.off()

# ---------------------------------------------------------------

gammas <- c(0.1, 0.5, 1, 5, 10, 50, 100)
res <- vector()

for (g in gammas)
{
  tmp_res <- vector()
  
  for (i in 1:10)
  {
    diabetes_rand <- diabetes[order(runif(n)),]
    diabetes_train <- diabetes_rand[1:nt,]
    diabetes_test <- diabetes_rand[(nt + 1):n,]
    
    class <- diabetes_test$Class
    diabetes_test$Class <- NULL
    
    model <- svm(
        Class ~ .,
        data = diabetes_train,
        type = "C-classification",
        cost = 1,
        kernel = "radial",
        gamma = g
      )
    
    predicted <- predict(model, diabetes_test)
    
    tbl <- table(predicted, class)
    
    tmp_res <- append(tmp_res, (tbl[1 , 2] + tbl[2, 1]) / length(class))
  }
  res <- append(res, mean(tmp_res))
  
  print(res)
}

png(paste(path, "Result2.png"))
x<-c(0.1, 0.5, 1, 5, 10, 50, 100)
plot(x, res, type = "l", col="red", xlab="Гамма", ylab="Доля ошибочных предсказаний", ylim=c(0.23,0.36))
dev.off()


# -----------boosting и bagging----------------------------------------

library(adabag)

diabetes_rand <- diabetes[order(runif(n)), ]
diabetes_train <- diabetes_rand[1: nt, ]
diabetes_test <- diabetes_rand[(nt + 1): n, ]

diabetes$Class <- as.factor(diabetes$Class)

tree_num <- seq(1, 201, 10)

error_bag <- vector()
error_boost <- vector()

for (t in tree_num)
{
  err_bag <- vector()
  err_boost <- vector()
  
  for(i in 1:4)
  {
    clf1 <- bagging(Class ~ ., data = diabetes_train, mfinal = t)
    err_bag <- append(err_bag, predict(clf1, diabetes_test)$error)
    
    clf2 <- boosting(Class ~ ., data = diabetes_train, mfinal = t)
    err_boost <- append(err_boost, predict(clf2, diabetes_test)$error)
  }
  error_bag <- append(error_bag, mean(err_bag))
  error_boost <- append(error_boost, mean(err_boost))
}

mean(error_bag)
mean(error_boost)

error_bag
error_boost

png(paste(path, "bagging.png"), width = 720)
plot(x = tree_num, y = error_bag, xlab = "Число деревьев", ylab = "Размер ошибки", pch = 2, lwd = 2, col = "green")
dev.off()

png(paste(path, "boosting.png"), width = 720)
plot(x = tree_num, y = error_boost, xlab = "Число деревьев", ylab = "Размер ошибки", pch = 2, lwd = 2, col = "red")
dev.off()

# Задание 3--------------------------------------------------------------
library(dplyr)
library(cluster)

diabetes <- read.csv(paste(path, "pima-indians-diabetes.data", sep = ""), sep = ",")

colnames(diabetes) <- c("Pregnant_times", 
                        "Glucose_concentration",
                        "Blood_pressure",
                        "Skin_thickness",
                        "Insulin",
                        "BMI",
                        "Pegigree", 
                        "Age",
                        "Class")

diabetes$Class <- NULL
matr <- dist(scale(diabetes))
hc <- hclust(matr)

hcd <- as.dendrogram(hc)

pdf(paste(path, "dendrogramma.pdf"), width = 45, height = 20)
par(cex = 0.3)
plot(hcd, cex = 0.7)
dev.off()


class <- diabetes$Class
diabetes$Class <- NULL

# будем использовать Евклидову метрику без параметра стандартизации, т.к. 
# выяснили, что это точнее всего в лабе по кластеризации

tmp <- vector()

for(i in 1:10)
{
  cl <- clara(diabetes, 2, rngR = T)
  cl$clustering[cl$clustering == 1] <- 0
  cl$clustering[cl$clustering == 2] <- 1
  
  tmp <- append(tmp, sum(abs(cl$clustering - class)))
}

1 - mean(tmp)/length(class)
mean(tmp)

sum(tmp)/10

# Задание 4 ------------------------------------------------------------
install.packages("glmnet")
library(glmnet)

diabetes <- read.csv(paste(path, "pima-indians-diabetes.data", sep = ""), sep = ",")

colnames(diabetes) <- c("Pregnant_times", 
                        "Glucose_concentration",
                        "Blood_pressure",
                        "Skin_thickness",
                        "Insulin",
                        "BMI",
                        "Pegigree", 
                        "Age",
                        "Class")

x<-as.matrix(diabetes[,-9])
y<-diabetes[,9]

glm<-glmnet(x,y,family="binomial",nlambda =97,alpha =1)
res<-as.matrix(glm$beta)

# Задание 5 ------------------------------------------------------------
install.packages("h2o")
# Modeling packages
library(h2o)  # for fitting autoencoders

diabetes <- read.csv(paste(path, "pima-indians-diabetes.data", sep = ""), sep = ",")

colnames(diabetes) <- c("Pregnant_times", 
                        "Glucose_concentration",
                        "Blood_pressure",
                        "Skin_thickness",
                        "Insulin",
                        "BMI",
                        "Pegigree", 
                        "Age",
                        "Class")

h2o.init()
features <- as.h2o(diabetes[-9])

ae1 <- h2o.deeplearning(
  x = seq_along(features),
  training_frame = features,
  autoencoder = TRUE,
  hidden = 2,
  activation = 'Tanh'
)

# Extract the deep features
ae1_codings <- h2o.deepfeatures(ae1, features, layer = 1)
ae1_codings
data_coding <- as.data.frame(ae1_codings)
data_coding$Class <- diabetes$Class


tsne <- Rtsne(data_coding)
png(paste(path, "Autoencode.png"))
plot(tsne$Y, pch = 21, bg = c("red", "blue"))
dev.off()

n <- nrow(data_coding)
rand <- data_coding[ order(runif(n)),]
nt <- as.integer(n*0.8)

data_train <- rand[1:nt, ]
data_test <- rand[(nt+1):n, ]

kernels <- c("radial", "sigmoid", "poly")
result <- vector()
class <- data_test$Class
data_test$Class <- NULL

for (k in kernels)
{
  res <- vector()
  for (c in c(1, 10, 50, 100, 500, 1000))
  {
    tmp_res <- vector()
    
    for (i in 1:10)
    {
      model <- svm(Class ~ ., data=data_train, type = "C-classification", cost = c, kernel = k)
      predicted <- predict(model, data_test)
      tbl <- table(predicted, class)
      tmp_res <- append(tmp_res, (tbl[1 , 2] + tbl[2, 1])/ length(class))
    }
    res <- append(res, mean(tmp_res))
  }
  print(res)
  result <- append(result, res)
}

auto_svm <- svm(Class ~ ., data = data_train, type = "C-classification", cost = 10, kernel = "radial")
pred_svm <- predict(auto_svm, data_test)
tbl <- table(pred_svm, class)
res <- (tbl[1 , 2] + tbl[2, 1])/ length(class)

# -------------------------------------------------------------------
h2o.init()
features <- as.h2o(diabetes[-9])

ae2 <- h2o.deeplearning(
  x = seq_along(features),
  training_frame = features,
  autoencoder = TRUE,
  hidden = 70,
  activation = 'Tanh'
)

# Extract the deep features
ae2_codings <- h2o.deepfeatures(ae2, features, layer = 1)
ae2_codings
data_coding <- as.data.frame(ae2_codings)
data_coding$Class <- diabetes$Class


tsne <- Rtsne(data_coding)
png(paste(path, "Autoencode2.png"))
plot(tsne$Y, pch = 21, bg = c("red", "blue"))
dev.off()

n <- nrow(data_coding)
rand <- data_coding[ order(runif(n)),]
nt <- as.integer(n*0.8)
data_train <- rand[1:nt, ]
data_test <- rand[(nt+1):n, ]

kernels <- c("radial", "sigmoid", "poly")
result <- vector()
class <- data_test$Class
data_test$Class <- NULL

for (k in kernels)
{
  res <- vector()
  for (c in c(1, 10, 50, 100, 500, 1000))
  {
    tmp_res <- vector()
    
    for (i in 1:10)
    {
      model <- svm(Class ~ ., data=data_train, type = "C-classification", cost = c, kernel = k)
      predicted <- predict(model, data_test)
      tbl <- table(predicted, class)
      tmp_res <- append(tmp_res, (tbl[1 , 2] + tbl[2, 1])/ length(class))
    }
    res <- append(res, mean(tmp_res))
  }
  print(res)
  result <- append(result, res)
}

auto_svm <- svm(Class ~ ., data = data_train, type = "C-classification", cost = 10, kernel = "radial")
pred_svm <- predict(auto_svm, data_test)
tbl <- table(pred_svm, class)
res <- (tbl[1 , 2] + tbl[2, 1])/ length(class)

# ----------------------------------------------------------------------
features <- as.h2o(diabetes[-9])
features_2 <- as.h2o(diabetes[-9] + rnorm(nrow(diabetes), mean = 0, sd = 0.1))
denoise_ae <- h2o.deeplearning(
  x = seq_along(features),
  training_frame = features_2,
  validation_frame = features,
  autoencoder = TRUE,
  hidden = 70,
  activation = 'Tanh',
  sparse = TRUE
)

denoise_ae_codings <- h2o.deepfeatures(denoise_ae, features, layer = 1)
data_coding <- as.data.frame(denoise_ae_codings)
data_coding$Class <- diabetes$Class

tsne <- Rtsne(data_coding)
png(paste(path, "Autoencode3.png"))
plot(tsne$Y, pch = 21, bg = c("red", "blue"))
dev.off()

n <- nrow(data_coding)
rand <- data_coding[order(runif(n)),]
nt <- as.integer(n*0.8)

data_train <- rand[1:nt, ]
data_test <- rand[(nt+1):n, ]

kernels <- c("radial", "sigmoid", "poly")
result <- vector()
class <- data_test$Class
data_test$Class <- NULL

for (k in kernels)
{
  res <- vector()
  for (c in c(1, 10, 50, 100, 500, 1000))
  {
    tmp_res <- vector()
    
    for (i in 1:10)
    {
      model <- svm(Class ~ ., data=data_train, type = "C-classification", cost = c, kernel = k)
      predicted <- predict(model, data_test)
      tbl <- table(predicted, class)
      tmp_res <- append(tmp_res, (tbl[1 , 2] + tbl[2, 1])/ length(class))
    }
    res <- append(res, mean(tmp_res))
  }
  print(res)
  result <- append(result, res)
}

auto_svm <- svm(Class ~ ., data = data_train, type = "C-classification", cost = 10, kernel = "poly")
pred_svm <- predict(auto_svm, data_test)
tbl <- table(pred_svm, class)
res <- (tbl[1 , 2] + tbl[2, 1])/ length(class)
res