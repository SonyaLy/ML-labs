path <- "C://Users//Софья//Desktop//1_курс_1_семестр//Машинное обучение//Лабораторная 3. SVM//"

# Задание 1-------------------------------------------------------------------
library(e1071)

data_train <- read.table(paste(path, "svmdata1.txt", sep =""), stringsAsFactors = TRUE)
data_test <- read.table(paste(path, "svmdata1test.txt", sep =""), stringsAsFactors = TRUE)

X <- data.frame(X1 = data_test$X1, X2 = data_test$X2)
Y <- data.frame(сolor = data_test$Color)

model <- svm(Color ~ ., kernel = "linear", data = data_train, type="C-classification", cost=1)

predicted <- predict(model, X)

table(data_test$Color, predicted)

png(paste(path, "svmdata1.png"), width = 720, height = 480)
plot(model, data = data_test, 
     col = c("lightgreen", "pink"), dataSymbol = "+", svSymbol = "V")
dev.off()

# Задание 2-------------------------------------------------------------------
library(e1071)

data_train <- read.table(paste(path, "svmdata2.txt", sep =""), stringsAsFactors = TRUE)
data_test <- read.table(paste(path, "svmdata2test.txt", sep =""), stringsAsFactors = TRUE)

X <- data.frame(X1 = data_test$X1, X2 = data_test$X2)
Y <- data.frame(colors = data_test$Colors)

C_param <- c(1, 10, 50, 100, 500, 1000)

tbl <- list()

for (c in C_param)
{
  model <- svm(Colors ~ ., kernel = "linear", data=data_train, type="C-classification", cost=c)
  print(summary(model))
  predicted <- predict(model, X)
  tbl <- append(tbl, list(table(data_test$Colors, predicted)))
  png(paste(path, "Svmdata2 C=", c, ".png", sep = ""), width = 720, height = 480)
  plot(model, data_test, col = c("lightgreen", "pink"), dataSymbol = "+", svSymbol = "V")
  dev.off()
}
print(tbl)

# Задание 3--------------------------------------------------------------------
library(e1071)

data <- read.table(paste(path, "svmdata3.txt", sep =""), stringsAsFactors = T)

ratio <- 0.8
n <- nrow(data)
nt <- as.integer(n * ratio)

data_rand <- data[order(runif(n)), ]
data_train <- data_rand[1: nt, ]
data_test <- data_rand[(nt + 1): n, ]

X <- data.frame(X1 = data_test$X1, X2 = data_test$X2)
Y <- data.frame(Colors = data_test$Colors)

Degree = c(1, 5, 10, 25, 50)

tbl <- list()

for (d in Degree){
  
  model <- svm(Colors ~ ., data=data_train, type="C-classification", cost=1, kernel = "poly", degree = d)
  print(summary(model))
  predicted <- predict(model, X)
  tbl <- append(tbl, list(table(data_test$Colors, predicted)))
  png(paste(path, "Svmdata3 Poly D=", d, ".png", sep = ""), width = 720, height = 480)
  plot(model, data_test, col = c("lightgreen", "pink"), dataSymbol = "+", svSymbol = "V")
  dev.off()
  
}

model <- svm(Colors ~ ., data=data_train, type="C-classification", cost=1, kernel = "radial")
predicted <- predict(model, X)
table(data_test$Colors, predicted)
png(paste(path, "Svmdata3 radial.png", sep = ""), width = 720, height = 480)
plot(model, data_test, col = c("lightgreen", "pink"), dataSymbol = "+", svSymbol = "V")
dev.off()


model <- svm(Colors ~ ., data=data_train, type="C-classification", cost=1, kernel = "sigmoid")
predicted <- predict(model, X)
table(data_test$Colors, predicted)
png(paste(path, "Svmdata3 sigmoid.png", sep = ""), width = 720, height = 480)
plot(model, data_test, col = c("lightgreen", "pink"), dataSymbol = "+", svSymbol = "V")
dev.off()

# Задание 4--------------------------------------------------------------------------
library(e1071)

data_train <- read.table(paste(path, "svmdata4.txt", sep =""),  stringsAsFactors = T)
data_test <- read.table(paste(path, "svmdata4test.txt", sep =""), stringsAsFactors = T)

X <- data.frame(X1 = data_test$X1, X2 = data_test$X2)
Y <- data.frame(Colors = data_test$Colors)

table(data_test$Colors)

model <- svm(Colors ~ ., data=data_train, type="C-classification", cost=1, kernel = "radial")
predicted <- predict(model, X)
table(data_test$Colors, predicted)
png(paste(path, "Svmdata4 radial.png"), width = 720)
plot(model, data_test, col = c("lightgreen", "pink"), dataSymbol = "+", svSymbol = "V")
dev.off()


model <- svm(Colors ~ ., data=data_train, type="C-classification", cost=1, kernel = "sigmoid")
predicted = predict(model, X)
table(data_test$Colors, predicted)
png(paste(path, "Svmdata4 sigmoid.png"), width = 720)
plot(model, data_test, col = c("lightgreen", "pink"), dataSymbol = "+", svSymbol = "V")
dev.off()


model <- svm(Colors ~ ., data=data_train, type="C-classification", cost=1, kernel = "poly")
predicted <- predict(model, X)
table(data_test$Colors, predicted)
png(paste(path, "Svmdata4 poly.png"), width = 720)
plot(model, data_test, col = c("lightgreen", "pink"), dataSymbol = "+", svSymbol = "V")
dev.off()

# Задание 5--------------------------------------------------------------
library(e1071)

data_train <- read.table(paste(path, "svmdata5.txt", sep =""), stringsAsFactors = T)
data_test <- read.table(paste(path, "svmdata5test.txt", sep=""), stringsAsFactors = T)

X <- data.frame(X1 = data_test$X1, X2 = data_test$X2)
Y <- data.frame(Colors = data_test$Colors)

table(data_test$Colors)

gamma = c(1, 50)

tbl = list()

for (g in gamma)
{
  model <- svm(Colors ~ ., data=data_train, type="C-classification",
               cost=1, kernel = "poly", gamma = g)
  predicted <- predict(model, X)
  tbl <- append(tbl, list(table(data_test$Colors, predicted)))
  png(paste(path, "Svmdata5 poly, gamma=", g, ".png", sep = ""), width = 720, height = 480)
  plot(model, data_test, col = c("lightgreen", "pink"), dataSymbol = "+", svSymbol = "v")
  dev.off()
}

print(tbl)

tbl <- list()

for (g in gamma)
{
  model <- svm(Colors ~ ., data=data_train, type="C-classification",
               cost=1, kernel = "radial", gamma = g)
  predicted <- predict(model, X)
  tbl <- append(tbl, list(table(data_test$Colors, predicted)))
  png(paste(path, "Svmdata5 radial, gamma=", g, ".png", sep = ""), width = 720, height = 480)
  plot(model, data_test, col = c("lightgreen", "pink"), dataSymbol = "+", svSymbol = "v")
  dev.off()
}

print(tbl)

tbl <- list()

for (g in gamma)
{
  model <- svm(Colors ~ ., data=data_train, type="C-classification",
               cost=1, kernel = "sigmoid", gamma = g)
  predicted <- predict(model, X)
  tbl <- append(tbl, list(table(data_test$Colors, predicted)))
  png(paste(path, "Svmdata5 sigmoid, gamma=", g, ".png", sep = ""), width = 720, height = 480)
  plot(model, data_test, col = c("lightgreen", "pink"), dataSymbol = "+", svSymbol = "v")
  dev.off()
}

print(tbl)

# Задание 6------------------------------------------------------------------
library(e1071)
library(Metrics)

data_train <- read.table(paste(path, "svmdata6.txt", sep =""), stringsAsFactors = TRUE)

x = c(X = data_train$X)
y = c(Y = data_train$Y)

eps <- c(0.1, 0.2, 0.5, 0.8, 1)

tbl <- vector()

for(e in eps)
{
  model = svm(x, y, type="eps-regression", eps=e, kernel = "radial", cost = 1)
  
  predcted = predict(model, x)
  
  png(paste(path, "Regression for eps =", e, ".png", sep = ""), width = 720, height = 480)
  
  plot(x, y, main = paste("Regression for eps =", e))
  
  points(x[model$index], y[model$index], col = "lightblue", pch = 19)
  
  lines(x, predcted, col = "red", lwd = 2)
  lines(x, predcted + model$epsilon, col = "darkgreen")
  lines(x, predcted - model$epsilon, col = "darkgreen")
  
  dev.off()
  
  tbl <- append(tbl, mse(predcted, y))
}

print(tbl)

png(paste(path, "MSE and Eps.png"), width = 720, height = 480)
plot(x = eps, y = tbl, type = "l", xlab = "epsilon", ylab = "mse", col = "red", lwd = 2)
dev.off()