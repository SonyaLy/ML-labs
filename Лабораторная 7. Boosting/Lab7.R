install.packages("mlbench")
install.packages("rpart")
install.packages("adabag")
install.packages("caret")
install.packages("robustbase")
install.packages("digest")


library(mlbench)
library(rpart)
library(adabag)

path <- "C:\\Users\\Софья\\Desktop\\1_курс_1_семестр\\Машинное обучение\\Лабораторная 7. Boosting\\"

# Задание 1 #########################################

data(Vehicle)

n <- nrow(Vehicle)
n_train <- as.integer(n*0.7)
Vehicle_rand <- Vehicle[order(runif(n)), ]
Vehicle_train <- Vehicle_rand[1:n_train, ]
Vehicle_test <- Vehicle_rand[n_train:n, ]

tree_num <- seq(1, 301, 10)

error <- vector()
for (k in tree_num)
{
  clf <- boosting(Class ~ ., data = Vehicle_train, mfinal = k)
  err <- predict(clf, Vehicle_test)$error
  error <- append(error, err)
}

max(error)
min(error)
mean(error)

png(paste(path, 'adaboost.png'))
plot(x = tree_num, y = error, xlab = "Число деревьев", pch = 2, lwd = 2, col = "green", main = "Adaboost")
dev.off()

# Задание 2 ##########################################
data("Glass")

n <- nrow(Glass)
n_train <- as.integer(n*0.7)
Glass_rand <- Glass[order(runif(n)), ]
Glass_train <- Glass_rand[1:n_train, ]
Glass_test <- Glass_rand[n_train:n, ]

error <- vector()

tree_num <- seq(1, 201, 10)

for (k in tree_num)
{
  err <- vector()
  #for(i in 1:4)
  #{
    clf <- bagging(Type ~ ., data = Glass_train, mfinal = k)
    err <- append(err, predict(clf, Glass_test)$error)
  #}
  #error <- append(error, mean(err))
  error <- append(error, err)
}

max(error)
min(error)
mean(error)

png(paste(path, 'bagging1.png'))
plot(x = tree_num, y = error, xlab = "Число деревьев", pch = 2, lwd = 2, col = "green", main = "Bagging")
dev.off()

# Задание 3 ##########################################
library(dplyr)

knn_w <- function(target, train, k, w) return(list(target = target, train = train, levels = levels(train[, target]), k = k, w = w))

knn_w_predicted <- function(clfier, testdata) {
  n <- nrow(testdata)
  pred <- rep(NA_character_, n)
  trainlabels <- clfier$train[, clfier$target]
  
  train <- clfier$train[, !(names(clfier$train) %in% clfier$target)]
  test <- testdata[, !(names(testdata) %in% clfier$target)]
  
  for (i in 1:n) {
    n_number <- order(apply(train, 1, function(x)
      sum((test[i,] - x)^2)))[1:clfier$k]
    
    myfreq <- data.frame(names = clfier$levels,
                         freq = rep(0, length(clfier$levels)))
    for (t in n_number) {
      myfreq[myfreq$names == trainlabels[t], ][2] <- myfreq[myfreq$names == trainlabels[t], ][2] + clfier$w[t]
    }
    most_frequent <- clfier$levels[myfreq$freq == max(myfreq$freq)]
    pred[i] <- sample(most_frequent, 1)
  }
  
  factor(pred, levels = levels(trainlabels))
}

knn_boosting <- function(target,
                         data,
                         k = 11,
                         mfinal = 1,
                         ...) {
  n <- nrow(data)
  w <- rep(1/n, each = n)
  
  classifiers <- list()
  
  alphas <- vector()
  for (t in 1:mfinal) {
    clfier <- knn_w(target, train = data, k = k, w)
    knn_predicted <- knn_w_predicted(clfier, data)
    error <- vector()
    for (i in 1:n) {
      if (data[[target]][i] != knn_predicted[i]) error <- append(error, w[i])
    }
    
    if (sum(error) >= 0.5) {
      break()
    }
    
    classifiers[[t]] <- clfier
    alphas[[t]] <- log((1 - sum(error)) / sum(error)) / 2
    for (i in 1:n) {
      if (knn_predicted[i] != data[[target]][i]) 
      {
        w[i] <- w[i]*exp(alphas[[t]])
      } else{
        w[i] <- w[i]*exp(-alphas[[t]])
      }
    }
  }
  
  result <- list()
  
  result$classifiers <- classifiers
  result$alphas <- alphas
  result$levels <- levels(data[, target])
  return(result)
}

boosting_pred <- function(clfier, testdata) {
  n <- nrow(testdata)
  pred = rep(NA_character_, n)
  
  for (i in 1:n) {
    myfreq <- data.frame(names = clfier$levels,
                         freq = rep(0, length(clfier$levels)))
    
    for (j in 1:length(clfier$classifiers)) {
      prediction <- knn_w_predicted(clfier$classifiers[[j]], testdata[i, ])
      myfreq[myfreq$names == prediction, ][2] <- myfreq[myfreq$names == prediction, ][2] + clfier$alphas[j]
    }
    
    most_frequent = clfier$levels[myfreq$freq == max(myfreq$freq)]
    pred[i] <- sample(most_frequent, 1)
  }
  factor(pred, levels = clfier$levels)
}

#----------------------------------------------------------------------------
library(mlbench)
library(adabag)
data("Glass")


n <- nrow(Glass)
n_train <- as.integer(n*0.7)
Glass_rand <- Glass[order(runif(n)), ]
Glass_train <- Glass_rand[1:n_train, ]
Glass_test <- Glass_rand[n_train:n, ]


Glass_rpart <- rpart(Type ~ ., data = Glass_train, maxdepth = 5)

Glass_rpart_pred <- predict(Glass_rpart, Glass_test, type = 'class')
tbl_rpart <- table(Glass_rpart_pred, Glass_test$Type)
1 - (sum(diag(tbl_rpart)) / sum(tbl_rpart))

clfier <- knn_boosting('Type', Glass_train, mfinal = 4)
pred <- boosting_pred(clfier, Glass_test)

tbl_knn <- table(Glass_test$Type, pred)

1 - sum(diag(tbl_knn)) / sum(tbl_knn)

#----------------------------------------------------------------------------

data(Vehicle)

n <- nrow(Vehicle)
n_train <- as.integer(n*ratio)
Vehicle_rand <- Vehicle[order(runif(n)), ]
Vehicle_train <- Vehicle_rand[1:n_train, ]
Vehicle_test <- Vehicle_rand[n_train:n, ]


Vehicle_rpart <- rpart(Class ~ ., data = Vehicle_train, maxdepth = 5)
Vehicle_rpart_pred <-
  predict(Vehicle_rpart, newdata = Vehicle_test, type = 'class')
tbl_rpart <- table(Vehicle_rpart_pred, Vehicle_test$Class)
1 - (sum(diag(tbl_rpart)) / sum(tbl_rpart))

clfier <- knn_boosting('Class', Vehicle_train, mfinal = 1)
pred <- boosting_pred(clfier, Vehicle_test)
tbl_knn <- table(Vehicle_test$Class, pred)
1 - sum(diag(tbl_knn)) / sum(tbl_knn)
