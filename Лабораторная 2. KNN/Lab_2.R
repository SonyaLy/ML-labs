# Задание 1 ####################################################
path <- "C:\\Users\\Софья\\Desktop\\1_курс_1_семестр\\Машинное обучение\\Лабораторная 2. KNN\\"
install.packages("kknn")
library(kknn)
# получили датафрейм
A_raw <- read.table(paste(path, "Tic_tac_toe.txt", sep = ""), sep = ",", stringsAsFactors = TRUE)
# количество строк
n <- nrow(A_raw)

# задаем параметры обучения
learning <- seq(0.1, 0.95, 0.01)
res_vec <- vector()
for (learn_ratio in learning) {
  nt <- as.integer(n * learn_ratio)
  exp_res <- vector()
  set.seed(1121)
  for (j in 1:10) {
    A_rand <- A_raw[order(runif(n)), ]
    A_train <- A_rand[1:nt, ]
    A_test <- A_rand[(nt + 1):n, ]
    
    A_classifier <- kknn(V10 ~ .,
                         A_train,
                         A_test,
                         k=round(sqrt(nt), digits = 0),
                         distance = 1,
                         kernel = "triangular")
    
    res <- table(A_classifier$fitted.values, A_test$V10)
    
    exp_res <- append(exp_res, (res[1, 1] + res[2, 2]) / sum(res))
  }
  res_vec <- append(res_vec, mean(exp_res))
}

x <- learning
y <- res_vec
max(y)
mean(y)
spline = smooth.spline(x, y, spar = 0.6)
png(paste(path, "Крестики-нолики1.png"))
plot (x, y,
  xlab = "Доля обучающей выборки" ,
  ylab = "Доля совпадений в предсказаниях",
  col = "green",
  pch = 19,
  main = "Крестики-нолики, KNN"
)
lines(spline, col = "red")
dev.off()
#-----------------------------------------------------------------
library(kernlab)
data(spam)

learning <- seq(10, 1000, 10)
res_vec <- vector()
for (learn_ratio in learning) {
  exp_res <- vector()
  for (j in 1:10)
  {
    idx <- sample(1:nrow(spam), learn_ratio)
    spamtrain <- spam[-idx, ]
    spamtest <- spam[idx, ]
    
    model <- kknn(type ~ .,
                  train = spamtrain,
                  test = spamtest,
                  distance = 1,
                  k=round(sqrt(learn_ratio), digits = 0),
                  kernel = "triangular")
    
    res <- table(model$fitted.values, spamtest$type)
    
    exp_res <- c(exp_res, (res[1, 1] + res[2, 2]) / sum(res))
  }
  res_vec <- c(res_vec, mean(exp_res))
}

x <- learning
y <- res_vec
spline = smooth.spline(x, y, spar = 0.6)
png(paste(path, "Спам, KNN (1).png"))
plot (x, y,
  xlab = "Размер обучающей выборки",
  ylab = "Доля совпадений в предсказаниях",
  col = "green",
  pch = 19,
  main = "Спам, KNN"
)
lines(spline, col = "red")
dev.off()

max(y)
mean(y)

# Задание 2 ####################################################
#----------------------------------------------------------------
library(kknn)
library(kernlab)
data("glass")

# Id number
glass <- glass[, -1]
glass$Type

learn_ratio = 0.8
n <- nrow(glass)
nt <- as.integer(n * learn_ratio)
glass_rand <- glass[order(runif(n)), ]
glass_train <- glass_rand[1:nt, ]
glass_test <- glass_rand[(nt + 1):n, ]

kernels <- c(
  "rectangular",
  "triangular",
  "epanechnikov",
  "biweight",
  "triweight",
  "cos",
  "inv",
  "gaussian",
  "rank",
  "optimal")

for (k in kernels) {
  best.rect <- train.kknn(Type ~ .,
                          glass_train,
                          kmax = 10,
                          distance = 2,
                          kernel = k)
  mis.rect <- best.rect$MISCLASS
  y<-mis.rect[!duplicated(mis.rect)]
  i <- order(mis.rect)
  
  png(paste(path, paste(k, "kernels.png")))
  plot(seq(1, 10, 1),
       mis.rect,
       xlab="k",
       ylab = "Ошибка классификации",
       col="blue",
       axes = FALSE)
  axis(side = 1,
       at = seq(1, 10, 1),
       labels = seq(1, 10, 1))
  axis(side = 2,
       at = y[i],
       labels = round(y[i], digits = 4),
       las = 0)
  box()
  dev.off()
}

#---------------------------------------------------------------

distances = 1:10
res_vec <- vector()
for (d in distances) {
  model <- kknn(
    Type ~ .,
    train = glass_train,
    test = glass_test,
    distance = d,
    k=6,
    kernel = "rank"
  )
  res <- table(model$fitted.values, glass_test$Type)
  res_vec <- append(res_vec, (sum(diag(res)) / sum(res)))
}

x <- distances
y <- res_vec
png(paste(path, "Distance parameter.png"))
plot(x, y, xlab = "Distance parameter", ylab = "Доля верных предсказаний")
dev.off()

#---------------------------------------------------------------

example <- data.frame(
  RI = 1.516,
  Na = 11.7,
  Mg = 1.01,
  Al = 1.19,
  Si = 72.59,
  K = 0.43,
  Ca = 11.44,
  Ba = 0.02,
  Fe = 0.1
)

res <- kknn(
  Type ~ .,
  train = glass_train,
  example,
  distance = 2,
  k = 6,
  kernel = "biweight")

print(res$fitted.values)
print(res$prob)

#---------------------------------------------------------------

res_vec <- vector()
for (i in 1:length(example)) {
  test_ex <- example
  test_ex[1, i] = 0
  res <- kknn(
    Type ~ .,
    train = glass_train,
    test_ex,
    k=6,
    distance = 2,
    kernel = "rank"
  )
  res_vec <- append(res_vec, res$prob[4])
}

x <- 1:length(colnames(example))
y <- res_vec
df <- data.frame(x, y)
png(paste(path, "Признаки.png"))
ggplot(df) +
  geom_point(aes(x = x, y = y), color = "darkgreen", lwd = 3) +
  scale_x_discrete(limit = x, labels = colnames(example)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15)) +
  labs(x = "Признаки", y = "Доля верных предсказаний")
dev.off()

# Задание 3 ####################################################
#-----------------------------------------------------------------
install.packages("ggplot2")
library(ggplot2)
SVM_Train <- read.table(paste(path, "svmdata4.txt", sep = ""),
                        sep = "\t",
                        stringsAsFactors = TRUE)
SVM_Test <- read.table(paste(path, "svmdata4test.txt", sep = ""),
                       sep = "\t",
                       stringsAsFactors = TRUE)

plot(SVM_Train$X1, SVM_Train$X2, pch=21, bg=c("red","blue") [unclass(SVM_Train$Colors)],  main="My train data", xlab = "X1", ylab="X2")

n <- nrow(SVM_Train)

png(paste(path, "Task3.png"))
ggplot(SVM_Train, aes(x = X1, y = X2, color = Colors)) +
  geom_point(size = 2) +
  theme_minimal()
dev.off()

model <- train.kknn(
  Colors ~ .,
  data = SVM_Train,
  #ks = 1:14,
  kmax=20,
  kernel = "optimal",
  distance = 2
)
png(paste(path, "Optimal.png"))
plot(model)
dev.off()

# Задание 4 ####################################################
#---------------------------------------------------------------
T_train<-read.csv(paste(path, "Titanic_train.csv", sep = ""),
                  stringsAsFactors = TRUE)
T_test<-read.csv(paste(path, "Titanic_test.csv", sep = ""),
                 stringsAsFactors = TRUE)
T_train[0,] #метки признаков

passengers <- T_test$PassengerId
#удаляем признаки PassengerId, Name,Ticket, Cabin, Embarked
T_train <- T_train[,-c(1,4,5,10,12,13)]
T_test <- T_test[,-c(1,3,4,9,11,12)]

#меняем пол на численные метки 0-м, 1-ж
T_train$Sex <- sapply(as.character(T_train$Sex), switch, 'male' = 0, 'female' = 1)
T_test$Sex <- sapply(as.character(T_test$Sex), switch, 'male' = 0, 'female' = 1)

#записываем вместо значения NaS среднее значение Age
train_age<-na.omit(T_train$Age)
T_train$Age[is.na(T_train$Age)] <- mean(train_age)

test_age<-na.omit(T_test$Age)
T_test$Age[is.na(T_test$Age)] <- mean(test_age)

#записываем вместо значения NaS среднее значение Fare
test_fare<-na.omit(T_test$Fare)
T_test$Fare[is.na(T_test$Fare)] <- mean(test_fare)

train_fare<-na.omit(T_train$Fare)
T_train$Fare[is.na(T_train$Fare)] <- mean(train_fare)

quantity_of_data<-dim(T_train)[1]
model <- train.kknn(Survived ~ .,
                    data = T_train,
                    kmax=floor(sqrt(quantity_of_data)),
                    distance = 1)
model

model_titanik <- kknn(Survived ~ .,
                      T_train,
                      T_test,
                      k = 37,
                      kernel = "optimal",
                      distance = 1)

fit <- round(fitted(model_titanik))

prop.table(table(T_train$Survived))
prop.table(table(fit))