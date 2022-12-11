install.packages("tree")
install.packages("maptree")
install.packages("mltools")

library(tree)
library(mlbench)
library(maptree)
library(mltools)

path <- "C:\\Users\\Софья\\Desktop\\1_курс_1_семестр\\Машинное обучение\\-Лабораторная 4. Tree\\"

# Задание 1 #########################################################
data("Glass")

ratio <- 0.8
n <- nrow(Glass)
nt <- as.integer(n * ratio)

glass_rand <- Glass[order(runif(n)), ]
glass_train <- glass_rand[1: nt, ]
glass_test <- glass_rand[(nt + 1): n, ]

#------------------
tree1 <- tree(Type ~ ., data = glass_train)

png(paste(path, "glass_tree.png"), width = 720, height = 480)
draw.tree(tree1, cex = 0.5)
dev.off()

pred1 <- predict(tree1, glass_test)

s <- vector()

for(i in 1:nrow(pred1))
{
  s <- append(s, mse(1, pred1[i, glass_test$Type[i]]))
}

1 - mean(s)

#------------------
tree2 <- prune.tree(tree1, 10)

png(paste(path, "glass_tree_optim.png"), width = 720, height = 480)
draw.tree(tree2, cex=0.5)
dev.off()

pred2 <- predict(tree2, glass_test)

s <- vector()

for(i in 1:nrow(pred2))
{
  s <- append(s, mse(1, pred2[i, glass_test$Type[i]]))
}

1 - mean(s)

#------------------
tree3 <- snip.tree(tree1, nodes = c(12))

png(paste(path, "glass_tree_snip.png"), width = 720, height = 480)
draw.tree(tree3, cex=0.5)
dev.off()

pred3 <- predict(tree3, glass_test)

s <- vector()

for(i in 1:nrow(pred3))
{
  s <- append(s, mse(1, pred3[i, glass_test$Type[i]]))
}

1 - mean(s)

predict(
  tree1,
  data.frame(
    RI = 1.516,
    Na = 11.7,
    Mg = 1.01,
    Al = 1.19,
    Si = 72.59,
    K = 0.43,
    Ca = 11.44,
    Ba = 0.02,
    Fe = 0.1))


# Задание 2 #########################################################
library(DAAG)
data(spam7)

ratio <- 0.8
n <- nrow(spam7)
nt <- as.integer(n * ratio)

spam_rand <- spam7[order(runif(n)), ]
spam_train <- spam_rand[1: nt, ]
spam_test <- spam_rand[(nt + 1): n, ]

tree1 <- tree(yesno ~ ., data = spam_train)
png(paste(path, "spam_tree.png"), width = 720, height = 480)
draw.tree(tree1, cex=0.8)
dev.off()

pred1 <- predict(tree1, spam_test)

s <- vector()

for(i in 1:nrow(pred1))
{
  s <- append(s, mse(1, pred1[i, spam_test$yesno[i]]))
}

1-mean(s)

#--------------
tree2 <- prune.tree(tree1, method = "misclass", k=1)
png(paste(path, "spam_tree_prune.png"), width = 720, height = 480)
draw.tree(tree2, cex=0.8)
dev.off()

pred2 <- predict(tree2, spam_test)

s <- vector()

for(i in 1:nrow(pred2))
{
  s <- append(s, mse(1, pred2[i, spam_test$yesno[i]]))
}

1-mean(s)

prune.tree(tree1, method = "misclass")$k
draw.tree(prune.tree(tree1, k = 0), cex=0.7)
draw.tree(prune.tree(tree1, k = 11), cex=0.7)
draw.tree(prune.tree(tree1, k = 105), cex=0.7)
draw.tree(prune.tree(tree1, k = 685), cex=0.7)

# Задание 3 #########################################################
library(e1071)

data(nsw74psid1)

n <- nrow(nsw74psid1)

ratio <- 0.8
nt <- as.integer(n*ratio)

data_rnd <- nsw74psid1[order(runif(n)), ]
nsw_train <- data_rnd[1:nt, ]
nsw_test <- data_rnd[(nt+1):n, ]

tree_nsw <- tree(re78 ~ ., nsw_train)

png(paste(path, "nsw_tree.png"), width = 720, height = 480)
draw.tree(tree_nsw, cex = 0.8)
dev.off()

pred1 <- predict(tree_nsw, nsw_test[, !colnames(nsw_test) %in% c("re78")])

mean(mse(sum(pred1), nsw_test$re78))

svm_nsw <- svm(re78 ~ ., nsw_train, type = "eps-regression")

pred2 <- predict(svm_nsw, nsw_test[, !colnames(nsw_test) %in% c("re78")])

mean(mse(sum(pred2), nsw_test$re78))

# Задание 4 #########################################################

library(mlbench)
library(maptree)

Lenses <- read.table(paste(path, "Lenses.txt", sep = ""), header = FALSE)
Lenses$V1 <- NULL

len_tree1 <- tree.control(nrow(Lenses), mincut = 2, minsize = 6)
len_tree2 <- tree(V6 ~ ., Lenses, control = len_tree1)

png(paste(path, "lenses.png"))
draw.tree(len_tree2)
dev.off()

print(predict(len_tree2, data.frame(V2 = 2, V3 = 1, V4 = 2, V5 = 1)))

# Задание 5 #########################################################

data(Glass)

glass <- Glass[, -1]

glass_tree <- tree(Type ~ ., Glass)
glass_tree1 <- prune.tree(glass_tree, k = 10)

png(paste(path, "glass_predict.png"), width = 720, height = 480)
draw.tree(glass_tree1, cex = 0.7)
dev.off()

predict(
  glass_tree1,
  data.frame(
    RI = 1.516,
    Na = 11.7,
    Mg = 1.01,
    Al = 1.19,
    Si = 72.59,
    K = 0.43,
    Ca = 11.44,
    Ba = 0.02,
    Fe = 0.1))

# Задание 6 #########################################################

svm_train <- read.table(paste(path, "svmdata4.txt", sep = ""), stringsAsFactors = TRUE)
svm_test <- read.table(paste(path, "svmdata4test.txt", sep = ""), stringsAsFactors = TRUE)

tree1 <- tree(Colors ~ ., data = svm_train)

png(paste(path, "svmdata4.png"), width = 720, height = 480)
draw.tree(tree1, cex=0.8)
dev.off()

pred1 <- predict(tree1, svm_test)

s <- vector()

for(i in 1:nrow(pred1)){
  s <- append(s, mse(1, pred1[i, svm_test$Colors[i]]))
}

1 - mean(s)

tree2 <- prune.tree(tree1, 10)

png(paste(path, "svmdata4_optim.png"), width = 720, height = 480)
draw.tree(tree2, cex=0.8)
dev.off()

pred1 <- predict(tree2, svm_test)

s <- vector()

for(i in 1:nrow(pred1)){
  s <- append(s, mse(1, pred1[i, svm_test$Colors[i]]))
}

1 - mean(s)

# Задание 7 #########################################################

T_train <- read.csv(paste(path, "Titanic_train.csv", sep = ""), stringsAsFactors = TRUE)
T_test <- read.csv(paste(path, "Titanic_test.csv", sep = ""),  stringsAsFactors = TRUE)

T_train$Name <- paste(T_train$Name, sep = ",", T_train$X)
T_train <- T_train[,-5]

T_test$Name <- paste(T_test$Name, sep = ",", T_test$X)
T_test <- T_test[,-4]

null_names <- c("PassengerId", "Name", "Ticket", "Cabin")

T_train[, colnames(T_train) %in% null_names] <- NULL
T_test[, colnames(T_test) %in% null_names] <- NULL

nt <- nrow(T_train)
n <- nt + nrow(T_test)

T_classifier <- tree(Survived ~ ., data = T_train)

png(paste(path, "Titanic.png"), width = 720, height = 480)
draw.tree(T_classifier)
dev.off()

T_predicted <- predict(T_classifier, T_test)

survived <- length(T_predicted[T_predicted > 0.5]) / length(T_predicted)
dead <- length(T_predicted[T_predicted <= 0.5]) / length(T_predicted)
survived
dead

prop.table(table(T_train$Survived))
