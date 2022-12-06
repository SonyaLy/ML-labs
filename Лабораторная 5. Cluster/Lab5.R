# Задание 1 ##########################################
path <- "C:\\Users\\Софья\\Desktop\\1_курс_1_семестр\\Машинное обучение\\Лабораторная 5. Cluster\\"

library(cluster)
data(pluton)

iterations <- c(3, 8, 13, 18)

for (i in iterations) {
  cl <- kmeans(pluton, iter.max = i, centers = 3)
  png(paste(path, "Kmeans function with", i, "iterations.png"))
  plot(pluton,
       col = cl$cluster,
       main = paste("Kmeans function with", i, "iterations"))
  dev.off()
}

# Задание 2 ##########################################
#------------------------------------------------------------
library(cluster)
size = 100
x1 <- matrix(c(rnorm(size, mean = -40, sd = 10), rnorm(size, mean = 0, sd = 2)), ncol = 2)
x2 <- matrix(c(rnorm(size, mean = -30, sd = 5), rnorm(size, mean = 0, sd = 5)), ncol = 2)
x3 <- matrix(c(rnorm(size, mean = -20, sd = 2), rnorm(size, mean = 0, sd = 10)), ncol = 2)

colnames(x) <- c("x", "y")
x <- rbind(x1, x2, x3)

png(paste(path, "Data.png"))
plot(x[, 1], x[, 2])
dev.off()

for (i in c(TRUE, FALSE)) {
  metrics <-  c("euclidean", "manhattan")
  
  for (m in metrics) {
    cl <- clara(x, 3, stand = i, metric = m)
    png(paste(path, "Clustering with method", m, "and", i, ".png"))
    plot(x, col = cl$cluster, main = paste("Clustering with method", m, "and standartization =", i))
    dev.off()
  }
}

# Задание 3 ##########################################
#--------------------------------------------------------------
library(cluster)
data(votes.repub)
png(paste(path, "Votes.png"), width = 720)
plot(agnes(votes.repub))
dev.off()

# Задание 4 ##########################################
#-------------------------------------------------------------
library(cluster)
data(animals)
png(paste(path, "Animals.png"))
plot(agnes(animals))
dev.off()

# Задание 5 ##########################################
#-------------------------------------------------------------
library(cluster)
seed <- read.csv(paste(path, "seeds_dataset.txt", sep = ""), header = FALSE, sep = "\t")
seed <- na.omit(seed)
result <- seed[, 8]
seed <- seed[,-8]

km <- kmeans(seed, centers = 3)
png(paste(path, "Cluster.png"))
plot(seed, col = km$cluster)
dev.off()

tbl1 <- table(km$cluster, result)

print(sum(diag(tbl1)) / sum(tbl1))

cl <- clara(seed, 3)
png(paste(path, "Clara.png"))
plot(seed, col = cl$cluster)
dev.off()
tbl <- table(cl$cluster, result)

print(sum(diag(tbl)) / sum(tbl))