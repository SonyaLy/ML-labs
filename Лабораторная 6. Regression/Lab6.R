# Задание 1---------------------------------------------------------
library(plot3D)
library(pracma)

path <- "C:\\Users\\Софья\\Desktop\\1_курс_1_семестр\\Машинное обучение\\-Лабораторная 6. Regression\\"

reglab <- read.table(paste(path, "reglab1.txt", sep = ""), sep = "\t", header = TRUE)
xy <- reglab[-1]
z <- reglab$z

png(paste(path, "reglab1.png"))
plot(reglab, pch = 19, col = "blue")
dev.off()

f <- lm(z ~ ., reglab)
f
summary(f)

f = lm(x ~ ., data = reglab, model = TRUE)
f
summary(f)
plot(f)

f = lm(y ~ ., data = reglab, model = TRUE)
f
summary(f)
plot(f)

x <- y <- seq(0, 1, 0.05)
r <- meshgrid(x)
z <- f$coefficients[1] + f$coefficients[2] * r$X +
  f$coefficients[3] * r$Y

png(paste(path, "surface.png"))
persp(x,y,z, theta=40, phi=30, r=2, ticktype="detailed", col="blue")
dev.off()


# Задание 2---------------------------------------------------------
library(qpcR)
reglab2 <- read.table(paste(path, 'reglab2.txt', sep =""), sep = '\t', header = TRUE)

png(paste(path, "reglab2.png"))
plot(reglab2, pch = 19, col = "blue")
dev.off()

min_res = Inf

d = ncol(reglab2)-1

for(k in 1:d) 
{
  comb <- combn(reglab2[, -1], k)
  for (s in 1:nrow(comb)) 
  {
    new_data <- data.frame(y = reglab2[, 1], comb[s, ])
    f <- lm(y ~ ., new_data)
    res <- RSS(f)
    if (res < min_res) {
      min_res <- res
      ans <- f
    }
  }
}

min_res
summary(ans)
ans$coefficients

pdf(paste(path, "reglab2_result.pdf"))
plot(ans)
dev.off()

# Задание 3-------------------------------------------------------
cygage <- read.table(paste(path, 'cygage.txt', sep =""), sep = '\t', header = TRUE)

xy <- cygage[-ncol(cygage)]
z <- cygage$Weight
y <- seq(-50, max(cygage[2]) + 50, 1)

# with weights
f <- lm(calAge ~ ., xy, weights = z)
summary(f)
png(paste(path, "With weights.png"))
plot(xy, pch = 19, col = "red", lwd = 4, main = "With weights")
lines(x = y*f$coefficients[2] + f$coefficients[1], y = y, col = "green")
dev.off()
RSS(f)

# Задание 4-----------------------------------------------------
library(datasets)
library(MASS)
library(Metrics)
data(longley)

png(paste(path, "longley.png"), width = 720, height = 720)
plot(longley, lwd = 2, pch = 19, col = "blue")
dev.off()

f <- lm(Employed ~ ., longley)
summary(f)

longl <- longley[-3]

n <- nrow(longl)
longley_rand <- longl[order(runif(n)), ]
longley_train <- longley_rand[1:as.integer(n*0.8), ]
longley_test <- longley_rand[as.integer(n*0.8):n, ]

f <- lm.ridge(
  Employed ~ .,
  longley_train,
  lambda = 10^(-3-0.2*seq(0,25, by=1)),
  model = TRUE,
  x = TRUE,
  y = TRUE
)

x <- longley_train[-6]
y <- longley_train$Employed

my_mse <- vector()

for (c in 1:ncol(f$coef)) 
{
  coefs <- f$coef[, c]
  tmp <- vector()
  for (s in 1:ncol(x)) tmp <- append(tmp, mse(y[s], sum(coefs*x[s, ])))
  tmp <- mean(tmp)
  my_mse <- append(my_mse, tmp)
}

png(paste(path, "train.png"))
plot(x = f$lambda, y = my_mse, pch = 19, col = "darkgreen", lwd = 4, main = "Тренировочная выборка")
dev.off()

x <- longley_test[-6]
y <- longley_test$Employed

my_mse <- vector()

for (c in 1:ncol(f$coef)) {
  coefs <- f$coef[, c]
  tmp <- vector()
  for (s in 1:ncol(x)) tmp <- append(tmp, mse(y[s], sum(coefs*x[s, ])))
  tmp <- mean(tmp)
  my_mse <- append(my_mse, tmp)
}

png(paste(path, "test.png"))
plot(x = f$lambda, y = my_mse, pch = 19, col = "darkred", lwd = 4, main = "Тестовая выборка")
dev.off()

# Задание 5----------------------------------------------------------------
data("EuStockMarkets")
library(mltools)

dax <- EuStockMarkets[, 1]
smi <- EuStockMarkets[, 2]
cac <- EuStockMarkets[, 3]
ftse <- EuStockMarkets[, 4]

png(paste(path, "EuStockMarket.png"))
#plot(EuStockMarkets)
plot(EuStockMarkets[,1], type = "l", col="red",
     xlab="Год", ylab="Котировка")
lines(EuStockMarkets[,2], type = "l", col="blue")
lines(EuStockMarkets[,3], type = "l", col="green")
lines(EuStockMarkets[,4], type = "l", col="black")
dev.off()

name <- time(EuStockMarkets)  # названия строк

all_mse <- vector()
rss <- vector()

# --------------------------------------------------

f <- lm(DAX ~ ., data.frame(time = name, DAX = dax))
png(paste(path, "DAX.png"))
plot(dax, main = "Deutscher Aktienindex (DAX)")
lines(xy.coords(x = name, y = name*f$coefficients[2] +
                  f$coefficients[1]), col = "red")
dev.off()

dax_mse <- mse(dax, name*f$coefficients[2] + f$coefficients[1])
all_mse <- append(all_mse, dax_mse)
rss <- append(rss, RSS(f))

# ------------------------------------------------

f <- lm(SMI ~ ., data = data.frame(time = name, SMI = smi))
png(paste(path, "SMI.png"))
plot(smi, main = "Swiss Market Index (SMI)")
lines(xy.coords(x = name, y = name*f$coefficients[2] +
                  f$coefficients[1]), col = "red")
dev.off()

smi_mse <- mse(smi, name*f$coefficients[2] + f$coefficients[1])
all_mse <- append(all_mse, smi_mse)
rss <- append(rss, RSS(f))

# -------------------------------------------------

f <- lm(CAC ~ ., data = data.frame(time = name, CAC = cac))
png(paste(path, "CAC.png"))
plot(cac, main = "Cotation Assistée en Continu (CAC)")
lines(xy.coords(x = name, y = name*f$coefficients[2] +
                  f$coefficients[1]), col = "red")
dev.off()
cac_mse <- mse(cac, name*f$coefficients[2] + f$coefficients[1])
all_mse <- append(all_mse, cac_mse)
rss <- append(rss, RSS(f))

#-------------------------------------------------

f <- lm(FTSE ~ ., data = data.frame(time = name, FTSE = ftse))
png(paste(path, "FTSE.png"))
plot(ftse, main = "Financial Times Stock Exchange (FTSE)")
lines(xy.coords(x = name, y = name*f$coefficients[2] +
                  f$coefficients[1]), col = "red")
dev.off()
ftse_mse <- mse(ftse, name*f$coefficients[2] + f$coefficients[1])
all_mse <- append(all_mse, ftse_mse)
rss <- append(rss, RSS(f))

#----------------------------------------------

overall = (dax + smi + cac + ftse) / 4
f <- lm(overall ~ ., data = data.frame(time = name, overall = overall))
png(paste(path, "Overall.png"))
plot(overall, main = "Overall")
lines(xy.coords(x = name, y = name*f$coefficients[2] +
                  f$coefficients[1]), col = "red")
dev.off()
overall_mse <- mse(overall, name*f$coefficients[2] + f$coefficients[1])
all_mse <- append(all_mse, overall_mse)
rss <- append(rss, RSS(f))


# Задание 6--------------------------------------------------------------------
data(JohnsonJohnson)

png(paste(path, "JohnsonJohnson.png"))
plot(time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
     JohnsonJohnson[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
     type = "l", col="red", main = "Изменение прибыли во времени", xlab="Год",
     ylab="Пибыль")
lines(time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
      JohnsonJohnson[seq(from = 2, to = length(JohnsonJohnson), by = 4)],
      type = "l", col="blue")
lines(time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
      JohnsonJohnson[seq(from = 3, to = length(JohnsonJohnson), by = 4)],
      type = "l", col="green")
lines(time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
      JohnsonJohnson[seq(from = 4, to = length(JohnsonJohnson), by = 4)],
      type = "l", col="black")
dev.off()

name <- time(JohnsonJohnson)
f <- lm(JohnsonJohnson ~ .,
        data = data.frame(time = name,
                          JohnsonJohnson = JohnsonJohnson))

png(paste(path, "JohnsonJohnson.png"))
plot(JohnsonJohnson, main = "JohnsonJohnson")
lines(xy.coords(x = name, y = name*f$coefficients[2] +
                  f$coefficients[1]), col = "red")
dev.off()

mean(predict.lm(f, data.frame(time = time(ts(start = 2016, end = 2017, frequency = 365)))))

qtr_mean <- vector()

for (i in 1:4) 
{
  name <- time(JohnsonJohnson)[seq(i,length(JohnsonJohnson), 4)]
  qtr <- JohnsonJohnson[seq(i, length(JohnsonJohnson), 4)]
  quater <- data.frame(time = name, qtr = qtr)
  f <- lm(qtr ~ ., data = quater)
  png(paste(path, "Quater №", i, ".png", sep = ""))
  plot(quater, main = paste("Quater №", i, sep = ""), pch = 19, lwd = 4, col = "lightgreen")
  lines(xy.coords(x = name, y = name*f$coefficients[2] + f$coefficients[1]), col = "red")
  dev.off()
  qtr_mean <- append(qtr_mean, mean(predict.lm(f, data.frame(time = time(ts(start = c(2016, i), end = c(2016, i), frequency = 4))))))
}
qtr_mean

all = lm(JohnsonJohnson[seq(from = 1, to = length(JohnsonJohnson), by = 4)]+
           JohnsonJohnson[seq(from = 2, to = length(JohnsonJohnson), by = 4)]+
           JohnsonJohnson[seq(from = 3, to = length(JohnsonJohnson), by = 4)]+
           JohnsonJohnson[seq(from = 4, to = length(JohnsonJohnson), by = 4)]~
           time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
         JohnsonJohnson)
all

# Задание 7 ----------------------------------------------------------------
library(qpcR)
data(sunspot.year)

name <- time(sunspot.year)
f <- lm(sun ~ ., data.frame(time = name, sun = sunspot.year))
png(paste(path, "Sunspot.png"))
plot(sunspot.year, main = "Sunspot year", col = "brown")
lines(xy.coords(x = name, y = name*f$coefficients[2] +
                  f$coefficients[1]), col = "green")
dev.off()
RSS(f)

# Задание 8 ---------------------------------------------------------------
gas <- read.csv(paste(path, "UKgas.csv", sep = ""), sep = ',', row.names = 1)

name = gas$time
f <- lm(UKgas ~ ., data = gas)
png(paste(path, "UKgas.png"))
plot(gas, main = "Ukgas", pch = 19, col = "lightgreen")
lines(xy.coords(x = name, y = name*f$coefficients[2] +
                  f$coefficients[1]), col = "red", lwd = 2)
dev.off()
summary(f)

mean(predict.lm(f, data.frame(time = time(ts(start = 2016, end = 2017, frequency = 365)))))

qtr_mean <- vector()

for (i in 1:4) 
{
  name <- gas$time[seq(i, length(gas$time), 4)]
  qtr <- gas$UKgas[seq(i, length(gas$UKgas), 4)]
  quater <- data.frame(time = name, qtr = qtr)
  f <- lm(qtr ~ ., data = quater)
  png(paste(path, "Quater №", i, "(Ukgas).png", sep = ""))
  plot(quater, main = paste("Quater №", i, "(Ukgas)", sep = ""), pch = 19, lwd = 4, col = "lightgreen")
  lines(xy.coords(x = name, y = name*f$coefficients[2] + f$coefficients[1]), col = "red")
  dev.off()
  qtr_mean <- append(qtr_mean, mean(predict.lm(f, data.frame(time = time(ts(start = c(2016, i), end = c(2016, i), frequency = 4))))))
}
qtr_mean

# Задание 9 ---------------------------------------------------------------
data(cars)
f <- lm(dist ~ ., data = cars)
png(paste(path, "cars.png"))
plot(cars, main = "Cars", pch = 19, lwd = 2, col = "lightgreen")
lines(x = cars$speed,
      y = cars$speed*f$coefficients[2] +
        f$coefficients[1], col = "red")
dev.off()
predict.lm(f, data.frame(speed = 40))
