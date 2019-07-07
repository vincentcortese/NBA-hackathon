library(fBasics)
library(quantmod)
library(TSA)
library(forecast)
library(fUnitRoots)
library(fTrading)
library(lubridate)

setwd("C:/Users/Vincent/Documents/0 - Stevens/random/NBA-hackathon-master/NBA-hackathon-master")

nba <- read.csv("training_set.csv")

adfTest(nba$Engagements, lag = 1, type = 'ct')

engage <- nba$Engagements
engage1 <- nba$Engagements[2:length(nba$Engagements)]
engage2 <- nba$Engagements[1:(length(nba$Engagements) - 1)]

# Simple Return
engage.simple.return <- (engage1 - engage2)/engage2

plot(engage.simple.return, type = 'l')
hist(engage.simple.return)
engage.simple.return

adfTest(engage.simple.return, lag = 1, type = 'c')

acf(engage.simple.return)
pacf(engage.simple.return)

ar(engage.simple.return, method = 'ols')
ar(engage.simple.return, method = 'yule-walker')


# eacf(engage.simple.return, ar.max = 15, ma.max = 15)
#
# AIC_matrix <- matrix(NA, nrow = 10, ncol = 10)
# # After running it once, we see that the min is at (9,9), so we will limit our method to that
#
# for(i in 0 : 9){
#   for(j in 0 : 9)
#   {
#     tem <- try(arima(engage.simple.return, order = c(i, 0, j))$aic)
#     if(inherits(tem, "try-error"))
#     {
#       next
#     }else
#     {
#       AIC_matrix[i+1, j+1] <- tem
#     }
#   }
# }
# AIC_matrix
# min(na.omit(AIC_matrix))
# which(AIC_matrix == min(na.omit(AIC_matrix)), arr.ind = TRUE)

m1 <- arima(engage.simple.return, order = c(1,0,5))
tsdiag(m1)
predict(m1, 10)

m1.pred <- coredata(predict(m1, 1000)$pred)

# first predicted number from regression is 421,181.9

nba.pred <- read.csv("holdout_set.csv")

nba.pred$Engagements[1] = 421181.9

for(i in 2:length(nba.pred[,1])){
  nba.pred$Engagements[i] <- nba.pred$Engagements[i - 1] * m1.pred[i-1]
}

m1.pred

