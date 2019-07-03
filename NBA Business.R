install.packages("lubridate")
library(lubridate)

# NBA Business problem 

# Regression info: 
# https://towardsdatascience.com/key-types-of-regressions-which-one-to-use-c1f25407a8a4

setwd("C:/Users/Vincent/Documents/0 - Stevens/random")

nba <- read.csv("training_set.csv")
# ==================================== FUNCTION ========================

# This loop adds the Caption length from the Description
nba$Caption.length <- NA
for(i in 1:length(nba[,1])){
  nba$Caption.length[i] <- nchar(as.character(nba$Description[i]))
  
}

# Now have to adjust the times based on this 
# https://blog.hubspot.com/marketing/instagram-best-time-post

# This finds the day of the week given the time
nba$Day <- NA
for(i in 1:length(nba[,1])){
  nba$Day[i] <- weekdays(strptime(nba$Created[i], format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))
}


# Assigns numbers for each day of the week. 1 is Sunday, 7 is Saturday
nba$Day.num <- NA
for(i in 1:length(nba[,1])){
  nba$Day.num[i] <- wday(nba$Created[i])
}

# This quantifies the category of post
nba$Type.num <- NA
for(i in 1:length(nba[,1])){
  if(nba$Type[i] == "Video")(nba$Type.num[i] = 1)
  if(nba$Type[i] == "Photo")(nba$Type.num[i] = 2)
  if(nba$Type[i] == "Album")(nba$Type.num[i] = 3)
}

# summary(lm(nba$Engagements ~ nba$Followers.at.Posting + nba$Caption.length + nba$Day.num + nba$Type.num))

# TO DO: Quantify time of day when it is posted, create groups for morning, afternoon, night,
# late night. 

# Times of the day
time.list <- c("Late Night", "Early Morning", "Late Morning", "Early Afternoon", "Late Afternoon", "Early Evening",
               "Late Evening", "Night")

# Used arbitrary times when each period ends. So the first one is Night, which ends at 12 am
time.vec <- c("00:00:00", "04:00:00", "08:00:00", "12:00:00", "15:00:00", "17:00:00", "19:00:00", "22:00:00")
# length(time.vec)

# Assigning each post a qualitative time of day it was posted based on above categories. 
nba$TimeofDay <- NA
for(i in 1:length(nba[,1])){
  x <- format(strptime(nba$Created[i], format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"), "%H:%M:%S")
  if(x < time.vec[2]){
    (nba$TimeofDay[i] = time.list[1])
  } else if(x < time.vec[3]){
    (nba$TimeofDay[i] = time.list[2])
  } else if(x < time.vec[4]){
    (nba$TimeofDay[i] = time.list[3])
  } else if(x < time.vec[5]){
    (nba$TimeofDay[i] = time.list[4])
  } else if(x < time.vec[6]){
    (nba$TimeofDay[i] = time.list[5])
  } else if(x < time.vec[7]){
    (nba$TimeofDay[i] = time.list[6])
  }else if(x < time.vec[8]){
    (nba$TimeofDay[i] = time.list[7])
  }else{
    (nba$TimeofDay[i] = time.list[8])
  }
}

# Assigns number to each time category based on its position in the matrix
nba$TimeNum <- NA
for(i in 1:length(nba[,1])){
  x <- match(nba$TimeofDay[i], time.list)
  nba$TimeNum[i] <- x
}

# ============================= TO DO ===========================

# The last thing is to analyze number of #'s in the caption
# caption.Split <- strsplit(as.character(nba$Description[3778]), "?")[[1]]
# count <- 0
# for(i in 1:length(caption.Split)){
#   if(caption.Split[i] == "#")(count = count + 1)
# }
# count
# is.empty(caption.Split)
# caption.Split
# 
# nba$hashCount <- NA
# for(i in 1:length(nba[,1])){
#   cap <- strsplit(as.character(nba$Description[i]), "#")[[1]]
#   if(length(cap) == 0){
#     nba$hashCount[i] <- 0
#   } else {
#   nba$hashCount[i] <- length(cap) - 1
#   }
# }

# The regression showed the amount of hashtags in a post is insignificant interms of engagements

# Let us try the log number of followers
nba$log.followers <- NA
for(i in 1:length(nba[,1])){
  nba$log.followers[i] <- log(nba$Followers.at.Posting[i], 10)
  }
# summary(lm(nba$log.engage ~ nba$log.followers + nba$Caption.length + nba$Day.num + nba$Type.num + nba$TimeNum))

# nba.log <- lm(nba$Engagements ~ nba$log.followers + nba$Caption.length + nba$Day.num + nba$Type.num + nba$TimeNum))
nba.sub <- nba[c(-3,-4,-5,-7,-10)]

# Log base 10 model
nba.log.pred <- data.frame(apply(nba.sub, 2, log10))
for(i in 1:length(nba[,1])){
  if(nba.log.pred$Caption.length[i] == -Inf)(nba.log.pred$Caption.length[i] <- 0)
}
summary(lm(nba.log.pred))

nba.log.model <- lm(nba.log.pred)

# summary(lm(nba.log.pred$Engagements ~ nba.log.pred$Followers.at.Posting + nba.log.pred$Caption.length 
#            + nba$Day.num + nba$Type.num + nba$TimeNum))

# # Linear Model
# nba.regress <- lm(nba.sub$Engagements ~ nba.sub$Followers.at.Posting + nba.sub$Caption.length
#                    + nba.sub$Day.num + nba.sub$Type.num + nba.sub$TimeNum)
# summary(nba.regress)
# 
# Robust linear model
# nba.rlm <- rlm(nba.sub$Engagements ~ nba.sub$Followers.at.Posting + nba.sub$Caption.length
#                  + nba.sub$Day.num + nba.sub$Type.num + nba.sub$TimeNum)
# nba.rlm
# 
# summary(nba.rlm)



# ============================ PREDICTION ============================================

nba.pred <- read.csv("holdout_set.csv")


# This loop adds the Caption length from the Description
nba.pred$Caption.length <- NA
for(i in 1:length(nba.pred[,1])){
  nba.pred$Caption.length[i] <- nchar(as.character(nba.pred$Description[i]))
  
}

# This finds the day of the week given the time
nba.pred$Day <- NA
for(i in 1:length(nba.pred[,1])){
  nba.pred$Day[i] <- weekdays(strptime(nba.pred$Created[i], format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))
}


# Assigns numbers for each day of the week. 1 is Sunday, 7 is Saturday
nba.pred$Day.num <- NA
for(i in 1:length(nba.pred[,1])){
  nba.pred$Day.num[i] <- wday(nba.pred$Created[i])
}

# This quantifies the category of post
nba.pred$Type.num <- NA
for(i in 1:length(nba.pred[,1])){
  if(nba.pred$Type[i] == "Video")(nba.pred$Type.num[i] = 1)
  if(nba.pred$Type[i] == "Photo")(nba.pred$Type.num[i] = 2)
  if(nba.pred$Type[i] == "Album")(nba.pred$Type.num[i] = 3)
}

# Times of the day
time.list <- c("Late Night", "Early Morning", "Late Morning", "Early Afternoon", "Late Afternoon", "Early Evening",
               "Late Evening", "Night")

# Used arbitrary times when each period ends. So the first one is Night, which ends at 12 am
time.vec <- c("00:00:00", "04:00:00", "08:00:00", "12:00:00", "15:00:00", "17:00:00", "19:00:00", "22:00:00")
# length(time.vec)

# Assigning each post a qualitative time of day it was posted based on above categories. 
nba.pred$TimeofDay <- NA
for(i in 1:length(nba.pred[,1])){
  x <- format(strptime(nba.pred$Created[i], format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"), "%H:%M:%S")
  if(x < time.vec[2]){
    (nba.pred$TimeofDay[i] = time.list[1])
  } else if(x < time.vec[3]){
    (nba.pred$TimeofDay[i] = time.list[2])
  } else if(x < time.vec[4]){
    (nba.pred$TimeofDay[i] = time.list[3])
  } else if(x < time.vec[5]){
    (nba.pred$TimeofDay[i] = time.list[4])
  } else if(x < time.vec[6]){
    (nba.pred$TimeofDay[i] = time.list[5])
  } else if(x < time.vec[7]){
    (nba.pred$TimeofDay[i] = time.list[6])
  }else if(x < time.vec[8]){
    (nba.pred$TimeofDay[i] = time.list[7])
  }else{
    (nba.pred$TimeofDay[i] = time.list[8])
  }
}

nba.pred$TimeNum <- NA
for(i in 1:length(nba.pred[,1])){
  x <- match(nba.pred$TimeofDay[i], time.list)
  nba.pred$TimeNum[i] <- x
}

nba.pred$log.followers <- NA
for(i in 1:length(nba.pred[,1])){
  nba.pred$log.followers[i] <- log(nba.pred$Followers.at.Posting[i], 10)
}

# ===================================== Writing the data ============================

nba.pred.sub <- nba.pred[c(-3,-4,-5,-7,-10)]

# Predicting the log values
nba.log.sub <- data.frame(apply(nba.pred.sub, 2, log10))
log.predict <- predict(nba.log.model, nba.log.sub)

for(i in 1:1000){
  log.predict[i] <- 10^(log.predict[i])
}

nba.pred$Engagements <- log.predict

# for(i in 1:length(nba.log.sub[,1])){
#   for(j in 1:length(nba.log.sub[1,])){
#     nba.log.sub[i,j] <- 10^(nba.log.sub[i,j])
#   }
# }

# nba.pred.rlm <- nba.pred.sub[-7]
# for(i in 1:length(nba.pred.rlm[,1])){
#   nba.pred.rlm$Engagements[i] <- (9.797307e+05 - 8.772701e-05*nba.pred.rlm$Followers.at.Posting[i]
#                                   - 1.030815e+03*nba.pred.rlm$Caption.length[i]
#                                   + 2.890879e+03*nba.pred.rlm$Day.num[i]
#                                   - 2.156870e+05*nba.pred.rlm$Type.num[i]
#                                   - 5.235592e+03*nba.pred.rlm$TimeNum[i])
# }
# summary(nba.pred.rlm$Engagements)

summary(nba$Engagements)
sd(nba$Engagements)

summary(final$Engagements)
sd(final$Engagements)

# ============================== Graphs and writing the data ===================

hist(nba$Engagements, freq = F)
par(new )
hist(final$Engagements, xlim = c(200000,1200000))

actual <- nba$Engagements
pred <- final$Engagements

hactual <- hist(actual, plot = F)
hpred <- hist(pred, plot = F)
xlim <- range(hactual$breaks, hpred$breaks)
ylim <- range(0, hactual$density, hpred$density)

plot(hactual, xlim = xlim, ylim = ylim, col = rgb(1,0,0,0.4), xlab = 'Engagements',
     freq = FALSE, main = 'Comparing the distributions')
opar <- par(new = FALSE)
plot(hpred, xlim = xlim, ylim = ylim, xaxt = 'n', yaxt = 'n', col = rgb(0,0,1,0.4), 
     add = TRUE,freq = FALSE)
legend('topleft',c('Actual','Predicted'),
       fill = rgb(1:0,0,0:1,0.4), bty = 'n',
       border = NA)
par(opar)

# Same graph above and below, slightly different ways to get there

hist(actual, xlim = xlim, ylim = ylim, col = rgb(1,0,0,0.4), xlab = "Engagements",
     main = 'Distribution of Engagements', freq = F)
hist(pred, col = rgb(0,0,1,0.4), add = TRUE, freq = F)
legend('topleft',c('Actual','Predicted'),
       fill = rgb(1:0,0,0:1,0.4), bty = 'n',
       border = NA)

#pred.rlm <- predict(nba.rlm, nba.pred.sub)

# predicted <- predict(nba.regress, nba.pred.sub)
# nba.pred$Engagements <- predicted

# ============================ Last bit ======================

final <- nba.pred[c(-6:-12)]

write.csv(final, file = "holdout_set_Cortese.csv")


