#NBA Analytics

setwd("C:/Users/Vincent/Documents/0 - Stevens/random/NBA-hackathon-master/NBA-hackathon-master")

eventCodes <- read.delim("Event_Codes.txt")
gameLineup <- read.delim("Game_Lineup.txt")
new.play <- read.delim("play-sorted.txt")

# Offensive Rating is defined as the team points scored per 100 possessions 
# while the player is on the court. 

# Defensive Rating is defined as the number of points per 100 possessions that
# the team allows while that individual player is on the court. 

# A possession is ended by (1) made field goal attempts, (2) made final free
# throw attempt, (3) missed final free throw attempt that results in a 
# defensive rebound, (4) missed field goal attempt that results in a defensive 
# rebound, (5) turnover, or (6) end of time period.


final <- data.frame(x = 1:10000, "Game_ID" = NA, "Player_ID" = NA, 
                      "OffRtg" = NA, "DefRtg" = NA)

# we will just test with one quarter
test.play <- new.play[1:117,]

# Update rosters with game lineup
for(i in 1:length(test.play[,1])){
  for(j in 1:length(gameLineup[,1])){
    if(gameLineup$Period[j] == 0 && gameLineup$Period[j + 1] == 1){
      # Must create a list of the roster for each team
      team1 <- data.frame(x <- 1:15, "Team_id" = NA, "Person_id" = NA, "Status" = NA)
      team2 <- data.frame(x <- 1:15, "Team_id" = NA, "Person_id" = NA, "Status" = NA)
      
    }
    if(test.play$Game_id[i] == gameLineup$Game_id[j]){
      
      
      
      break
    }
  }
}


