a_5 <- subset(a_5, toupper(a_5$namePlayer) == "LEBRON JAMES")

time_series <- ts(a_5$fantasy_points)

library(lubridate)
library(sqldf)
library(forecast)
library(dplyr)

nn_fit <- nnetar(time_series, repeats = 50, xreg = subset(a_5, select = c("Dim_1", "Dim_2", "Dim_3", "Dim_4", "Dim_5")), lambda = NULL, model = NULL, subset = NULL, scale.inputs = TRUE)

nn_forecast <- forecast(nn_fit, h = freq, xreg = subset(emberd_nba, emberd_nba$slugOpponent == "PHI", select = c("Dim_1", "Dim_2", "Dim_3", "Dim_4", "Dim_5")))

nn_forecast

#### Load Up Fanduel Data ####

fanduel_df <- read.csv("/opt/R/R_project/pperrin/2020/Fantasy_Sports/Data/FanDuel-NBA-2020-02-10-43466-players-list.csv", stringsAsFactors = F)

fanduel_df$Tier <- NULL

fanduel_df$X <- NULL

fanduel_df$X.1 <- NULL

fanduel_df_2 <- subset(fanduel_df, fanduel_df$Injury.Indicator != "GTD" & fanduel_df$Injury.Indicator != "O")

fanduel_df_2$Injury.Indicator <- NULL

fanduel_df_2$Injury.Details <- NULL

fanduel_df_2$Id <- NULL

fanduel_df_2$First.Name <- NULL

fanduel_df_2$Last.Name<- NULL

fanduel_df_2$Team<- NULL

#### LOOP FORECAST ####

a_5$Nickname <- a_5$namePlayer

fanduel_df_2$Nickname <- toupper(fanduel_df_2$Nickname)

a_5$Nickname <- toupper(a_5$Nickname)

j = 0

#for(i in 1:10){
for(i in 1:length(unique(a_5$Nickname))){
  
  #print(i)
  
  a_5_temp <- subset(a_5, a_5$Nickname  == unique(a_5$Nickname)[i])
  
  print(unique(a_5_temp$Nickname))
  
  opp <- subset(fanduel_df_2, fanduel_df_2$Nickname == unique(a_5_temp$Nickname))$Opponent
  
  #print(opp)
  
  opp_dat <- subset(emberd_nba, emberd_nba$slugOpponent == opp, select = c("Dim_1", "Dim_2", "Dim_3", "Dim_4", "Dim_5"))
  
  if(nrow(opp_dat) > 0){
    
    j = j + 1
    
    time_series <- ts(a_5_temp$fantasy_points)
    
    nn_fit <- nnetar(time_series, repeats = 50, xreg = subset(a_5_temp, select = c("Dim_1", "Dim_2", "Dim_3", "Dim_4", "Dim_5")), lambda = NULL, model = NULL, subset = NULL, scale.inputs = TRUE)
    
    nn_forecast <- forecast(nn_fit, h = freq, xreg = subset(emberd_nba, emberd_nba$slugOpponent == opp, select = c("Dim_1", "Dim_2", "Dim_3", "Dim_4", "Dim_5")))
    
    if(j == 1){
      
      nn_out <- as.data.frame(nn_forecast)
      
      nn_out$Nickname <- unique(a_5_temp$Nickname)
      
    }else{
      
      nn_forecast <- as.data.frame(nn_forecast)
      
      nn_forecast$Nickname <- unique(a_5_temp$Nickname)
      
      nn_out <- bind_rows(nn_out, nn_forecast)
      
      }
    
  } else{
    
    print("Skipping NN")
    
  }
  

  
}

#### This is the End ####

fanduel_df_2 %>% distinct(Opponent, .keep_all = TRUE)

temp <- full_join(fanduel_df_2 %>% distinct(Opponent, .keep_all = TRUE), emberd_nba, by = c("Opponent" = "slugOpponent"))

#### Join Historic with Fanduel ####

names(a_5)[names(a_5) == 'namePlayer'] <- 'Nickname'

fanduel_df_2$Nickname <- toupper(fanduel_df_2$Nickname)

a_5$Nickname <- toupper(a_5$Nickname)

fanduel_df_3 <- left_join(fanduel_df_2, a_5)

z_1 <- sqldf("SELECT DISTINCT Nickname, Opponent FROM fanduel_df_2")
