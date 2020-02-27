#### START THE CLOCK ####

start_time <- Sys.time()

print(paste0('The program began at: ', Sys.time()))

#### Install Stuff ####

#devtools::install_github("abresler/nbastatR")

library(nbastatR)
library(future)
library(forecast)
library(tidyverse)
library(sqldf)
library(paulverse)

#install.packages('dummies')

library(lpSolve)
library(dummies)

library(caret)
library(FactoMineR)

#### User Inputs ####

setwd("/opt/R/R_project/pperrin/2020/Fantasy_Sports")

#Disable Scientific Notation
options(scipen = 999)

#### Load Up Fanduel Data ####

fanduel_df <- read.csv("Data/FanDuel-NBA-2020-02-24-43885-players-list.csv", stringsAsFactors = F)

fanduel_df$Tier <- NULL

fanduel_df$X <- NULL

fanduel_df$X.1 <- NULL

fanduel_df_2 <- subset(fanduel_df, fanduel_df$Injury.Indicator != "GTD" & fanduel_df$Injury.Indicator != "O")

fanduel_df_2$Injury.Indicator <- NULL

fanduel_df_2$Injury.Details <- NULL

fanduel_df_2$Id <- NULL

fanduel_df_2$First.Name <- NULL

fanduel_df_2$Last.Name<- NULL

#### FIX NAME MISMATCHES HERE ####

fanduel_df_2$Nickname <- ifelse(fanduel_df_2$Nickname == "KELLY OUBRE", "KELLY OUBRE JR.", fanduel_df_2$Nickname)

#### Load Fantasy Data ####

plan(multiprocess)

a <- game_logs(seasons = 2018:2020)

saveRDS(a, file = "Data/hst.rds")

#a <- readRDS(file = "/opt/R/R_project/pperrin/2020/Fantasy_Sports/Data/hst.rds")

a_2 <- subset(a, select = c("dateGame", "namePlayer", "treb", "ast", "stl", "blk", "tov", "pts", "slugOpponent", "slugTeam", "locationGame"))

a_2$namePlayer <- ifelse(grepl(" " , a_2$namePlayer), a_2$namePlayer, paste0(a_2$namePlayer, " "))

#a_2 <- subset(a_2, toupper(a_2$namePlayer) == "LEBRON JAMES")

a_2$fantasy_points <- a_2$pts + (1.2*a_2$treb) + (1.5*a_2$ast) + (2*a_2$stl) + (2*a_2$blk) - (1*a_2$tov)

dates <- subset(a_2, select = c("dateGame", "namePlayer")) %>% arrange(namePlayer,dateGame)

dates <- tibble::rownames_to_column(dates, "Date_ID")

a_3 <- left_join(a_2, dates)

a_3$Date_ID <- as.numeric(a_3$Date_ID)

player_count <- sqldf("

SELECT
 namePlayer,
 COUNT(*) AS count,
 SUM(fantasy_points) AS SUM_fantasy_points

FROM a_3
GROUP BY namePlayer
              
")

quants <- as.data.frame(quantile(player_count$count))

#Remove bottom 25 percent

#player_count_2 <- subset(player_count, player_count$count > mean(player_count$count))

#player_count_2 <- subset(player_count, player_count$count > quants[2,])

#Actually Don't!

player_count_2 <- player_count

a_4 <- sqldf("SELECT * FROM a_3 WHERE (namePlayer in (SELECT DISTINCT namePlayer FROM player_count_2))")

#### embed.hierarchy ####

embed.hierarchy <- function(df_hier, df_var_vector, num_features){
  
  embed_stat_time <- Sys.time()
  
  a_1 <- subset(df_hier, select = df_var_vector)
  
  a_1 <- unique(a_1[df_var_vector])
  
  # dummify the data
  
  dmy <- dummyVars(" ~ .", data = a_1)
  
  trsf <- data.frame(predict(dmy, newdata = a_1))
  
  trsf <- trsf %>% mutate_all(as.character)
  
  gc()
  
  mca1 = MCA(trsf, ncp = num_features, ind.sup = NULL, quanti.sup = NULL, 
             quali.sup = NULL, excl=NULL, graph = TRUE, 
             level.ventil = 0, axes = c(1,2), row.w = NULL, 
             method="Indicator", na.method="NA", tab.disj=NULL)
  
  a_2 <- cbind(a_1, mca1$ind$coord)
  
  names(a_2) <- gsub(x = names(a_2), pattern = " ", replacement = "_") 
  
  print(Sys.time()-embed_stat_time)
  
  return(a_2)
  
}

emberd_nba <- embed.hierarchy(df_hier = a_4, df_var_vector = c("slugOpponent"), num_features = 5)

names(emberd_nba) <- gsub(x = names(emberd_nba), pattern = "Dim", replacement = "opponent_feature") 

a_5 <- left_join(a_4, emberd_nba)

a_5$home_ind <- ifelse(a_5$locationGame == "H", 1, 0)

#### LOOP FORECAST ####

a_5$Nickname <- a_5$namePlayer

fanduel_df_2$Nickname <- toupper(fanduel_df_2$Nickname)

a_5$Nickname <- toupper(a_5$Nickname)

a_6 <- subset(a_5, a_5$dateGame < as.Date("2020-02-20"))

j = 0

#for(i in 1:10){
for(i in 1:length(unique(a_6$Nickname))){
  
  #print(i)
  
  a_6_temp <- subset(a_6, a_6$Nickname  == unique(a_6$Nickname)[i])
  
  print(unique(a_6_temp$Nickname))
  
  #### Create teammate features ####
  
  #z_2 <- sqldf("SELECT DISTINCT Nickname, dateGame, slugTeam FROM a_6 WHERE Nickname != 'LEBRON JAMES'")
  
  z_2 <- sqldf(paste0('SELECT DISTINCT Nickname, dateGame, slugTeam FROM a_6 WHERE Nickname != "', unique(a_6_temp$Nickname), '"'))
  
  z_3 <- sqldf("
             
   SELECT    
             
    l.*,
    r.Nickname AS teammates,
    1 AS dum
             
  FROM a_6_temp AS l
  LEFT JOIN z_2 AS r
  ON 
   l.dateGame = r.dateGame AND
   l.slugTeam = r.slugTeam
             
  ")
  
  # The arguments to spread():
  # - data: Data object
  # - key: Name of column containing the new column names
  # - value: Name of column containing values
  z_4 <- spread(z_3, teammates, dum)
  
  z_4[is.na(z_4)] <- 0
  
  opp <- subset(fanduel_df_2, fanduel_df_2$Nickname == unique(a_6_temp$Nickname))$Opponent
  
  #print(opp)
  
  #Fix opp data
  
  if(nrow(subset(fanduel_df_2, fanduel_df_2$Nickname == unique(a_6_temp$Nickname))) > 0){
    
    if(opp == "NY"){
      
      opp <- "NYK"
      
    } else if(opp == "PHO"){
      
      opp <- "PHX"
      
    } else if(opp == "NO"){
      
      opp <- "NOP"
      
    } else{
      
      opp <- opp
      
    }
    
  }
  
  opp_dat <- subset(emberd_nba, emberd_nba$slugOpponent == opp, select = c("opponent_feature_1", "opponent_feature_2", "opponent_feature_3", "opponent_feature_4", "opponent_feature_5"))
  
  if((nrow(opp_dat) > 0) & (nrow(a_6_temp) > 2)){
    
    j = j + 1
    
    time_series <- ts(a_6_temp$fantasy_points)
    
    #get home or away
    
    home_or_away <- subset(fanduel_df_2, fanduel_df_2$Nickname == unique(a_6_temp$Nickname))
    
    if(strsplit(home_or_away$Game, "@")[[1]][2] == home_or_away$Opponent){
      
      the_home_ind <- 0
      
    } else{
      
      the_home_ind <- 1
      
    }
    
    #### Create teammates feature ####
    
    #Distinct Combination of teammates
    
    temp <- distinct(subset(z_4, select = grep(" ", names(z_4), value=TRUE)))
    
    temp <- temp %>% mutate_all(as.character)
    
    mca1 = MCA(temp, ncp = 4, ind.sup = NULL, quanti.sup = NULL, 
               quali.sup = NULL, excl=NULL, graph = TRUE, 
               level.ventil = 0, axes = c(1,2), row.w = NULL, 
               method="Indicator", na.method="NA", tab.disj=NULL)
    
    z_5 <- cbind(temp, mca1$ind$coord)
    
    #names(z_5) <- gsub(x = names(z_5), pattern = " ", replacement = "_") 
    
    names(z_5) <- gsub(x = names(z_5), pattern = "Dim", replacement = "teammates_feature")
    
    z_5 <- z_5 %>% mutate_all(as.numeric)
    
    z_6 <- left_join(z_4, z_5)
    
    #### Train Neural Net ####
    
    x_train <- left_join(a_6_temp, z_6) %>% select(c(grep("opponent_feature", names(z_6), value=TRUE), "home_ind", grep("teammates_feature", names(z_6), value=TRUE)))
    
    nn_fit <- nnetar(time_series, repeats = 50, xreg = x_train, lambda = NULL, model = NULL, subset = NULL, scale.inputs = TRUE, MaxNWts = 1000)
    
    x_regs <- subset(emberd_nba, emberd_nba$slugOpponent == opp, select = c("opponent_feature_1", "opponent_feature_2", "opponent_feature_3", "opponent_feature_4", "opponent_feature_5"))
    
    x_regs$home_ind <- the_home_ind
    
    #### FORMAT TEST DATA ####
    
    fanduel_temp <- subset(fanduel_df_2, toupper(fanduel_df_2$Nickname) == toupper(unique(a_6_temp$Nickname)))
    
    x_test <- sqldf(paste0('SELECT DISTINCT Nickname, Team FROM fanduel_df_2 WHERE Nickname != "', unique(a_6_temp$Nickname), '"'))
    
    x_test_2 <- sqldf("
             
    SELECT    
             
     l.*,
     r.Nickname AS teammates,
     1 AS dum
             
    FROM fanduel_temp AS l
    LEFT JOIN x_test AS r
    ON 
     l.Team = r.Team
             
    ")
    
    x_test_2$teammates <- toupper(x_test_2$teammates)
    
    x_test_3 <- sqldf("
                      
    SELECT
     
     l.teammates AS teammates,
     r.dum AS dum

    FROM (SELECT DISTINCT teammates FROM z_3) AS l
    LEFT JOIN x_test_2 AS r
    ON 
     l.teammates = r.teammates
                      
    ")
    
    x_test_3$dum <- ifelse(is.na(x_test_3$dum), 0, 1)
    
    # The arguments to spread():
    # - data: Data object
    # - key: Name of column containing the new column names
    # - value: Name of column containing values
    
    x_test_4 <- spread(x_test_3, teammates, dum)
    
    x_test_4[is.na(x_test_4)] <- 0
    
    x_test_5 <- bind_cols(x_regs, x_test_4)
    
    #### MOST SIMLIAR LINEUP ####
    
    yyyy <- x_test_5
    
    names(yyyy) <- gsub(x = names(yyyy), pattern = " ", replacement = ":") 
    
    zzzz <- sqldf("
                  
                  SELECT 
                  l.*,
                  r.*
                  FROM z_4 AS l
                  LEFT JOIN yyyy AS r
                  
                  ")

    # The arguments to gather():
    # - data: Data object
    # - key: Name of new key column (made from names of data columns)
    # - value: Name of new value column
    # - ...: Names of source columns that contain values
    # - factor_key: Treat the new key column as a factor (instead of character vector)

    zzzz_1 <- gather(zzzz, hist_team, ind,  grep(" ", names(zzzz), value=TRUE), factor_key=TRUE)

    zzzz_2 <- gather(zzzz, current_team, ind_2,  grep(":", names(zzzz), value=TRUE), factor_key=TRUE)

    zzzz_1 <- subset(zzzz_1, select = c("dateGame","hist_team", "ind"))

    zzzz_2 <- subset(zzzz_2, select = c("current_team", "ind_2"))

    zzzz_3 <- bind_cols(zzzz_1, zzzz_2)

    zzzz_3$match <- ifelse(zzzz_3$ind == zzzz_3$ind_2, 1, 0)

    zzzz_4 <- sqldf("
                
     SELECT
      dateGame,
      SUM(match) AS match
                
     FROM zzzz_3
     GROUP BY dateGame
     ORDER BY match DESC, dateGame DESC
                
    ")

    zzzz_5 <- head(zzzz_4, n = 1)

    zzzz_6 <- inner_join(zzzz_5, z_6)
    
    x_test_5 <- x_test_5 %>% select(c(grep("opponent_feature", names(x_test_5), value=TRUE), "home_ind"))
    
    zzzz_6 <- zzzz_6 %>% select(c(grep("teammates_feature", names(zzzz_6), value=TRUE)))
    
    x_test_6 <- cbind(x_test_5, zzzz_6)
    
    if(any(is.na(x_test_6))){
      
      print("Teammate Combination Not Found")
      
    } else{
      
      print("Teammate Combination Found!")
      
    }
    
    x_test_6[is.na(x_test_6)] <- 0
    
    #### Run model on test data ####
    
    nn_forecast <- forecast(nn_fit, h = freq, xreg = x_test_6)
    
    if(j == 1){
      
      nn_out <- as.data.frame(nn_forecast)
      
      nn_out$Nickname <- unique(a_6_temp$Nickname)
      
    }else{
      
      nn_forecast <- as.data.frame(nn_forecast)
      
      nn_forecast$Nickname <- unique(a_6_temp$Nickname)
      
      nn_out <- bind_rows(nn_out, nn_forecast)
      
    }
    
  } else{
    
    print("Skipping NN")
    
  }
  
}

#saveRDS(fc_2, file = "/opt/R/R_project/pperrin/2020/MISC/fc_2.rds")

#write_csv(fc_2, path = "/opt/R/R_project/pperrin/2020/MISC/fc_2.csv")

#fc_2 <- readRDS(fc_2, file = "/opt/R/R_project/pperrin/2020/MISC/fc_2.rds")

#### Join Forecast with Fanduel ####

fanduel_df_3 <- left_join(fanduel_df_2, nn_out)

fanduel_df_3$fantasy_points <- fanduel_df_3$`Point Forecast`

fanduel_df_3$`Point Forecast` <- NULL

fanduel_df_3$fantasy_points <- ifelse(is.na(fanduel_df_3$fantasy_points), fanduel_df_3$FPPG, fanduel_df_3$fantasy_points)

fanduel_df_3$fantasy_points <- ifelse(fanduel_df_3$fantasy_points <= 0, 0, fanduel_df_3$fantasy_points)

fanduel_df_3$gmean <- (fanduel_df_3$FPPG * fanduel_df_3$Played * fanduel_df_3$fantasy_points)**(1/3)

fanduel_df_3$gmean_2 <- (fanduel_df_3$Played * fanduel_df_3$fantasy_points)**(1/2)

summary(fanduel_df_3)

#### Optimize ####

opt_lineup <- function(the_data, the_var, salary){
  
  #Dummy variables
  
  Position.Mat <- as.data.frame(dummy(the_data[, "Position"]))
  
  names(Position.Mat) = c("C", "PF", "PG", "SF", "SG")
  
  nba <- cbind(the_data, Position.Mat)
  
  #Set up objective function
  
  f.obj <- nba[, the_var]
  
  #Set up constraints
  
  f.con <- t(cbind(Salary = nba[, "Salary"], Position.Mat))
  
  colnames(f.con) <- nba$Nickname
  
  # Instantiate the vectors
  f.dir <- rep(0, nrow(f.con))
  f.rhs <- rep(0, nrow(f.con))
  
  #Salary
  
  f.dir[1] <- "="
  f.rhs[1] <- salary
  
  #One Center
  
  f.dir[2] <- "="
  f.rhs[2] <- 1
  
  #2 PF
  
  f.dir[3] <- "="
  f.rhs[3] <- 2
  
  #2 PG
  
  f.dir[4] <- "="
  f.rhs[4] <- 2
  
  #2 SF
  
  f.dir[5] <- "="
  f.rhs[5] <- 2
  
  #2 SG
  
  f.dir[6] <- "="
  f.rhs[6] <- 2
  
  #Do it
  
  opt <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = TRUE)
  picks <- nba[which(opt$solution == 1), ]
  #kable(picks, format = "markdown", row.names = F)
  
  picks$C <- NULL
  
  picks$PF <- NULL
  
  picks$PG <- NULL
  
  picks$SF <- NULL
  
  picks$SG <- NULL
  
  return(picks)
  
}

z_FPPG_Played_fantasy_points <- opt_lineup(the_data = fanduel_df_3, the_var = "gmean", salary = 60000)

z_Played_fantasy_points <- opt_lineup(the_data = fanduel_df_3, the_var = "gmean_2", salary = 60000)

z_fantasy_points <- opt_lineup(the_data = fanduel_df_3, the_var = "fantasy_points", salary = 60000)

#### Optional TEST section ####

test <- subset(a_5, a_5$dateGame == as.Date("2020-02-24"))

test_2 <- left_join(nn_out, test)

test_2 <- na.omit(test_2)

test_2$resid <- test_2$`Point Forecast` - test_2$fantasy_points

test_2$ape <- (test_2$`Point Forecast` - test_2$fantasy_points)/test_2$fantasy_points

test_2 <- subset(test_2, select = c("Point Forecast", "Nickname", "fantasy_points", "resid", "ape"))

print(paste0("rmse is: ", sd(test_2$resid)))

#### save data ####

time_stamp <- str_replace_all(substr(Sys.time(), 1, 10), "-","_")

saveRDS(z_FPPG_Played_fantasy_points, file = paste0("Output/FPPG_Played_fantasy_points", time_stamp, ".rds"))

saveRDS(z_Played_fantasy_points, file = paste0("Output/Played_fantasy_points", time_stamp, ".rds"))

saveRDS(z_fantasy_points, file = paste0("Output/fantasy_points", time_stamp, ".rds"))

#### Stop the Clock ####

end_time <- Sys.time()

args <- commandArgs(trailingOnly = F)

scriptPath <- normalizePath(dirname(sub("^--file=", "", args[grep("^--file=", args)])))

print(paste0(args, ' ended at: ',Sys.time()))

print(end_time - start_time)

#### This is the End #### 

##### Scrap Code #####

write_csv(a_6, "MISC/a_6.csv")

#### B 2020/02/21 ####

dates <- as.tibble(sqldf("SELECT dateGame, namePlayer FROM a_2 ORDER BY namePlayer, dateGame"))

dates_2 <- subset(a_2, select = c("dateGame", "namePlayer")) %>% arrange(namePlayer,dateGame)

dates_2 <- tibble::rownames_to_column(dates_2, "Date_ID")

z_1 <- subset(a_6, a_6$Nickname == "LEBRON JAMES")

z_2 <- sqldf("SELECT DISTINCT Nickname, dateGame, slugTeam FROM a_6 WHERE Nickname != 'LEBRON JAMES'")

z_3 <- sqldf("
             
SELECT    
             
 l.*,
 r.Nickname AS teammates,
 1 AS dum
             
FROM z_1 AS l
LEFT JOIN z_2 AS r
ON 
 l.dateGame = r.dateGame AND
 l.slugTeam = r.slugTeam
             
")

# The arguments to spread():
# - data: Data object
# - key: Name of column containing the new column names
# - value: Name of column containing values
z_4 <- spread(z_3, teammates, dum)

z_4[is.na(z_4)] <- 0



temp <- embed.hierarchy(df_hier = z_3, df_var_vector = c("teammates"), num_features = 2)

#### E 2020/02/21 ####

rm(list=setdiff(ls(), "a"))

temp <- as.data.frame(dummy(a_4[, "slugOpponent"], verbose = T))

temp <- as.data.frame(dummy(a_4$slugOpponent, verbose = T, sep = "::"))

colnames(temp) <- sub("slugOpponent, verbose = T, sep = \"::\")::", "", colnames(temp))

temp_2 <- cbind(a_4, temp)
