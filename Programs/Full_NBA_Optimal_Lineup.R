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

#### Load Up Fanduel Data ####

fanduel_df <- read.csv("/opt/R/R_project/pperrin/2020/Fantasy_Sports/Data/FanDuel-NBA-2020-02-20-43597-players-list.csv", stringsAsFactors = F)

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


#### Load Fantasy Data ####

plan(multiprocess)

a <- game_logs(seasons = 2018:2020)

saveRDS(a, file = "/opt/R/R_project/pperrin/2020/Fantasy_Sports/Data/hst.rds")

#a <- readRDS(file = "/opt/R/R_project/pperrin/2020/Fantasy_Sports/Data/hst.rds")

a_2 <- subset(a, select = c("dateGame", "namePlayer", "treb", "ast", "stl", "blk", "tov", "pts", "slugOpponent", "slugTeam"))

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

a_5 <- left_join(a_4, emberd_nba)

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
  
  opp <- subset(fanduel_df_2, fanduel_df_2$Nickname == unique(a_6_temp$Nickname))$Opponent
  
  #print(opp)
  
  opp_dat <- subset(emberd_nba, emberd_nba$slugOpponent == opp, select = c("Dim_1", "Dim_2", "Dim_3", "Dim_4", "Dim_5"))
  
  if((nrow(opp_dat) > 0) & (nrow(a_6_temp) > 2)){
    
    j = j + 1
    
    time_series <- ts(a_6_temp$fantasy_points)
    
    nn_fit <- nnetar(time_series, repeats = 50, xreg = subset(a_6_temp, select = c("Dim_1", "Dim_2", "Dim_3", "Dim_4", "Dim_5")), lambda = NULL, model = NULL, subset = NULL, scale.inputs = TRUE)
    
    nn_forecast <- forecast(nn_fit, h = freq, xreg = subset(emberd_nba, emberd_nba$slugOpponent == opp, select = c("Dim_1", "Dim_2", "Dim_3", "Dim_4", "Dim_5")))
    
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

b1 <- opt_lineup(the_data = fanduel_df_3, the_var = "gmean", salary = 60000)

b2 <- opt_lineup(the_data = fanduel_df_3, the_var = "gmean_2", salary = 60000)

#### Optional TEST section ####

test <- subset(a_5, a_5$dateGame >= as.Date("2020-02-20"))

test_2 <- left_join(nn_out, test)

test_2 <- na.omit(test_2)

test_2$resid <- test_2$`Point Forecast` - test_2$fantasy_points

test_2$ape <- (test_2$`Point Forecast` - test_2$fantasy_points)/test_2$fantasy_points

print(paste0("rmse is: ", sd(test_2$resid)))

#### Stop the Clock ####

end_time <- Sys.time()

args <- commandArgs(trailingOnly = F)

scriptPath <- normalizePath(dirname(sub("^--file=", "", args[grep("^--file=", args)])))

print(paste0(args, ' ended at: ',Sys.time()))

print(end_time - start_time)

#### This is the End #### 

##### Scrap Code #####

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