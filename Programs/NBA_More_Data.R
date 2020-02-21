#### START THE CLOCK ####

start_time <- Sys.time()

print(paste0('The program began at: ', Sys.time()))

#### Install Stuff ####

#devtools::install_github("abresler/nbastatR")

library(nbastatR)
library(future)
library(fable)
library(tsibble)
library(tidyverse)
library(sqldf)
library(paulverse)

#install.packages('dummies')

library(lpSolve)
library(dummies)

#### Load Fantasy Data ####

plan(multiprocess)

a <- game_logs(seasons = 2018:2020)

a_2 <- subset(a, select = c("dateGame", "namePlayer", "treb", "ast", "stl", "blk", "tov", "pts", "slugOpponent"))

#a_2 <- subset(a_2, toupper(a_2$namePlayer) == "LEBRON JAMES")

a_2$fantasy_points <- a_2$pts + (1.2*a_2$treb) + (1.5*a_2$ast) + (2*a_2$stl) + (2*a_2$blk) - (1*a_2$tov)

dates <- as.tibble(sqldf("SELECT dateGame, namePlayer FROM a_2 ORDER BY namePlayer, dateGame"))

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

player_count_2 <- subset(player_count, player_count$count > quants[2,])

a_4 <- sqldf("SELECT * FROM a_3 WHERE (namePlayer in (SELECT DISTINCT namePlayer FROM player_count_2))")

a_tsbl <- as_tsibble(a_4, key = namePlayer, index = Date_ID)

#a_tsbl <- rs_by_var_2(df = a_tsbl, byvars = c("namePlayer"), num = 10)

#### FORECAST ####

# a_tsbl_2 <- a_tsbl %>%
#   group_by_key() %>%
#   fill_gaps() %>% 
#   tidyr::fill(fantasy_points, .direction = "down")

fit <- a_tsbl %>%
  model(
    ets = ETS(fantasy_points)
    ,arima = ARIMA(fantasy_points)
    ,tslm = TSLM(fantasy_points)
    ,nnetar = NNETAR(fantasy_points)
  )

fc <- fit %>%
  forecast(h = 1)

fc$.distribution <- NULL

fc_2 <- spread(fc, .model, fantasy_points)

fc_2 <- transform(fc_2, fantasy_points = rowMeans(subset(fc_2, select=c(arima, ets, tslm)), na.rm = TRUE))

saveRDS(fc_2, file = "/opt/R/R_project/pperrin/2020/MISC/fc_2.rds")

#write_csv(fc_2, path = "/opt/R/R_project/pperrin/2020/MISC/fc_2.csv")

#fc_2 <- readRDS(fc_2, file = "/opt/R/R_project/pperrin/2020/MISC/fc_2.rds")

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

fanduel_df_2$Opponent<- NULL

#### Join Forecast with Fanduel ####

names(fc_2)[names(fc_2) == 'namePlayer'] <- 'Nickname'

fanduel_df_2$Nickname <- toupper(fanduel_df_2$Nickname)

fc_2$Nickname <- toupper(fc_2$Nickname)

fanduel_df_3 <- left_join(fanduel_df_2, fc_2)

fanduel_df_3$fantasy_points <- ifelse(is.na(fanduel_df_3$fantasy_points), fanduel_df_3$FPPG, fanduel_df_3$fantasy_points)

fanduel_df_3$gmean <- (fanduel_df_3$FPPG * fanduel_df_3$Played * fanduel_df_3$fantasy_points)**(1/3)

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

#### Stop the Clock ####

end_time <- Sys.time()

args <- commandArgs(trailingOnly = F)

scriptPath <- normalizePath(dirname(sub("^--file=", "", args[grep("^--file=", args)])))

print(paste0(args, ' ended at: ',Sys.time()))

print(end_time - start_time)

#### This is the End #### 
