#### Import and Clean ####

#install.packages('dummies')

library(lpSolve)
library(dummies)
library(sqldf)

a <- read.csv("/opt/R/R_project/pperrin/2020/Fantasy_Sports/Data/FanDuel-NBA-2020-02-10-43466-players-list.csv",
              stringsAsFactors = F)

a$p_s <- (a$FPPG/a$Salary) * 1000

a$Tier <- NULL

a$X <- NULL

a$X.1 <- NULL

a_1 <- subset(a, a$Injury.Indicator != "GTD" & a$Injury.Indicator != "O")

a_1$p_s <- (a_1$FPPG/a_1$Salary) * 1000

a_1$Injury.Indicator <- NULL

a_1$Injury.Details <- NULL

a_1$Id <- NULL

a_1$First.Name <- NULL

a_1$Last.Name<- NULL

a_1$Team<- NULL

a_1$Opponent<- NULL

a_1$gmean <- (a_1$FPPG*a_1$Played)**.5

a_1$gmean_2 <- (a_1$FPPG*a_1$Played*a_1$FPPG)**(1/3)

a_1$gmean_3 <- (a_1$FPPG*a_1$Played*a_1$p_s)**(1/3)

#### Do Stuff ####

#Dummy variables

Position.Mat <- as.data.frame(dummy(a_1[, "Position"]))

names(Position.Mat) = c("C", "PF", "PG", "SF", "SG")

nba <- cbind(a_1, Position.Mat)

#Set up objective function

f.obj <- nba[, "FPPG"]

#Set up constraints

f.con <- t(cbind(Salary = nba[, "Salary"], Position.Mat))

colnames(f.con) <- nba$Nickname

# Instantiate the vectors
f.dir <- rep(0, nrow(f.con))
f.rhs <- rep(0, nrow(f.con))

#Salary

f.dir[1] <- "="
f.rhs[1] <- 60000

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
kable(picks, format = "markdown", row.names = F)

#### Make it a Function ####

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

b1 <- opt_lineup(the_data = a_1, the_var = "FPPG", salary = 60000)

b2 <- opt_lineup(the_data = a_1, the_var = "gmean", salary = 60000)

b3 <- opt_lineup(the_data = a_1, the_var = "gmean_2", salary = 60000)

b4 <- opt_lineup(the_data = a_1, the_var = "gmean_3", salary = 60000)

summary(b1)

summary(b2)

summary(b3)

summary(b4)

c <- rbind(b1, b2, b3, b4)

d <- sqldf("
           
SELECT
 Nickname,
 COUNT(*) AS COUNT
FROM c
GROUP BY Nickname
ORDER BY COUNT DESC
           
           
")

#### This is the End ####
