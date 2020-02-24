temp <- full_join(distinct(a_6, slugOpponent), distinct(fanduel_df_2, Opponent), by = c("slugOpponent" = "Opponent"))

temp <- full_join(
  mutate(distinct(a_6, slugOpponent), Opponent.a = slugOpponent), 
  mutate(distinct(fanduel_df_2, Opponent), Opponent.b = Opponent), 
  by=c("slugOpponent" = "Opponent")
)


RUSSELL_WESTBROOK <- subset(a_6, a_6$Nickname == "RUSSELL WESTBROOK")

RUSSELL_WESTBROOK_2 <- sqldf("
                             
SELECT
 home_ind,
 MIN(fantasy_points) AS  MIN_fantasy_points,
 MAX(fantasy_points) AS  MAX_fantasy_points,
 AVG(fantasy_points) AS  AVG_fantasy_points
 
FROM RUSSELL_WESTBROOK
GROUP BY home_ind
                             
")
