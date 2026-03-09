# initialize

library(tidyverse)
library(lme4)

as23 <- read.csv("data/all2023Pitches.csv")

gameList <- as23 %>%
  mutate(game_pitch = 100 * at_bat_number + pitch_number) %>%
  split(~game_pk)
  
gl <- lapply(gameList, function(gg){
  go <- gg %>%
    arrange(game_pitch) %>%
    mutate(game_pitch = 1:nrow(.))
})

new23 <- do.call(rbind, gl) %>%
  mutate(balls = ifelse(balls %in% 4, 3, balls))


# simple 23

simple23 <- new23 %>%
  select(balls, strikes, pitch_number, at_bat_number, game_pitch, description, woba_value, type, game_pk) %>%
  mutate(description = ifelse(description %in% c("blocked_ball", "pitchout"), "ball", description)) %>%
  mutate(description = ifelse(description %in% "bunt_foul_tip", "foul_bunt", description)) %>%
  mutate(game_bat = paste(game_pk, at_bat_number, sep = "_")) %>%
  group_by(game_bat) %>%
  mutate(ult_woba = last(woba_value), fouls = 0)

for(i in 2:nrow(simple23)){
  if(simple23$game_bat[i] == simple23$game_bat[i - 1]){
    simple23$fouls[i] <- simple23$fouls[i - 1]
    if(simple23$description[i - 1] %in% c("foul", "foul_tip", "foul_bunt")){
      simple23$fouls[i] <- simple23$fouls[i] + 1
    }
  }
  cat("Completed", i, "of", nrow(simple23), "\n")
}




# simple count woba

bs <- simple23 %>% 
  filter(!is.na(woba_value)) %>%
  group_by(balls, strikes, fouls) %>%
  summarise(n = n(),
            mn = mean(woba_value), 
            sd = sd(woba_value))

  
  
  
  
  