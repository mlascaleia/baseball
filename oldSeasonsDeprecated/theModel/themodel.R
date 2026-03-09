# initialize 

library(cmdstanr)
library(brms)
library(tidyverse)
library(nnet)
library(NeuralNetTools)
library(neuralnet)

# organize data

hip <- read.csv("theModel/theData/hip.csv")

hip <- hip %>%
  filter(!is.na(release_speed)) %>%
  filter(!is.na(hc_x)) %>%
  filter(!is.na(hc_y)) %>%
  filter(!is.na(hit_distance_sc)) %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(launch_speed))

# combine events

hip$simple_events <- hip$events
hip$simple_events[hip$simple_events %in% c("double_play", "field_out", "fielders_choice",
                                           "fielders_choice_out", "force_out", "grounded_into_double_play",
                                           "sac_fly", "sac_fly_double_play")] <- "out"

hip$simple_events[hip$simple_events %in% "out" &
                    hip$bb_type %in% "ground_ball"] <- "ground_out"

hip$simple_events[hip$simple_events %in% "out" &
                    hip$bb_type %in% c("line_drive", "fly_ball")] <- "fly_out"

hip$simple_events[hip$simple_events %in% "out" &
                    hip$bb_type %in% "popup"] <- "pop_out"


hip <- hip[!hip$events %in% c("sac_bunt", "field_error"),]
hip$simple_events[hip$simple_events %in% c("double", "triple", "home_run")] <- "big_hit"

types <- unique(hip$simple_events)

ggplot(hip) +
  geom_point(aes(x = hc_x, y = -hc_y, color = simple_events))

hip$mirror_x <- abs(hip$hc_x - 125)
hip$mirror_y <- -hip$hc_y + 200

ggplot(hip) +
  geom_point(aes(x = mirror_x, y = mirror_y, color = simple_events))

hip$spray <- atan2(hip$mirror_y, hip$mirror_x) * 57.29
hip <- hip %>%
  mutate(stand_int = ifelse(stand %in% "R", 1, -1))

eval_nn <- function(mm, lint = 1:10000, tdat = hip, rawped = F){
  predo <- predict(mm, tdat[lint, ])
  peds <- cbind.data.frame(est = predo, act = tdat[lint, "simple_events"], desc = tdat[lint, "des"])
  meas <- lapply(types, function(t){
    true_pos <- nrow(peds[peds$est %in% t & peds$act %in% t,])/nrow(peds[peds$est %in% t,])
    true_neg <- nrow(peds[!peds$est %in% t & !peds$act %in% t,])/nrow(peds[!peds$est %in% t,])
    false_pos <- nrow(peds[peds$est %in% t & !peds$act %in% t,])/nrow(peds[peds$est %in% t,])
    false_neg <- nrow(peds[!peds$est %in% t & peds$act %in% t,])/nrow(peds[!peds$est %in% t,])
    return(data.frame(result = t, pred_events = nrow(peds[peds$est %in% t,]), act_events = nrow(peds[peds$act %in% t,]),
                      true_pos, false_pos, true_neg, false_neg))
  })
  
  moo <- meas %>%
    do.call(what = rbind) %>%
    mutate(overpred = (pred_events - act_events)/act_events) %>%
    mutate(across(true_pos:overpred, \(x) round(x, 3))) %>%
    arrange(-overpred)
  
  if(!rawped)
    return(moo)
  else
    return(peds)
}


modo <-
  multinom(
    simple_events ~ launch_speed * launch_angle * spray *
      release_speed * release_pos_x * release_pos_z * 
      pfx_x * pfx_z *
      plate_x * plate_z *
      ax * ay * az + pfx_x:stand + ax:stand + release_pos_x:stand + plate_x:stand,
    random = (1 | home_team),
    data = hip[1:10000,],
    maxit = 500
  )

test <- 1:nrow(hip)

eval_nn(modo, lint = test)
peds <- eval_nn(modo, lint = test, rawped = T)

res <- peds %>%
  mutate(shouldBe = ifelse(est %in% c("ground_out", "fly_out", "pop_out"), "out", "safe"),
         isActually = ifelse(act %in% c("ground_out", "fly_out", "pop_out"), "out", "safe")) %>%
  cbind.data.frame(hip) %>%
  select(player_name, shouldBe, isActually) %>%
  mutate(real = as.integer(isActually == "safe"),
         exp = as.integer(shouldBe == "safe")) %>%
  group_by(player_name) %>%
  summarise(HIP = n(),
            BAHIP = sum(real)/n(),
            xBAHIP = sum(exp)/n()) %>%
  mutate(luck = BAHIP - xBAHIP) %>%
  filter(HIP > 20) %>%
  mutate(across(BAHIP:luck, \(x) round(x, 3))) %>%
  arrange(-luck)

a <- read.csv("theModel/theData/all2024.csv")
so <- a[a$events %in% "strikeout", ] %>%
  group_by(player_name) %>%
  summarize(K = n()) %>%
  right_join(res) %>%
  mutate(AVG = 1 - (HIP * (1 - BAHIP) + K)/(HIP + K),
         xAVG = 1 - (HIP * (1 - xBAHIP) + K)/(HIP + K)) %>%
  mutate(AVG_luck = AVG - xAVG) %>%
  mutate(across(AVG:AVG_luck, \(x) round(x, 3))) %>%
  mutate(PA = K + HIP) %>%
  filter(PA > 120) %>%
  arrange(-AVG_luck)




