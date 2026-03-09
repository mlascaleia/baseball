# initialize

rm(list = ls())
library(tidyverse)
library(stringi)

# functions

noPercent <- function(colo){
  return(as.numeric(gsub("%", "", colo))/100)
}

noNA <- function(colo){
  return(ifelse(is.na(colo), 0, colo))
}

# load data
starters <- read.csv("findmeOutfielderApr1/sheets/startingPs.csv", encoding = "UTF-8")
batstats <- read.csv("findmeOutfielderApr1/sheets/allHit.csv", encoding = "UTF-8")
pstats <- read.csv("findmeOutfielderApr1/sheets/pitchData.csv", encoding = "UTF-8")
pitchAmount <- read.csv("findmeOutfielderApr1/sheets/pitchAmount.csv", encoding = "UTF-8")

rhb <- read.csv("findmeOutfielderApr1/sheets/rhb.csv", encoding = "UTF-8")
lhb <- read.csv("findmeOutfielderApr1/sheets/lhb.csv", encoding = "UTF-8")
bhb <- read.csv("findmeOutfielderApr1/sheets/bhb.csv", encoding = "UTF-8")

hb <- rbind(rhb, lhb, bhb) %>%
  rename(name = X.U.FEFF.name) %>%
  mutate(name = gsub("[[:upper:]]\\. ", "", name)) %>%
  mutate(name = stri_trans_general(name, "Latin-ASCII"))

# clean batstats

batstats <- batstats %>%
  mutate(Name = stri_trans_general(Name, "Latin-ASCII")) %>%
  mutate(Name = gsub("[[:upper:]]\\. ", "", Name)) %>%
  select(-X.U.FEFF..) %>%
  mutate(across(starts_with("w"), noNA)) %>%
  rename(name = Name, team = Team) %>%
  filter(!is.na(PA))

# clean starters

startersCols <- unlist(starters[starters$g0 %in% "1-Apr", ])

startersNames <- starters[starters$X.U.FEFF.team %in% "", ] %>%
  select(-X.U.FEFF.team)
colnames(startersNames) <- startersCols[-1]
startersTeams <- starters[!starters$X.U.FEFF.team %in% "", ]
colnames(startersTeams) <- paste0("match_", startersCols)

starters <- cbind.data.frame(startersTeams, startersNames) %>%
  rename(team = "match_")

facing <- starters %>%
  pivot_longer(starts_with("match_"), names_to = "date1", values_to = "oppTeam") %>%
  pivot_longer(!c(team, date1, oppTeam), names_to = "date2", values_to = "pitcher") %>%
  mutate(pitcher = stri_trans_general(pitcher, "Latin-ASCII")) %>%
  mutate(date1 = gsub("match_", "", date1)) %>%
  filter(date1 == date2) %>%
  select(-date2) %>%
  rename(date = date1) %>%
  mutate(hand = str_extract(pitcher, "\\(\\w\\)")) %>%
  mutate(hand = ifelse(hand == "(R)", 1, 0)) %>%
  mutate(pitcher = gsub(" \\(\\w\\)", "", pitcher), 
         oppTeam = gsub("^\\@ ", "", oppTeam))

# clean pitch stats

pitchFinal <- pstats %>%
  mutate(Name = stri_trans_general(Name, "Latin-ASCII")) %>%
  rename(pitcher = Name) %>%
  right_join(facing, by = "pitcher") %>%
  select(-X.U.FEFF.., -Team, -IP, -starts_with("X"))

pitchFinal <- pitchAmount %>%
  mutate(Name = stri_trans_general(Name, "Latin-ASCII")) %>%
  select(pitcher = Name, matches("^[[:upper:]]{2}\\.$")) %>%
  mutate(across(ends_with("."), noNA)) %>%
  right_join(pitchFinal)

rm(facing, pstats, starters, startersNames, startersTeams, startersCols)

matchups <- merge(batstats, pitchFinal, by.x = "team", by.y = "oppTeam") %>%
  rename(pTeam = team.y) %>%
  relocate(name, date, team, pitcher, pTeam) %>%
  select(-PA) %>%
  filter(!date %in% "1-Apr") %>%
  mutate(date = as.Date(date, tryFormats = "%d-%b")) %>%
  arrange(name, date)

batmatchW <- matchups %>%
  select(matches("[[:upper:]]\\.x$")) %>%
  as.matrix()

pitmatchAM <- matchups %>%
  select(ends_with(".")) %>%
  select(-XX.) %>%
  as.matrix()

pitmatchW <- matchups %>%
  select(matches("[[:upper:]]\\.y$")) %>%
  as.matrix()


matchups$value <- rowSums(pitmatchAM * (batmatchW - pitmatchW), na.rm = T)

matchups$value[matchups$value %in% 0] <- mean(matchups$value)
matchups$value <- matchups$value + abs(min(matchups$value))

deets <- matchups %>%
  mutate(md = 1/(mday(date) - 1)) %>%
  group_by(name) %>%
  summarise(avgValue = mean(value),
            totalValue = sum(value),
            dayWghtValue = sum(value * md)) %>%
  arrange(-dayWghtValue)

deets

deets[deets$name %in% "Christopher Morel", ]

matchups[matchups$date %in% "2024-04-02", c("name", "value")] %>%
  arrange(-value)

matchups[matchups$date %in% "2024-04-02",] %>%
  filter(name %in% "Christopher Morel")

batstats[batstats$name %in% "Christopher Morel", ]
batstats[batstats$name %in% "Juan Soto", ]

pitchFinal
