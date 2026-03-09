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
fasinit <- read.csv("moreStreamlinedApr10/sheets/FAs.csv")
starters <- read.csv("findmeOutfielderApr1/sheets/startingPs.csv", encoding = "UTF-8")
batstats <- read.csv("findmeOutfielderApr1/sheets/batData.csv", encoding = "UTF-8")
pstats <- read.csv("findmeOutfielderApr1/sheets/pitchData.csv", encoding = "UTF-8")
batAmount <- read.csv("findmeOutfielderApr1/sheets/batAmount.csv", encoding = "UTF-8")
pitchAmount <- read.csv("findmeOutfielderApr1/sheets/pitchAmount.csv", encoding = "UTF-8")


rhb <- read.csv("findmeOutfielderApr1/sheets/rhb.csv", encoding = "UTF-8")
lhb <- read.csv("findmeOutfielderApr1/sheets/lhb.csv", encoding = "UTF-8")
bhb <- read.csv("findmeOutfielderApr1/sheets/bhb.csv", encoding = "UTF-8")

hb <- rbind(rhb, lhb, bhb) %>%
  rename(name = X.U.FEFF.name) %>%
  mutate(name = gsub("[[:upper:]]\\. ", "", name)) %>%
  mutate(name = stri_trans_general(name, "Latin-ASCII"))

# clean FAs

fasTeam <- fasinit %>%
  rename(name = ï..name) %>%
  filter(grepl("^\\w{2,3}..(,|$)", name)) %>%
  mutate(team = str_extract(name, "^...")) %>%
  mutate(team = toupper(gsub("(O|\\d)$", "", team))) %>%
  select(team) %>%
  unlist()

fas <- fasinit %>%
  rename(name = ï..name) %>%
  mutate(name = gsub("(S$|DTD$)", "", name)) %>%
  mutate(name = gsub("[[:upper:]]\\. ", "", name)) %>%
  filter(!n1 %in% "") %>%
  mutate(team = fasTeam) %>%
  select(name, team)

myTeam <- data.frame(name = c("Christian Yelich", "Lourdes Gurriel Jr.",
                     "Chas McCormick", "James Outman", "Michael Conforto",
                     "Nelson Velazquez"),
                     team = c("MIL", "ARI", "HOU", "LAD", "SFG", "KCR"))

fas <- rbind(fas, myTeam)                     

# clean batstats

batstats <- batstats %>%
  mutate(Name = stri_trans_general(Name, "Latin-ASCII")) %>%
  mutate(Name = gsub("[[:upper:]]\\. ", "", Name)) %>%
  select(-X.U.FEFF.., -Team) %>%
  mutate(across(starts_with("w"), noNA)) %>%
  rename(name = Name) %>%
  left_join(hb, by = "name") %>%
  right_join(fas, by = "name") %>%
  filter(!is.na(PA))

# batstats <- batAmount %>%
#   mutate(Name = stri_trans_general(Name, "Latin-ASCII")) %>%
#   mutate(Name = gsub("[[:upper:]]\\. ", "", Name)) %>%
#   select(name = Name, matches("^[[:upper:]]{2}\\.$")) %>%
#   mutate(across(ends_with("."), noNA)) %>%
#   right_join(batstats)



lgj <- c(1.4,	1.0,	-0.5,	0.3,	-0.4,	1.8,	0.0, 0,0,0)
batstats[batstats$name %in% "Lourdes Gurriel Jr.", 14:23] <- lgj

rm(bhb,fas,fasinit,hb,lhb,rhb,fasTeam, lgj)

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
         oppTeam = gsub("^\\@ ", "", oppTeam)) %>%
  filter(!is.na(hand))

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
  relocate(name, date, team, pitcher, pTeam, hb, hand) %>%
  select(-PA) %>%
  filter(!date %in% "1-Apr") %>%
  mutate(date = as.Date(date, tryFormats = "%d-%b")) %>%
  arrange(name, date)

batmatchW <- matchups %>%
  select(matches("[[:upper:]]\\.x$")) %>%
  as.matrix()

pitmatchAM <- matchups %>%
  select(ends_with("..y")) %>%
  select(-XX..y) %>%
  as.matrix()

pitmatchW <- matchups %>%
  select(matches("[[:upper:]]\\.y$")) %>%
  as.matrix()




matchups$value <- rowSums(pitmatchAM * (batmatchW - pitmatchW), na.rm = T)

matchups$value[matchups$value %in% 0] <- mean(matchups$value)
matchups$value <- matchups$value + abs(min(matchups$value))

matchups$myTeam <- 0
matchups$myTeam[matchups$name %in% myTeam$name] <- 1

deets <- matchups %>%
  mutate(md = 1/(mday(date) - 1), batGood = ifelse(hb == hand, 0.5, 1.5)) %>%
  group_by(name) %>%
  summarise(avgValue = mean(value),
            totalValue = sum(value),
            dayWghtValue = sum(value * md),
            handValue = sum(value * batGood),
            handDayValue = sum(value  * batGood * md)) %>%
  arrange(-dayWghtValue)

deets
deets %>% arrange(-handDayValue)
deets[deets$name %in% "Christopher Morel", ]

matchups[matchups$date %in% "2024-04-02", c("name", "value", "myTeam")] %>%
  arrange(-value)

matchups %>%
  filter(name %in% "Jose Siri")


