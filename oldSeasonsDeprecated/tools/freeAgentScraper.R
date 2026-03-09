# find the free agents automatically

library(tidyverse)
library(RSelenium)
library(rvest)
library(stringi)

# useful things

positions <- c("C", "1B", "2B", "SS", "3B", "OF", "DH", "SP", "RP")

teams <- c("Ari", "Atl", "Bal", "Bos", "ChC", 
           "ChW", "Cin", "Cle", "Col", "Det", "Hou", "KC", "LAA", "LAD", "Mia", "Mil", 
           "Min", "NYM", "NYY", "Oak", "Phi", "Pit", "SD", "SF", "Sea", "Stl",
           "TB", "Tex", "Tor", "Wsh")

injuries <- c("IL10", "IL15", "O", "IL60", "DTD")

pitches <- c("FA", "FC", "FS", "SI", "CH",
             "SL", "CU", "CS", "KN", "SB")

noPercent <- function(colo){
  return(as.numeric(gsub("%", "", colo))/100)
}

noNA <- function(colo){
  return(ifelse(is.na(colo), 0, colo))
}

espnScrape <- function(xx, injo = injuries, myTeam = F){
  Sys.sleep(2)
  page_html <- remd$getPageSource()
  pag <- read_html(page_html[[1]])
  
  nemos <- pag %>%
    html_elements(css = ".truncate:nth-child(1) .pointer") %>%
    html_text()
  
  nemos[nemos %in% "Christian Encarnacion-Strand"] <- "Christian Encarnacion-St..."
  nemos[nemos %in% "Michael Taylor"] <- "Michael A. Taylor"
  
  posits <- pag %>%
    html_elements(css = ".playerinfo__playerpos") %>%
    html_text()
  
  tems <- pag %>%
    html_elements(css = ".playerinfo__playerteam") %>%
    html_text()
  
  pts <- pag %>%
    html_elements(css = ".Table__TD .total span") %>%
    html_text()
  
  pts[pts %in% "--"] <- 0
  pts <- as.integer(pts)
  
  heal <- pag %>%
    html_elements(css = ".player-info , .injury-status_medium") %>%
    html_text()
  
  j <- 0
  whoInj <- NA
  for(i in 1:length(heal)){
    inj <- heal[i] %in% injo
    if(inj){
      j <- j + 1
      whoInj[j] <- i - j
    }
  }
  
  healReal <- pag %>%
    html_elements(css = ".injury-status_medium") %>%
    html_text()
  
  agents <- data.frame(agent = nemos, position = posits, team = tems, 
                       points = pts, heal = "")
  
  if(!is.na(whoInj[1]))
    agents$heal[whoInj] <- healReal
  
  if(!myTeam){
    nextButton <- remd$findElement(using = "css selector", ".Pagination__Button--next")
    nextButton$clickElement()
  }
  return(agents)
}
fgScrape <- function(zurl, pitch = F){
  remd$navigate(zurl)
  Sys.sleep(2)
  page_html <- remd$getPageSource()
  stats <- read_html(page_html[[1]]) %>%
    html_elements(css = "table") %>%
    html_table()
  
  if(pitch){
    batstats <- stats[[length(stats)]] %>%
      select(-`-- Line Break --`) %>%
      mutate(Name = stri_trans_general(Name, "Latin-ASCII"))
  } else {
    batstats <- stats[[length(stats)]] %>%
      mutate(Name = stri_trans_general(Name, "Latin-ASCII"))
  }
  
  return(batstats)
}

rd <- rsDriver(browser="firefox", port = sample(4444L:4534L, 1))
remd <- rd[["client"]]


rbat <- fgScrape("https://www.fangraphs.com/leaders/major-league?pos=all&stats=bat&lg=all&season=2024&season1=2024&ind=0&pageitems=2000000000&qual=1&team=0&sortcol=14&sortdir=default&hand=R", pitch = T)
lbat <- fgScrape("https://www.fangraphs.com/leaders/major-league?pos=all&stats=bat&lg=all&season=2024&season1=2024&ind=0&pageitems=2000000000&qual=1&team=0&sortcol=14&sortdir=default&hand=L", pitch = T)
sbat <- fgScrape("https://www.fangraphs.com/leaders/major-league?pos=all&stats=bat&lg=all&season=2024&season1=2024&ind=0&pageitems=2000000000&qual=1&team=0&sortcol=14&sortdir=default&hand=S", pitch = T)

rbat$bat <- "R"
lbat$bat <- "L"
sbat$bat <- "S"

hb <- rbind(rbat, lbat, sbat) %>%
  select(agent = Name, bat)

remd$navigate("https://fantasy.espn.com/baseball/players/add?leagueId=1895031955")

aa <- lapply(1:10, espnScrape) # batters
# bb <- lapply(1:10, espnScrape) # pitchers

remd$navigate("https://fantasy.espn.com/baseball/team?leagueId=1895031955&teamId=5")

myTeam <- lapply(1, espnScrape, myTeam = T) # (make sure no empty spaces)

goodTeam <- myTeam[[1]]

goodBatters <- do.call(rbind, aa) %>%
  filter(points > 0) %>%
  filter(heal %in% "") %>%
  rbind(goodTeam) %>%
  filter(!grepl("(SP|RP)", position)) %>%
  left_join(hb)

# goodPichers <- do.call(rbind, bb) %>%
#   filter(heal %in% "")

vsRight_batURL <- "https://www.fangraphs.com/leaders/major-league?pos=all&stats=bat&lg=all&season=2024&season1=2024&ind=0&pageitems=2000000000&qual=1&team=0&type=1&month=14&sortcol=14&sortdir=default&pagenum=1"
vsLeft_batURL <- "https://www.fangraphs.com/leaders/major-league?pos=all&stats=bat&lg=all&season=2024&season1=2024&ind=0&pageitems=2000000000&qual=1&team=0&sortcol=14&sortdir=desc&type=1&month=13&pagenum=1"
vsRight_pitchURL <- "https://www.fangraphs.com/leaders/major-league?pos=all&stats=pit&lg=all&season=2024&season1=2024&ind=0&pageitems=2000000000&team=0&sortcol=16&sortdir=asc&qual=0&type=0&month=14"
vsLeft_pitchURL <- "https://www.fangraphs.com/leaders/major-league?pos=all&stats=pit&lg=all&season=2024&season1=2024&ind=0&pageitems=2000000000&team=0&type=0&month=13&sortcol=16&sortdir=asc&qual=0"

batstats_r <- fgScrape(vsRight_batURL)
batstats_l <- fgScrape(vsLeft_batURL)

pitchstats_r <- fgScrape(vsRight_pitchURL, pitch = T)
pitchstats_l <-fgScrape(vsLeft_pitchURL, pitch = T)

bsr <- batstats_r %>%
  select(Name, starts_with("wOBAwOBA")) %>%
  rename(bwOBA_r = `wOBAwOBA - Weighted On Base Average (Linear Weights)`)

batraa <- batstats_l %>%
  select(Name, starts_with("wOBAwOBA")) %>%
  rename(bwOBA_l = `wOBAwOBA - Weighted On Base Average (Linear Weights)`) %>%
  full_join(bsr) %>%
  mutate(across(starts_with("bwOBA"), noNA)) %>%
  rename(agent = Name)

megged <- merge(goodBatters, batraa, by = "agent")

psr <- pitchstats_r %>%
  select(Name, Team, TBF_r = `TBFTBF - Total Batters Faced`, wOBA_r = wOBA)

pitwoba <- pitchstats_l %>%
  select(Name, Team, TBF_l = `TBFTBF - Total Batters Faced`, wOBA_l = wOBA) %>%
  full_join(psr, by = c("Name", "Team")) %>%
  mutate(across(starts_with("wOBA"), noNA)) %>%
  mutate(across(starts_with("TBF"), noNA)) %>%
  filter(!(Name %in% "Logan Allen" & Team %in% "ARI")) %>%
  select(-Team) %>%
  rename(pitcher = Name)

# get probables ####
remd$navigate("https://www.fangraphs.com/roster-resource/probables-grid")
page_html <- remd$getPageSource()

all <- read_html(page_html[[1]]) %>%
  html_elements(css = "table") %>%
  html_table()

starters <- all[[length(all)]]

fgteams <- unique(starters$Team)
notTeams <-  c("Team", "AL East", "AL Central", "AL West", "NL East", "NL Central", "NL West")

infoRows <- starters[starters$Team %in% notTeams, ]
colnames(starters) <- infoRows[infoRows$Team %in% "AL East", ]

fgteams <- fgteams[!fgteams %in% notTeams]

slong <- starters %>%
  pivot_longer(!`AL East`, names_to = "day", values_to = "pitcher") %>%
  filter(!`AL East` %in% notTeams) %>%
  rename(pTeam = `AL East`)

slong$opp <- gsub("^@ ","",str_extract(slong$pitcher, "^@? ?[[:upper:]]{3}"))
slong$pitcher <- gsub("^@? ?[[:upper:]]{3}","",slong$pitcher)
slong$hand <- str_extract(slong$pitcher, " \\(.\\)$")
slong$hand <- gsub(" ","",slong$hand)
slong$hand <- gsub("\\)","",slong$hand)
slong$hand <- gsub("\\(","",slong$hand)
slong$pitcher <- gsub(" \\(.\\)$","",slong$pitcher)
slong$pitcher <- stri_trans_general(slong$pitcher, "Latin-ASCII")

# merge info ####

batterOpp <- merge(pitwoba, slong, by = "pitcher")

tt <- lapply(1:20, espnScrape) # all batters
goot <- do.call(rbind, tt) %>%
  left_join(hb)

megged <- merge(goot, batraa, by = "agent")

megged$team2 <- megged$team
for(i in 1:length(teams)){
  megged$team2 <- gsub(sort(teams)[i], sort(fgteams)[i], megged$team2)
}

bigmeg <- merge(megged, batterOpp, by.x = "team2", by.y = "opp")

# bigmeg$wRAA_l <- bigmeg$wRAA_l + abs(min(bigmeg$wRAA_l))
# bigmeg$wRAA_r <- bigmeg$wRAA_r + abs(min(bigmeg$wRAA_r))

bigmeg$ll <- bigmeg$bwOBA_l + bigmeg$wOBA_l
bigmeg$lr <- bigmeg$bwOBA_l + bigmeg$wOBA_r
bigmeg$rl <- bigmeg$bwOBA_r + bigmeg$wOBA_l
bigmeg$rr <- bigmeg$bwOBA_r + bigmeg$wOBA_r

bigmeg$value <- NA
bigmeg$value <- ifelse(bigmeg$bat %in% "L" & bigmeg$hand %in% "L", bigmeg$ll, bigmeg$value)
bigmeg$value <- ifelse(bigmeg$bat %in% c("L", "S") & bigmeg$hand %in% "R", bigmeg$lr, bigmeg$value)
bigmeg$value <- ifelse(bigmeg$bat %in% c("R", "S") & bigmeg$hand %in% "L", bigmeg$rl, bigmeg$value)
bigmeg$value <- ifelse(bigmeg$bat %in% "R" & bigmeg$hand %in% "R", bigmeg$rr, bigmeg$value)

nextWeek <- bigmeg[!bigmeg$day %in% c("Mon5/13", "Tue5/14", "Wed5/15", "Mon5/6"), ]

# onlyFree <- nextWeek[nextWeek$agent %in% goodBatters$agent, ]

teamDay <- split(nextWeek, f = paste0(nextWeek$team2, nextWeek$day))

td9 <- lapply(teamDay, function(td){
  arred <- td %>%
    arrange(-points) %>%
    filter(heal %in% c("", "DTD"))
  return(arred[1:9,])
})

nw9 <- do.call(rbind, td9) 

howGood <- nw9 %>%
  group_by(pitcher, team, day) %>%
  summarize(total = sum(value)) %>%
  # mutate(mine = ifelse(agent %in% goodTeam$agent, 1, 0)) %>%
  arrange(total) %>%
  ungroup() %>%
  mutate(rank = 1:nrow(.))

howGood_b <- onlyFree %>%
  group_by(agent, team2, points, position, day) %>%
  summarize(total = sum(value)) %>%
  # mutate(mine = ifelse(agent %in% goodTeam$agent, 1, 0)) %>%
  arrange(-total) %>%
  ungroup() %>%
  mutate(rank = 1:nrow(.))


oof <- howGood_b[grepl("(OF)", howGood_b$position) &
                   howGood_b$day %in% "Tue4/30",] %>%
  mutate(rank = 1:nrow(.))

howGood$total[howGood$total > -1e-8 &
                howGood$total < 1e-8] <- 0


ee <- bigmeg %>%
  group_by(pitcher, day) %>%
  summarise(total = sum(value))


remd$refresh()

remd$close()

