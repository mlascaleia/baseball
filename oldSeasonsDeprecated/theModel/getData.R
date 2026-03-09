# intialize

rm(list = ls())

library(baseballr)
library(dplyr)

# search
source("theModel/newStatcast_search.R")

bb_dates <- as.Date(as.Date("2024-03-20"):Sys.Date())

statos <- lapply(bb_dates, FUN = function(b){
  oo <- statcast_search(start_date = b, end_date = b)
  print(cat("Did ", as.Date(b), "\n"))
  return(oo)
})

noPlay <- lapply(statos, nrow) == 0
statos <- statos[!noPlay]
as <- do.call(rbind, statos) %>%
  mutate(location_x = hc_x - 125.42,
         location_y = 198.27 - hc_y)

as[as$description %in% "foul", ][1:10, ]

hap <- as[!as$events %in% ""]
hip <- as[as$description %in% "hit_into_play", ]  

hip$good <- 0  
hip$good[hip$events %in% c("single", "double", "triple", "home_run")] <- 1

write.csv(hip, "theModel/theData/hip.csv")  
write.csv(as, "theModel/theData/all2024.csv")  
  
# get FULL 2023 season  

bb_dates <- as.Date(as.Date("2023-03-30"):as.Date("2023-10-01"))

statos23 <- lapply(bb_dates, FUN = function(b){
  oo <- statcast_search(start_date = b, end_date = b)
  charDate <- as.character(b)
  cat("Completed download of", nrow(oo), "pitches from", charDate, "\n")
  return(oo)
})

noPlay <- lapply(statos23, nrow) == 0
statos23 <- statos23[!noPlay]
as23 <- do.call(rbind, statos23) %>%
  mutate(location_x = hc_x - 125.42,
         location_y = 198.27 - hc_y)
  
write.csv(as23, "data/all2023Pitches.csv", row.names = F)






