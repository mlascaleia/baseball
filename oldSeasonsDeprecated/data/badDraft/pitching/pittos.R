# scored by...

# Innings Pitched (IP)3
# Hits Allowed (H)-1
# Earned Runs (ER)-2
# Walks Issued (BB)-1
# Strikeouts (K)1
# Wins (W)5
# Losses (L)-5
# Saves (SV)5

pp <- read.csv("data/pitching/pitchingStats_better.csv")


pp %>%
  mutate(score = 3 * IP + -1 * H + -1 * ER + -1 * BB + -1 * IBB + SO + 5 * W + 5 * L + 5 * SV) %>%
  arrange(-score)
  


