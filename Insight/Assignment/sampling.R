getwd()
setwd("C:/Users/souvi/Documents/R/Insight/Assignment")
data <- read.csv("games.csv")

library(dplyr)

# Subsetting relevant data --- (removing the draw matches)

game <- data %>% filter(winner!="draw") %>% select(id, victory_status, winner, white_rating, black_rating)


# Creating new column where:
#      1 >> Where higher rated player wins
#      0 >> Where higher rated player loses

game <- game %>% mutate(expected_win = ifelse(white_rating>black_rating&winner=="white",1,ifelse(white_rating<black_rating&winner=="black",1,0)))

mean(game$expected_win)
sd(game$expected_win)

# Sampling of data ---

set.seed(12345)

s <- game[sample(1:nrow(game),1000),]

mean(s$expected_win)

# H0 : At least 70% of time the higher rated player wins the match

tt <- t.test(s$expected_win, alternative="less", mu = 0.7, conf.level = 0.95)

tt
