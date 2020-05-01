getwd()
setwd("C:/Users/souvi/Documents/R/Insight/Assignment")
data <- read.csv("games.csv")

library(dplyr)

# Subsetting relevant data --- (removing the draw matches)

game <- data %>% filter(winner!="draw") %>% select(id, victory_status, winner, white_rating, black_rating)


# Creating new column where:
#      T >> Where higher rated player wins
#      F >> Where higher rated player loses

game <- game %>% mutate(expected_win = ifelse(white_rating>black_rating&winner=="white","T",ifelse(white_rating<black_rating&winner=="black","T","F")))

prop <- sum(game$expected_win=="T")/length(game$expected_win)

prop

# Sampling of data ---

set.seed(12345)

n <- 1000
s <- game[sample(1:nrow(game),n),]

x <- sum(s$expected_win=="T")

# H0 : 70% of time the higher rated player wins the match

bt <- binom.test(x, n, p = 0.7, alternative = ("two.sided"), conf.level = 0.95)

bt
