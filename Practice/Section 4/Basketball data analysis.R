Games
round(MinutesPlayed/Games)

library(ggplot2)

matplot(t(FieldGoals/FieldGoalAttempts),type="b", pch=15:20, col=c(1:4,6))


legend("bottomleft", inset=0.01, legend=Players, pch=15:20, col=c(1:4,6), horiz=F)
