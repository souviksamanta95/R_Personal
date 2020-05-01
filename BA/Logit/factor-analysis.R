setwd("D:/Analytics/R/BA/Logit")
fact_1 <- read.csv("factor_2.csv")
head(fact_1)
fact_1.pca <- princomp(fact_1)
summary(fact_1.pca)
plot(fact_1.pca)
fact_2 <- factanal(fact_1, factors = 3, rotation = "varimax")
summary(fact_2)
fact_2
fact_3 <- factanal(fact_1,
                   factors = 3,
                   rotation = "varimax",
                   scores = "regression"
                   )
fact_3

