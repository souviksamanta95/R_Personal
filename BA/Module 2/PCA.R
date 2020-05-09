# install.packages("corpcor")
# install.packages("GPArotation")
# install.packages("psych")
# install.packages("REdaS")

setwd("D:/Analytics/R/BA/Online")

library(corpcor)
library(GPArotation)
library(psych)
library(REdaS)
library(readxl)

# Principal component analysis and factor analysis
car <- read_excel("CAR.xlsx")
car
# Descriptive statistics
summary(car)
cor(car)
cortest.bartlett(car) # significance to check whether factor analysis can be done
KMOS(car)

# Principal component analysis
Upca <- princomp(car, scores = TRUE, cor = TRUE)
summary(Upca)

# Loading of pt=rincipal components
loadings(Upca)

# Scree plot of eigen values
plot(Upca)
screeplot(Upca, type = "line", main = "Scree plot")
# Biplot of score variables
biplot(Upca)
# Scores of the components
Upca$scores
# Rotation
# Varimax
# Factor component analysis with rotation
Rpca <- principal()
Rpca

print.psych(Rpca, cut = 0.3, sort = TRUE)

SaveRpca <- principal(car, nfactors = 5, rotate = "varimax", scores = TRUE)
SaveRpca
SaveRpca$scores
NMAT <- SaveRpca$scores
cor(NMAT)


install.packages("Factoshiny")
library(Factoshiny)
data(car)
result <- Factoshiny(car)

