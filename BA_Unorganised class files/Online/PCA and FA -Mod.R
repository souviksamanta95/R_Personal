#We will need the packages corpcor, GPArotation (for rotating) and psych (for the factor analysis). 
install.packages("corpcor"); install.packages("GPArotation"); 
install.packages("psych")

#Then we need to load the packages by executing these commands:
library(corpcor); library(GPArotation); library(psych)


# Principal Component Analysis and Factor Analysis in R

CAR <- read_excel("C:/Users/surender.kumar/Desktop/Factor Analysis/CAR.xlsx")
View(CAR)
# Descriptive statistics
summary(CAR)
cor(CAR)
cortest.bartlett(CAR)
library(REdaS)
KMOS(CAR)

# Principal component analysis
Upca <- princomp(CAR, scores=TRUE, cor=TRUE)
summary(Upca)

# Loadings of principal components
loadings(Upca)
#Upca$loadings
# Scree plot of eigenvalues
plot(Upca)
screeplot(Upca, type="line", main="Scree Plot")
# Biplot of score variables
biplot(Upca)
# Scores of the components
Upca$scores
# Rotation
# varimax
# Factor Component Analysis - with Rotation
Rpca <- principal(CAR, nfactors = 5, rotate = "varimax")
Rpca

print.psych(Rpca, cut = 0.3, sort = TRUE)

SaveRpca <- principal(CAR, nfactors=5, rotate="varimax", scores = TRUE)
SaveRpca
SaveRpca$scores
NMAT <- SaveRpca$scores
cor(NMAT)

# Additional package to get detail analysis and report.

install.packages("Factoshiny")
library(Factoshiny)
data(CAR)
result <- Factoshiny(CAR)





