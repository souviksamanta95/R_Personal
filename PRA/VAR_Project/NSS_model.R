rm(list=ls())
options (scipen=99999)

library(xts)
library(YieldCurve)
library(lattice)

# Reading the CSV file
# df <- read.csv(file.choose())
df <- read.csv("/media/souvik/Analytics/R/PRA/VAR_Project/Monthly_ZCYC.csv")

# Making a xts class time series
xdf <- as.xts(read.zoo(df, FUN = as.Date, format='%d/%m/%Y'))

# Extracting NSS as estimated by CCIL
CCIL <- xdf[,1:6]

# Extracting maturity matrix
mat <- xdf[,7:67]

# Extracting yield matrix
yld <- xdf[,68:128]

# Plot of yields for different time frames
xyplot.ts(yld,scales=list(y=list(relation="same")),ylab="Yield (%)")

# Combined plot
xyplot.ts(yld,superpose=TRUE,auto.key=list(columns=4),ylab="Yield (%)")

# Defining variables for any random day
i <- 56
rate <- yld[i]
maturity <- as.numeric(df[1,8:68])

# Fitting Nelson Siegel model
NSParameters <- Nelson.Siegel(rate= rate, maturity=maturity)
n_rate <- NSrates(NSParameters, maturity)
plot(maturity,rate[1,],main="Fitting Nelson-Siegel yield curve", type="o")
lines(maturity,n_rate, col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"), col=c(1,2),lty=1)

# Fitting Svensson function
SvenssonParameters <- Svensson(rate = rate, maturity = maturity)
s_rate <- Srates( SvenssonParameters ,maturity,"Spot")
plot(maturity, rate[1,],main="Fitting Svensson yield curve", type="o")
lines(maturity, s_rate, col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"), col=c(1,2),lty=1)


k <- df[56,]

write.csv(k, "/media/souvik/Analytics/R/PRA/VAR_Project/sample.csv")




