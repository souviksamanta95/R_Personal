rm(list=ls())
options (scipen=99999)

library(xts)
library(YieldCurve)
library(lattice)

# Reading the CSV file
# df <- read.csv(file.choose())
# df <- read.csv("/media/souvik/Analytics/R/PRA/VAR_Project/Monthly_ZCYC.csv")
df <- read.csv("yield.csv")

# Making a xts class time series
xdf <- as.xts(read.zoo(df, FUN = as.Date, format='%d-%m-%Y'))

# Defining maturity for yield curve
maturity <- c(0.25,0.5,1,2,3,4,5,6,7,8,9,10,15,20,30)

# Extracting yield matrix
rate <- xdf[,1:15]

# Fitting Nelson Siegel model
NSParameters <- Nelson.Siegel(rate= rate, maturity=maturity)

# Plotting reults for any day
i <- 58
n_rate <- NSrates(NSParameters[i,], maturity)
plot(maturity,rate[i,],main="Fitting Nelson-Siegel yield curve", type="o")
lines(maturity,n_rate, col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"), col=c(1,2),lty=1)

# Fitting Svensson function
SvenssonParameters <- Svensson(rate = rate, maturity = maturity)

# Plotting reults for any day
i <- 45
s_rate <- Srates( SvenssonParameters[i,] ,maturity,"Spot")
plot(maturity, rate[i,],main="Fitting Svensson yield curve", type="o")
lines(maturity, s_rate, col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"), col=c(1,2),lty=1)

# Creating Data frame with dates for export
df_NS <- cbind(df[1], as.data.frame(NSParameters))
df_NSS <- cbind(df[1], as.data.frame(SvenssonParameters))

# Exporting outputs
write.csv(df_NS, "NS.csv", row.names = F)
write.csv(df_NSS, "NSS.csv", row.names = F)
