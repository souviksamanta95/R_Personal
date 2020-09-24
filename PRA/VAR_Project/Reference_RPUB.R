library(dplyr)
library(YieldCurve)
library(ustyc)

yc <- getYieldCurve()

yc_xts = xts(yc$df, order.by=as.Date(rownames(yc$df)))
save(yc_xts, file = "yc_xts.rda")
load("yc_xts.rda")


x10y <- yc_xts %>% subset(select = "BC_10YEAR")
x3m <- yc_xts %>% subset(select = "BC_3MONTH")

x10y1 <- x10y['2002/2017']
x3m1 <- x3m['2002/2017']

x10y2 <- x10y['2017']
x3m2 <- x3m['2017']


isNotZero = apply(x10y1, 1, function(row) all(row != 0 ))
x10y1z <- x10y1[isNotZero,]

isNotZero = apply(x3m1, 1, function(row) all(row != 0 ))
x3m1z <- x3m1[isNotZero,]
plot.xts(cbind(x10y1z,x3m1z), col = c("red", "blue"),ylim=c(0,6), main="10 year and 3 month rate")
title(main="The interest rates of the Federal Reserve",
    xlab="Date", ylab="Interest Rate by %")
legend("topleft",legend=c("10 Years Yield","3 Months Yield"),
col=c("red", "blue"),lty=1)

# Remove zero

isNotZero = apply(x10y2, 1, function(row) all(row != 0 ))
x10y2z <- x10y2[isNotZero,]

isNotZero = apply(x3m2, 1, function(row) all(row != 0 ))
x3m2z <- x3m2[isNotZero,]
plot.xts(cbind(x10y2z,x3m2z), ylim=c(0,3), col = c("red", "blue"), main="10 year and 3 month rate")
title(main="The interest rates of the Federal Reserve",
    xlab="Date by Month", ylab="Interest Rate by %")
legend("topleft",legend=c("10 Years Yield","3 Months Yield"),
col=c("red", "blue"),lty=1)

require(ustyc)
yc <- getYieldCurve()

summary(yc)
head(yc$df)

require(xts)
require(lattice)

xt = xts(yc$df,order.by=as.Date(rownames(yc$df)))
xyplot.ts(xt,scales=list(y=list(relation="same")),ylab="Yield (%)")

xyplot.ts(xt,superpose=TRUE,auto.key=list(columns=4),ylab="Yield (%)")

treasury.maturity <- c(1/12,3/12,6/12,1,2,3,5,7,10,20,30)
mv = max(tail(yc$df,n=180))
g <- gray.colors(6,start=0.3,end=0.9,gamma=2.2)

plot(treasury.maturity,
     yc$df[nrow(yc$df),-12],
     type="l",
     main="US Treasury Constant Maturity Yield Curve",
     sub="Current Values And Change Over Six Months",
     xlab="Maturity (yr)",
     ylab="Yield (%)",
     lwd=2,
     ylim=c(0.0,mv),
     col="darkred")

points(treasury.maturity,
       as.numeric(yc$df[nrow(yc$df),-12]),
       col="darkred")

for ( i in 1:6 ) {
  lines(treasury.maturity,
        as.numeric(yc$df[nrow(yc$df)-i*30,-12]),
        type="l",
        col=g[i])
}

mtext(yc$updated,side=3,col="darkgray",cex=0.7)

### Nelson.Siegel function and Fed data-set ###

data(FedYieldCurve)
rate.Fed = first(FedYieldCurve,'5 month')
maturity.Fed <- c(0.25, 0.5, 1,2,3,5,7,10)
NSParameters <- Nelson.Siegel( rate= rate.Fed, maturity=maturity.Fed )

y <- NSrates(NSParameters[5,], maturity.Fed)

plot(maturity.Fed,rate.Fed[5,],main="Fitting Nelson-Siegel yield curve", type="o")
lines(maturity.Fed,y, col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"),
col=c(1,2),lty=1)

### Svensson function and ECB data-set ###
data(ECBYieldCurve)
rate.ECB = ECBYieldCurve[1:5,]
maturity.ECB = c(0.25,0.5,seq(1,30,by=1))
SvenssonParameters <- Svensson(rate.ECB, maturity.ECB)
Svensson.rate <- Srates( SvenssonParameters ,maturity.ECB,"Spot")
plot(maturity.ECB, rate.ECB[5,],main="Fitting Svensson yield curve", type="o")
lines(maturity.ECB, Svensson.rate[5,], col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"),
col=c(1,2),lty=1)


















