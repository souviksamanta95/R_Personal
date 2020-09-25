library(xts)
library(YieldCurve)
library(lattice)
library(plotly)

# Reading the CSV file
# df <- read.csv(file.choose())
df <- read.csv("/media/souvik/Analytics/R/PRA/VAR_Project/NSS_out.csv")

# Defining Time - y axis
y <- dt <- as.Date(df$Date)

# Defining Maturity - x axis
mat <- read.csv("/media/souvik/Analytics/R/PRA/VAR_Project/maturity.csv")
x <- maturity <- mat$Maturity

# Making a xts class time series
xdf_NSS <- as.xts(read.zoo(df, FUN = as.Date, format='%d/%m/%Y'))

# Plotting reults for any day
i <- 45
s_rate <- Srates(xdf_NSS,maturity,"Spot")
plot(maturity, s_rate,main="Fitting Svensson yield curve", type="o")

z <- s_rate

ts_surface(USgas)



plot_model(model.obj = NSParameters)

data(USgas)



# Defining Plotly
axx <- list(title = "X AXIS TITLE")

axy <- list(title = "Y AXIS TITLE")

axz <- list(title = "Z AXIS TITLE")

x <- 70*(runif(70, 0, 1))
y <- 55*(runif(70, 0, 1))
z <- 40*(runif(70, 0, 1))

fig <- plot_ly(x = ~x, y = ~y, z = ~z, type = 'mesh3d')
fig <- fig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))

fig







