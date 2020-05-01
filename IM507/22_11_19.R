# Basic graphs

aq <- airquality

# For plotting whole dataset
plot(aq)

# Histogram
hist(aq$Ozone)
?hist()
hist(aq$Ozone, ylim = c(0,60))
hist(aq$Ozone, main = "Ozone Histogram",
     xlab = "Ozone",
     col = "lightblue",
     labels = T,
     breaks = 10,
     ylim = c(0,60))
hist(aq$Ozone, main = "Ozone Histogram",
     xlab = "Ozone",
     col = "lightblue",
     labels = T,
     breaks = 5,
     ylim = c(0,60))

plot(aq$Ozone, type = "l")
plot(aq$Ozone, type = "h")

# Boxplot
boxplot(aq$Ozone)


plot(aq$Month,aq$Ozone)

# Splitting data monthwise

s<- split(aq, aq$Month)
s
typeof(s)
class(s)

# lapply function

lapply(s,function(x){
  colMeans(x[,c("Ozone","Wind")],na.rm = T)
})

# sapply function

sapply(s,function(x){
  colMeans(x[,c("Ozone","Wind")],na.rm = T)
})

# tapply function

tapply(aq$Temp,aq$Month,mean)  # Note, variable should be categorical otherwise error will be there

# dplyr and tidyverse package  - advanced case

#install.packages("dplyr")
#install.packages("tidyverse")

library(dplyr)
library(tidyverse)

# dplyr :
# mutate()  select()  filter()  summerise()   arrange()


select(aq,Ozone,Wind)

aq %>% select(Ozone,Wind,Temp,Month)

aq %>% filter(Month==7|Month==9)

aq %>% mutate(Temp_Cat=ifelse(Temp>90,"H","N"))

aq %>% group_by(Month) %>%
  summarize(mean(Ozone))


aq %>% select(Ozone,Wind,Temp,Month) %>%
  filter(Month==7|Month==9)%>%
  mutate(Temp_Cat=ifelse(Temp>90,"H","N"))%>%
  group_by(Month)%>%
  summarize(Mean_Ozone=mean(Ozone))%>%
  arrange(Mean_Ozone)


