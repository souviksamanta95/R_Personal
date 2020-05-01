getwd()
setwd("C:/Users/souvi/Documents/R/Insight/Bollywood data")
library(readxl)
excel_sheets("Bollywood Data.xlsx")
data <- read_xlsx("Bollywood Data.xlsx", sheet = "Bollywood Data")

head(data)
str(data)
summary(data)

#------------------ Variable Conversion -----------------------------

data$`Release Date` <-  as.Date(data$`Release Date`)
unique(data$`Release Date (N / LW / Festive)`)
data$`Release Date (N / LW / Festive)` <- factor(data$`Release Date (N / LW / Festive)`, levels = c("LW","N","HS","FS"), labels = c("Long weekend", "Normal", "Holiday season", "Festive season") )
unique(data$`Genre - Defined`)
data$`Genre - Defined` <- factor(data$`Genre - Defined`, levels = c("Romance" ,"Thriller","Comedy","Drama","Action"))

#------------------ Missing values -----------------------------

sapply(data, function(x)sum(is.na(x)))
which(is.na(data$`S No`))
which(is.na(data$`Release Date`))
which(is.na(data$`Movie Name`))
which(is.na(data$`Release Date (N / LW / Festive)`))
which(is.na(data$`Genre - Defined`))
which(is.na(data$Budget))
which(is.na(data$`Youtube Views`))
which(is.na(data$`Youtube Likes`))
which(is.na(data$`Youtube Dislikes`))
# all missing values are in same row, so removing that row ---

nrow(data)
data <- data[-150,]
nrow(data)

sum(is.na(data))
# No missing values ---------------------

names(data)
cvp <- data%>% select(Budget,`Box Office Collection`,`Youtube Views`,`Youtube Likes`,`Youtube Dislikes`)
cvp<-data.frame(cvp)
cvp
m <- cor(cvp)
m

library(ggplot2)
library(dplyr)

p1 <- ggplot(data, aes(x=Budget, y=`Box Office Collection`))

p1 + geom_point(aes(color=`Genre - Defined`))
p1 + geom_point(aes(color=`Genre - Defined`, size = `Youtube Views`))
p1 + geom_point(aes(color=`Genre - Defined`, shape = `Release Date (N / LW / Festive)`))


p2 <- ggplot(data, aes(x=`Youtube Views`, y=`Box Office Collection`))

p2 +  geom_point(aes(color=`Genre - Defined`,size=`Youtube Likes`))
p2 +  geom_smooth(aes(color=`Genre - Defined`,size=`Youtube Likes`))

p3 <- ggplot(data, aes(x=`Release Date (N / LW / Festive)`,y=`Box Office Collection`))

p3 +  geom_bar(stat = "identity", aes(fill=`Genre - Defined`))
p3 +  geom_bar(stat = "identity", fill = "light blue")+
  facet_grid(`Genre - Defined`~.,scales = "free")+ theme_classic()


# Mean Budget by Genre
p4 <- data %>% group_by(Genre = `Genre - Defined`)%>%
  summarize(Mean = mean(Budget)) %>% arrange(desc(Mean))

ggplot(p4,aes(x=Genre,y=Mean)) + geom_bar(stat = "identity", fill = "light blue")

# Mean box office collection by Genre
p5 <- data %>% group_by(Genre = `Genre - Defined`)%>%
  summarize(Mean = mean(`Box Office Collection`)) %>% arrange(desc(Mean))

ggplot(p5,aes(x=Genre,y=Mean)) + geom_bar(stat = "identity", fill = "light blue")

# Mean Budget vs mean box office collection by Genre
p6 <- data %>% group_by(Genre = `Genre - Defined`)%>%
  summarize(Mean_Budget = mean(Budget),Mean_Collection = mean(`Box Office Collection`))
ggplot(p6,aes(x=Mean_Budget,y=Mean_Collection,size=Genre,color=Genre))+
  geom_point()

# Youtube views by Genre
p7 <- ggplot(data, aes(x=`Genre - Defined`,y=`Youtube Views`))

p7 + geom_bar(stat = "identity", fill = "light blue")

# Youtube Likes/Views proportion by genre

p8 <- data %>% mutate(Like_View = `Youtube Likes`/`Youtube Views`) %>% group_by(Genre = `Genre - Defined`)%>%
  summarize(Like_View_Ratio = mean(Like_View))

ggplot(p8,aes(x=Genre,y=Like_View_Ratio)) + geom_bar(stat = "identity", fill = "light blue")


