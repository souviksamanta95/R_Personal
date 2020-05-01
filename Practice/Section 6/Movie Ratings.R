getwd()
setwd("C:\\Users\\souvi\\Documents\\R\\Practice\\Section 6")
getwd()

movies <- read.csv("P2-Movie-Ratings.csv")

head(movies)
colnames(movies) <- c("Film","Genre","CriticRating","AudienceRating","BudgetMillions","Year")
tail(movies, n=7)
str(movies)
summary(movies)

#Conversion to a factor

movies$Year <- factor(movies$Year)

summary(movies)
str(movies)

#------------ Aesthetics

library(ggplot2)

ggplot(data=movies, aes(x=CriticRating, y=AudienceRating))

#------------ Geometry

ggplot(data=movies, aes(x=CriticRating, y=AudienceRating)) +
  geom_point()

#------------ Color

ggplot(data=movies, aes(x=CriticRating, y=AudienceRating,
       colour=Genre)) +
  geom_point()

#------------ add size

ggplot(data=movies, aes(x=CriticRating, y=AudienceRating,
       colour=Genre, size=Genre)) +
  geom_point()

#------------ add size in a better way

ggplot(data=movies, aes(x=CriticRating, y=AudienceRating,
                        colour=Genre, size=BudgetMillions)) +
  geom_point()

#>>> This is #1 (We will improve it)

p <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating,
                             colour=Genre, size=BudgetMillions))
# Points
p + geom_point()
# Lines
p + geom_line()
# Multiple Lines
p + geom_line() + geom_point()

# Overriding Aesthetics

q <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating,
                             colour=Genre, size=BudgetMillions))
# ad geom layer

q + geom_point()

#Oveerriding aes
#Ex1
q + geom_point(aes(size=CriticRating))

#Ex2

q + geom_point(aes(color=BudgetMillions))

#Ex3

q + geom_point(aes(x=BudgetMillions)) +
  xlab("Budget Millions $$$") # For renaming the axis name from the original q

#Ex4

p + geom_line() + geom_point()

# Reduce line size

p + geom_line(size=1) + geom_point()

# Mapping vs Setting

#Mapping
p + geom_point(aes(color=Genre))

#Setting
p + geom_point(color="DarkGreen")  #We can't put aes for setting

#Error
p + geom_point(aes(color="DarkGreen"))


# Histograms and Density charts

s <- ggplot(data=movies, aes(x=BudgetMillions))
s + geom_histogram(binwidth = 5)

# Add Color
# Setting Color
s + geom_histogram(binwidth = 5, fill="green")

# Mapping Color
s + geom_histogram(binwidth = 5, aes(fill=Genre))

# Adding border to the colors

s + geom_histogram(binwidth = 5, aes(fill=Genre), color="Black")

#>>> 3 (We will improve it)

# Density Charts 
s + geom_density(aes(fill=Genre)) # Overlapping

s + geom_density(aes(fill=Genre), position = "stack") # Stacking

# Starting layer tips

t <- ggplot(data=movies, aes(x=AudienceRating))
t + geom_histogram(binwidth = 40,
                   fill="White", color="blue")
# Another way
t <- ggplot(data=movies)
t + geom_histogram(binwidth = 40,
                   aes(x=AudienceRating),
                   fill="White", color="blue")

# Statistical Transformation

u <- ggplot(data=movies, aes(x=CriticRating,
                             y=AudienceRating, color=Genre))
u + geom_point() + geom_smooth(fill=NA)

# Boxplot

u <- ggplot(data=movies, aes(x=Genre,
                             y=AudienceRating, color=Genre))
u + geom_boxplot()
u + geom_boxplot(size=1.2)
u + geom_boxplot(size=1.2) + geom_point()

# Trick / hack

u + geom_boxplot(size=1.2) + geom_jitter()

# Another way 

u + geom_jitter() + geom_boxplot(size=1.2, alpha=0.5)

# Using facets

v <- ggplot(data=movies, aes(x=BudgetMillions))
v + geom_histogram(binwidth=10, aes(fill=Genre),
                   color="black")
  # Facets
v + geom_histogram(binwidth=10, aes(fill=Genre),
                   color="black") + 
  facet_grid(Genre~., scales = "free")

# Scatterplots

w <- ggplot(data=movies, aes(x=CriticRating,
                             y=AudienceRating, color=Genre))
w + geom_point(size=3)

# Facets

w + geom_point(size=3) + facet_grid(Genre~.)

w + geom_point(size=3) + facet_grid(.~Year)

w + geom_point(size=3) + facet_grid(Genre~Year)

w + geom_point(size=3) +
  geom_smooth() +facet_grid(Genre~Year)

w + geom_point(aes(size=BudgetMillions)) +
  geom_smooth() +facet_grid(Genre~Year)

# Coordinates : (Limits, Zoom)

m <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating,
                             size=BudgetMillions, color=Genre))
m + geom_point()

m + geom_point() +
  xlim(50,100) +
  ylim(50,100)

#Won't work always

n <- ggplot(data=movies, aes(x=BudgetMillions))
n + geom_histogram(binwidth=10, aes(fill=Genre),
                   color="black")+
  ylim(0,50)

#Instead - zoom
n + geom_histogram(binwidth=10, aes(fill=Genre),
                   color="black")+
  coord_cartesian(ylim=c(0,50))

# Improved graph

w + geom_point(aes(size=BudgetMillions)) +
  geom_smooth() +facet_grid(Genre~Year) +
  coord_cartesian(ylim = c(0,100))

# Themes

o <- ggplot(data=movies, aes(x=BudgetMillions))
h <- o + geom_histogram(binwidth = 10, aes(fill=Genre), color="Black")
h
# Axis Lebel

h +
  xlab("Money Axis") +
  ylab("Number of Movies")

# lebel formatting

h +
  xlab("Money Axis") +
  ylab("Number of Movies") +
  theme(axis.title.x = element_text(color = "darkgreen", size = 30) ,
        axis.title.y = element_text(color = "darkred", size = 30) )

# Tick mark formatting

h +
  xlab("Money Axis") +
  ylab("Number of Movies") +
  theme(axis.title.x = element_text(color = "darkgreen", size = 30),
        axis.title.y = element_text(color = "darkred", size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# Legend formatting

h +
  xlab("Money Axis") +
  ylab("Number of Movies") +
  theme(axis.title.x = element_text(color = "darkgreen", size = 30),
        axis.title.y = element_text(color = "darkred", size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        legend.position = c(1,1),
        legend.justification = c(1,1))


# Title to the Plot :

h +
  xlab("Money Axis") +
  ylab("Number of Movies") +
  ggtitle("Movie Budget Distribution")+
  theme(axis.title.x = element_text(color = "darkgreen", size = 30),
        axis.title.y = element_text(color = "darkred", size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        
        plot.title = element_text(color = "Darkblue",
                                  size = 40,
                                  family = "courier"))



