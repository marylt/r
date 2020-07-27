
# Data & Packages setup

library(summarytools)  # for summarytools
library(car)  # for recode & scatterplot
library(PerformanceAnalytics) # for chart.Correlation
library(pwr)  # for power: pwr.f2.test
library(stargazer) # for regression table outputs
library(PerformanceAnalytics) # alternate correlattion matrix
library(corrplot) # for scatter plot
library(Hmisc) # for scatter plot
library(ggplot2) # for customizable scatterplot

######################

# Research Idea: How much do video game ranks affect video game sales in Japan?

videogames <- read.csv(file.choose(), header=T)  # import vgsales.csv

######################

# INDEPENDENT VARIABLE: Video Game Rank (Rank)

# DEPENDENT VARIABLE: Video Game Sales in Japan (JP_Sales)

# Examining data:

str(videogames)
str(videogames[c("JP_Sales", "Rank")])

# summary stats

summary(videogames[c("JP_Sales", "Rank")])
summarytools::descr(videogames[c("JP_Sales", "Rank")])
summarytools::dfSummary(videogames[c("JP_Sales", "Rank")])

######################

# Assumptions / Diagnostic

# Diagnostic: Videogame Rankings

ranking <- lm(videogames$JP_Sales ~ videogames$Rank)
plot(ranking)

# Assumptions: Plot with constant

plot(videogames$JP_Sales, videogames$Rank, pch=11, col="darkgreen", lwd=2, 
     xlab="Rank", ylab="JP Sales in Millions", cex=1.2, cex.axis=1.5, cex.lab=1.5, cex.main=2)

# Fitting line

abline(lm(videogames$JP_Sales ~ videogames$Rank), col="red", lwd=4)

# Fitting line WITHOUT constant (Y-Intercept)

abline(lm(videogames$JP_Sales ~ 0 + videogames$Rank), col="blue", lwd=4)

######################

# Relationship between Game Rank and JP_Sales

# Checking correlation

cor.test(videogames$JP_Sales, videogames$Rank)

# Creating correlation matrix

cormatrix <- cor(videogames[c("JP_Sales", "Rank")])

corrplot(cormatrix,         # correlation matrix
         type="upper",      # upper triangle only
         order="hclust",    # variable reordering method
         tl.col = "black",  # text label color
         tl.srt = 45,       # test string label rotation
         sig.level = 0.05,  # include p-values sig test
         insig = "blank")   # leave blank if not stat.sig.

# Alternate correlattion matrix

chart.Correlation(videogames[c("JP_Sales", "Rank")], histogram=TRUE, pch=11)

######################

# Regression

my_model <- (lm(videogames$Rank ~ videogames$JP_Sales))
summary(my_model)

getwd()
stargazer(my_model, dep.var.caption = "JP Sales", type = "html", out = "project2.html" ,title = "Project 2 Model", align = TRUE)

######################

# Scatterplot

plot(videogames$Rank, videogames$JP_Sales, main="Video Game Rank Sales in Japan",
     xlab="Game Rank ", ylab="Japan Sales in Millions ", pch=11)
abline(lm(videogames$JP_Sales~videogames$Rank), col="red") # regression line (y~x)

# OR using ggplot

sg <- ggplot(videogames, aes(x=videogames$Rank,
       y=videogames$JP_Sales)) + geom_point(size= 2, shape= 1) + geom_smooth(method= lm, se= FALSE)

sg + ggtitle("Video Game Rank Sales in Japan") +
  xlab("Rank") + ylab("JP Sales in Millions")

######################

### End of code For Project 2 ###
