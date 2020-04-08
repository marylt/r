# Mary Ton
# INST 314-0105
# Project 3
# Last Revised R Date: Sunday, December 8, 2019

######################

# Data & Packages setup

library(summarytools)  # for summarytools
library(car)  # for recode & scatterplot
library(PerformanceAnalytics) # for chart.Correlation
library(pwr)  # for power: pwr.f2.test
library(stargazer) # for regression table outputs
library(corrplot) # for scatter plot
library(Hmisc) # for scatter plot

######################

# Research Idea: Which variable is the best predictor of Japanese game sales?

videogames <- read.csv(file.choose(), header=T)  # import vgsales.csv

######################

# INDEPENDENT VARIABLE: Rank, Platform, Genre, Publisher

  # Examining independent variable data:

str(videogames$Rank)
levels(videogames$Rank)
sd(videogames$Rank, na.rm=T)
summary(videogames$Rank)


str(videogames$Platform)
levels(videogames$Platform) # 31
nlevels(videogames$Platform)
summary(videogames$Platform)
  # create platform duummies
platform.handheld <- recode(as.numeric(videogames$Platform), "1:7=1; 8:19=0; 20:21=1; 22:31=0")
platform.console <- recode(as.numeric(videogames$Platform), "1:7=0; 8:19=1; 20:21=0; 22:31=1")
      # check platform recode
table(videogames$Platform, platform.console)
table(videogames$Platform, platform.handheld)


str(videogames$Genre)
levels(videogames$Genre)# 12
nlevels(videogames$Genre)
summary(videogames$Genre)
  # create genre duummies
genre.action <- recode(as.numeric(videogames$Genre), "1=1; 2:12=0")
genre.sports <- recode(as.numeric(videogames$Genre), "1:10=0; 11=1; 12=0")
genre.rpg <-recode(as.numeric(videogames$Genre), "1:7=0; 8=1; 9:12=0")
genre.adventure <-recode(as.numeric(videogames$Genre), "1=0; 2=1; 3:12=0")
genre.other <- recode(as.numeric(videogames$Genre), "1:2=0; 3:7=1; 8=0; 9:10=1; 11=0; 12=1")
    # check genre recode
table(videogames$Genre, genre.action)
table(videogames$Genre, genre.sports)
table(videogames$Genre, genre.rpg)
table(videogames$Genre, genre.adventure)
table(videogames$Genre, genre.other)


str(videogames$Publisher)
levels(videogames$Publisher) # 579
nlevels(videogames$Publisher)
summary(videogames$Publisher)
  # create publisher dummies
publisher.ea <- recode(as.numeric(videogames$Publisher), "1:138=0; 139=1; 140:579=0")
publisher.activision <- recode(as.numeric(videogames$Publisher), "1:16=0; 17:19=1; 20:579=0")
publisher.namco <- recode(as.numeric(videogames$Publisher), "1:351=0; 352=1; 353:579=0")
publisher.ubisoft <- recode(as.numeric(videogames$Publisher), "1:532=0; 533:534=1; 535:579=0")
publisher.others <- recode(as.numeric(videogames$Publisher), "1:16=1; 17:19=0; 20:138=1; 139=0; 140:351=1; 352=0; 353:532=1; 533:534=0; 535:579=1")
    # check publisher recode
table(videogames$Publisher, publisher.ea)
table(videogames$Publisher, publisher.activision)
table(videogames$Publisher, publisher.namco)
table(videogames$Publisher, publisher.ubisoft)
table(videogames$Publisher, publisher.others)

# DEPENDENT VARIABLE: Video Game Sales in Japan (JP_Sales)

  # Examining dependent variable data:

str(videogames$JP_Sales)
summarytools::freq(videogames$JP_Sales)
levels(videogames$JP_Sales)
sd(videogames$JP_Sales, na.rm=T)

######################

# Summary stats

# regression with specifying reference group approach for rank, platform, genre, publisher
reg.jpsales <- lm(JP_Sales ~ Rank + platform.console + genre.action + genre.adventure + genre.sports + genre.rpg + 
                  publisher.ea + publisher.activision + publisher.namco + publisher.ubisoft, data = videogames)
summary(reg.jpsales)

######################

# Assumptions / Diagnostic

par(mfrow=c(2,2))
plot(reg.jpsales)
par(mfrow=c(1,1))

######################

# Regression Full Model + Stargazer

my_model <- lm(JP_Sales ~ Rank + platform.console + genre.action + genre.adventure + genre.sports + genre.rpg +
                 publisher.ea + publisher.activision + publisher.namco + publisher.ubisoft, data = videogames)
summary(my_model)
getwd()
stargazer(my_model, dep.var.caption = "Japanese Sales", covariate.labels=c("Rank", "Platform: Console",
                                                                           "Genre: Action", "Genre: Adventure","Genre: Sports", "Genre: RPG",
                                                                           "Publisher: EA", "Publisher: Activision", "Publisher: Namco", "Publisher: Ubisoft"
                                                                           ),type = "html", out = "project3.html", title = "Project 3 Model", align = TRUE)


######################

# Effect Size = .146

# (.128)/(1-.128) = .146 = f2 = medium effect

# Power = 1

pwr.f2.test(u = 4, v = 16587, f2 = .146, sig.level = .05, power = NULL)

######################

### End of code For Project 3 ###