# Mary Ton
# INST314
# Homework 4
# Last edited on November 3, 2019

# libraries to be imported:
library(summarytools)
library(lsr)
library(pwr)
library(effsize)

#############

# Question 1:

  # defining variables:
regulargas <- c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28)
premiumgas <- c(19, 22 ,24, 24, 25 ,25, 26, 26, 28, 32)
  
  # inserting into t.test:
t.test(regulargas,premiumgas, pooled = T, paired = F)
  # t = -1.2462, df = 17.892, p-value = 0.2288
  # alternative hypothesis: true difference in means is not equal to 0
  # 95 percent confidence interval: (-5.373131 , 1.373131)
  # sample estimates: mean of x = 23.1 , mean of y = 25.1

#############

# Question 2:

  #defining variables:
dcww <- c(comicmovies$Worldwide[comicmovies$Studio=='DC'])
marvelww <- c(comicmovies$Worldwide[comicmovies$Studio=='Marvel'])
dcmarvelww <- c(dcww,marvelww)

  # summaries and sd for variables:
summary(dcww)
sd(dcww)
length(dcww)

summary(marvelww)
sd(marvelww)
length(marvelww)

summary(dcmarvelww)
sd(dcmarvelww)
length(dcmarvelww)

  # inserting dcww and marvelww into t.test:
t.test(dcww, marvelww, pooled = T, paired = F)
  # t = -0.91202, df = 48.142, p-value = 0.3663
  # alternative hypothesis: true difference in means is not equal to 0
  # 95 percent confidence interval: (-288.4993  108.4358)
  # sample estimates: mean of x = 526.8640 ,  mean of y = 616.8957

  # cohen's d effect size:
cohensD(dcww,marvelww)
  # effect size = 0.227309
cohen.d(dcww,marvelww, na.rm = T, pooled = T, paired = F)

#############

# Question 3:

  # defining variables:
marvel <- c(comicmovies$Year[comicmovies$Studio=='Marvel'])
prepostdisney <- c("Pre-Disney", "Post-Disney")
marvelprepostdisney <- factor(ifelse
                              (marvel <= 2009, 1,
                                ifelse(marvel > 2009 & marvel <= 2018, 2, NA)), labels=prepostdisney, ordered=T)
reviews <- c(comicmovies$Review[comicmovies$Studio=='Marvel'])

marvelpredisney <- c(30,32,39,39,40,42,43,48,49,49,58,60,61,64,64,68,76,82,83,88,89)
marvelpostdisney <- c(26,36,60,64,68,69,71,72,75,75,77,77,77,80,80,81,81,82,83,84,85,86,86,86,88,88)
marvelpreandpostdisney <- c(marvelpredisney, marvelpostdisney)

  # re-organizing into table:
table(reviews,marvelprepostdisney)

  # summaries and sd for pre and post disney marvel:
summary(marvelpredisney)
sd(marvelpredisney)
length(marvelpredisney)

summary(marvelpostdisney)
sd(marvelpostdisney)
length(marvelpostdisney)

summary(marvelpreandpostdisney)
sd(marvelpreandpostdisney)
length(marvelpreandpostdisney)

  # inserting marvelpredisney and marvelpostdisney into t.test:
t.test(marvelpostdisney, marvelpredisney, pooled = T, paired = F)
  # t = 3.4556, df = 38.034, p-value = 0.001365
  # alternative hypothesis: true difference in means is not equal to 0
  # 95 percent confidence interval: (7.110354 27.222979)
  # sample estimates: mean of x = 74.50 , mean of y = 57.33

#############

# Question 4:
  
  # two-sample t-test power:
  # if n in each group is 126:
pwr.t.test(power=NULL, n=126, sig.level=.05, type="two.sample", alt="two.sided", d=.05)
  # if n in each group is 63:
pwr.t.test(power=NULL, n=63, sig.level=.05, type="two.sample", alt="two.sided", d=.05)

#############

# No code for questions 5 and 6

#############

# End of R file for Homework 6