
# data & packages setup

load(file.choose())  # select the Video Game Sales dataset, we named it as vgsales
library(car)  # needed for recode function
library(DescTools)  # for effect size
library(dplyr)
library(summarytools)
library(DescTools)
library(descr)

######################

# IV prep
# Note that 'NA' represents the other 10 genres (we are only focusing on sports and action)

pubs = subset(vgsales, vgsales$Publisher ==  "Nintendo" | vgsales$Publisher == "Electronic Arts" | vgsales$Publisher == "Electronic Arts Victor")

topgenresas = ifelse(pubs$Genre=="Sports","Sports", 
       ifelse(pubs$Genre=="Action","Action", NA))

table(topgenresas)
prop.table(table(topgenresas))
summarytools::freq(topgenresas)

####################

# DV prep
# Note that Electronic Arts Victor is a part of Electronic Arts

toppub = subset(vgsales, vgsales$Publisher ==  "Nintendo" | vgsales$Publisher == "Electronic Arts" | vgsales$Publisher == "Electronic Arts Victor")
toppubb = (toppub$Publisher)

table(toppubb)
prop.table(table(toppubb))
summarytools::freq(toppubb)

# length of both variables need to be the same for the bivariate chart

length(toppubb)
length(topgenresas)

######################

# Bivariate Chart

addmargins(table(toppubb, topgenresas))  # frequency table
round(addmargins(prop.table(table(toppubb, topgenresas),2)),3)

######################

# Chi Square Test & Effect Size, alpha = .05

# test: X-squared = 64.357, df = 2, p-value = 1.06e-14
chisq.test(toppubb, topgenresas)

# n = 880

sum(table(toppubb, topgenresas))

# effect size using Phi = 0.2704306
Phi(toppubb,topgenresas)

### end of code ###
