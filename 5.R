
library(Rmisc)

# Q1: - no code





## Q2 (has three parts for making kickscale means for the three groups):

    # PART ONE
    # making kickscale variable for ALL FIFA PLAYERS:

fifaplayers <- c(Fifa2017_FullData_Kaggle_$Name)
fifaplayers

shotpower <- c(Fifa2017_FullData_Kaggle_$Shot_Power)
finishing <- c(Fifa2017_FullData_Kaggle_$Finishing)
longshots <- c(Fifa2017_FullData_Kaggle_$Long_Shots)
curve <- c(Fifa2017_FullData_Kaggle_$Curve)
freekick <- c(Fifa2017_FullData_Kaggle_$Freekick_Accuracy)
shortpass <- c(Fifa2017_FullData_Kaggle_$Short_Pass)
longpass <- c(Fifa2017_FullData_Kaggle_$Long_Pass)

    # build a mean scale as a new variable within kickscale:

kickscaleplayers <- data.frame(fifaplayers, shotpower, finishing, longshots, 
                      curve, freekick, shortpass, longpass)
kickscaleplayers

kickscaleplayers$kickscale.mean <- ((kickscaleplayers$shotpower + kickscaleplayers$finishing + 
                                         kickscaleplayers$longshots + kickscaleplayers$curve + 
                                         kickscaleplayers$freekick + kickscaleplayers$shortpass + 
                                         kickscaleplayers$longpass) / 7)
    
    # summary statistics and sd of kickscaleplayers.mean:

summary(kickscaleplayers$kickscale.mean)
sd(kickscaleplayers$kickscale.mean)
CI(kickscaleplayers$kickscale.mean, ci=.95)

    ######################
    # PART TWO
    # making kickscale variable for FC BARCELONA PLAYERS:

fcbarcelona <- c(Fifa2017_FullData_Kaggle_$Name[Fifa2017_FullData_Kaggle_$Club=='FC Barcelona'])
fcbarcelona
    
shotpower2 <- c(Fifa2017_FullData_Kaggle_$Shot_Power[Fifa2017_FullData_Kaggle_$Club=='FC Barcelona'])
finishing2 <- c(Fifa2017_FullData_Kaggle_$Finishing[Fifa2017_FullData_Kaggle_$Club=='FC Barcelona'])
longshots2 <- c(Fifa2017_FullData_Kaggle_$Long_Shots[Fifa2017_FullData_Kaggle_$Club=='FC Barcelona'])
curve2 <- c(Fifa2017_FullData_Kaggle_$Curve[Fifa2017_FullData_Kaggle_$Club=='FC Barcelona'])
freekick2 <- c(Fifa2017_FullData_Kaggle_$Freekick_Accuracy[Fifa2017_FullData_Kaggle_$Club=='FC Barcelona'])
shortpass2 <- c(Fifa2017_FullData_Kaggle_$Short_Pass[Fifa2017_FullData_Kaggle_$Club=='FC Barcelona'])
longpass2 <- c(Fifa2017_FullData_Kaggle_$Long_Pass[Fifa2017_FullData_Kaggle_$Club=='FC Barcelona'])

    # build a mean scale as a new variable within kickscale:

kickscalefcbarcelona <- data.frame(fcbarcelona, shotpower2, finishing2, longshots2, 
                                   curve2, freekick2, shortpass2, longpass2)
kickscalefcbarcelona

kickscalefcbarcelona$kickscale.mean <- ((kickscalefcbarcelona$shotpower2 + kickscalefcbarcelona$finishing2 +
                                             kickscalefcbarcelona$longshots2 + kickscalefcbarcelona$curve2 +
                                             kickscalefcbarcelona$freekick2 + kickscalefcbarcelona$shortpass2 +
                                             kickscalefcbarcelona$longpass2) / 7)
    # summary statistics and sd of kickscalegcbarcelona.mean:

summary(kickscalefcbarcelona$kickscale.mean)
sd(kickscalefcbarcelona$kickscale.mean)
CI(kickscalefcbarcelona$kickscale.mean, ci=.95)

    ######################
    # PART THREE
    # making kickscale variable for LONGFORD TOWN PLAYERS:

longfordtown <- c(Fifa2017_FullData_Kaggle_$Name[Fifa2017_FullData_Kaggle_$Club=='Longford Town'])
longfordtown

shotpower3 <- c(Fifa2017_FullData_Kaggle_$Shot_Power[Fifa2017_FullData_Kaggle_$Club=='Longford Town'])
finishing3 <- c(Fifa2017_FullData_Kaggle_$Finishing[Fifa2017_FullData_Kaggle_$Club=='Longford Town'])
longshots3 <- c(Fifa2017_FullData_Kaggle_$Long_Shots[Fifa2017_FullData_Kaggle_$Club=='Longford Town'])
curve3 <- c(Fifa2017_FullData_Kaggle_$Curve[Fifa2017_FullData_Kaggle_$Club=='Longford Town'])
freekick3 <- c(Fifa2017_FullData_Kaggle_$Freekick_Accuracy[Fifa2017_FullData_Kaggle_$Club=='Longford Town'])
shortpass3 <- c(Fifa2017_FullData_Kaggle_$Short_Pass[Fifa2017_FullData_Kaggle_$Club=='Longford Town'])
longpass3 <- c(Fifa2017_FullData_Kaggle_$Long_Pass[Fifa2017_FullData_Kaggle_$Club=='Longford Town'])

    # build a mean scale as a new variable within kickscale:

kickscalelongfordtown <- data.frame(longfordtown, shotpower3, finishing3, longshots3, 
                                   curve3, freekick3, shortpass3, longpass3)
kickscalelongfordtown

kickscalelongfordtown$kickscale.mean <- ((kickscalelongfordtown$shotpower3 + kickscalelongfordtown$finishing3 +
                                              kickscalelongfordtown$longshots3 + kickscalelongfordtown$curve3 +
                                              kickscalelongfordtown$freekick3 + kickscalelongfordtown$shortpass3 +
                                              kickscalelongfordtown$longpass3) / 7)

    # summary statistics and sd of kickscalelongfordtown.mean:

summary(kickscalelongfordtown$kickscale.mean)
sd(kickscalelongfordtown$kickscale.mean)
CI(kickscalelongfordtown$kickscale.mean, ci=.95)





## Q3:
    
    # 3b:

attach(gss)

    # Do you agree or disagree that...

    # 1) In uncertain times, I usually expect the best.
LOTR1.r <- as.numeric(LOTR1)  # convert
table(LOTR1, LOTR1.r)         # confirm recode


    # 2) If something can go wrong for me, it will.  (need to reverse code)
LOTR2.r <- as.numeric(LOTR2)  # convert
LOTR2.r <- 6 - LOTR2.r        # reverse code
table(LOTR2, LOTR2.r)         # confirm recode


    # 3) I'm always optimistic about my future.
LOTR3.r <- as.numeric(LOTR3)  # convert
table(LOTR3, LOTR3.r)         # confirm recode


    # 4)I hardly ever expect things to go my way.  (need to reverse code)
LOTR4.r <- as.numeric(LOTR4)  # convert
LOTR4.r <- 6 - LOTR4.r        # reverse code
table(LOTR4, LOTR4.r)         # confirm recode


    # 5) I rarely count on good things happening to me. (need to reverse code)
LOTR5.r <- as.numeric(LOTR5)  # convert
LOTR5.r <- 6 - LOTR5.r        # reverse code
table(LOTR5, LOTR5.r)         # confirm recode


    # 6)  Overall, I expect more good things to happen to me than bad.
LOTR6.r <- as.numeric(LOTR6)  # convert
table(LOTR6, LOTR6.r)         # confirm recode


    # LOTR scale variable

LOTR.scale <- (LOTR1.r + LOTR2.r + LOTR3.r + LOTR4.r + LOTR5.r + LOTR6.r) - 5
LOTR.scale

    # summary statistics and sd:

summary(na.omit(LOTR.scale))
length(na.omit(LOTR.scale))
min(na.omit(LOTR.scale))
max(na.omit(LOTR.scale))
quantile(na.omit(LOTR.scale))
median(na.omit(LOTR.scale))
mean(na.omit(LOTR.scale))
sd(na.omit(LOTR.scale))
CI(na.omit(LOTR.scale), ci=.95)

    # 3c histogram

hist(LOTR.scale,
main="LOTR.scale Results",
xlab="Results",
ylab="Frequency",
xlim=c(0,30),
ylim=c(0,350),
col="darkmagenta",
)
    # 3d confidence interval

CI(na.omit(LOTR.scale), ci=.99)





## End of HOMEWORK 5 file
