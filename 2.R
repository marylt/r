
# 4a) 16 columns, 26217 rows

# 4b) NA = 153 , 5 = 133
table(nassCDS$injSeverity)
# 4b) 3 = 8495 , 4 = 1118 , % = 36.667%
8495+1118
9613/26217 * 100

# 4c)
table(nassCDS$dead, nassCDS$seatbelt)
prop.table(table(nassCDS$dead, nassCDS$seatbelt))

# 4d)
summary(nassCDS$yearVeh)
sd(nassCDS$yearVeh, na.rm = TRUE)

# 4e)
hist(nassCDS$yearVeh,
     main="Histogram for Car Year",
     xlab="Car Year",
     border="blue",
     col="green")

# 5a)
table(gss$HRS1)
summary(gss$HRS1)
levels(gss$HRS1)
typeof(gss$HRS1)
class(gss$HRS1)

# 5b)
hours1 <- car::recode(gss$HRS1,"'89+ hrs'=89")
hours1 <-factor(hours1)
levels(hours1)
hours1 <- as.numeric(as.character(hours1))
table(gss$HRS1)
table(hours1)

# 5c)
summary(hours1)
class(gss$HRS1)
nlevels(gss$HRS1)
levels(gss$HRS1)
work.lev <- c("Part-time","Full time","More than full-time")
gss$work3 <-factor(
  ifelse(hours1 <= 39, 1,
         ifelse(hours1 > 39 & hours1 <= 40, 2,
                ifelse(hours1 > 40, 3,
                       NA))), labels=work.lev, ordered=T)
table(gss$work3)
