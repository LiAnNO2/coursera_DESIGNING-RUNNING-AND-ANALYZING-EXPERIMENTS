
designide = read.csv("materials\\designtime.csv")
#View(designide)
designide$Subject = factor(designide$Subject)
summary(designide)

library(plyr)
ddply(designide, ~ Tool, function(data) summary(data$Time))
ddply(designide, ~ Tool, summarise, Time.mean=mean(Time), Time.sd=sd(Time))

#hist(designide[designide$Tool == "Illustrator",]$Time)
#hist(designide[designide$Tool == "InDesign",]$Time)
#plot(Time ~ Tool, data=designide)

shapiro.test(designide[designide$Tool == "Illustrator",]$Time)
shapiro.test(designide[designide$Tool == "InDesign",]$Time)

m_1 = aov(Time ~ Tool, data=designide)
shapiro.test(residuals(m_1))
qqnorm(residuals(m_1)); qqline(residuals(m_1))

library(carData)
library(car)
leveneTest(Time ~ as.factor(Tool), data=designide, center=mean) # Levene
leveneTest(Time ~ as.factor(Tool), data=designide, center=median) # Brown-Forsythe


library(MASS)
fit = fitdistr(designide[designide$Tool == "Illustrator",]$Time, "lognormal")$estimate
ks.test(designide[designide$Tool == "Illustrator",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)
fit = fitdistr(designide[designide$Tool == "InDesign",]$Time, "lognormal")$estimate
ks.test(designide[designide$Tool == "InDesign",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)


designide$logTime = log(designide$Time)
#View(designide)
summary(designide)

library(plyr)
ddply(designide, ~ as.factor(Tool), function(data) summary(data$Time))
ddply(designide, ~ as.factor(Tool), summarise, logTime.mean=mean(logTime), logTime.sd=sd(logTime))

# Welch t-test
t.test(logTime ~ as.factor(Tool), data=designide, var.equal=FALSE)

# Mann-Whitney U test
library(coin)
wilcox_test(Time ~ as.factor(Tool), data=designide, distribution="exact")
wilcox_test(logTime ~ as.factor(Tool), data=designide, distribution="exact")
