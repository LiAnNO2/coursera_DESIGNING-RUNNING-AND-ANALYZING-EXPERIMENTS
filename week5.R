alphabet = read.csv("materials\\alphabets.csv")
#View(alphabet)
alphabet$Subject = factor(alphabet$Subject)
alphabet$Alphabet = factor(alphabet$Alphabet)
summary(alphabet)

library(plyr)
ddply(alphabet, ~ Alphabet, function(data) summary(data$WPM))
ddply(alphabet, ~ Alphabet, summarise, WPM.mean=mean(WPM), WPM.sd=sd(WPM))

#hist(alphabet[alphabet$Alphabet == "EdgeWrite",]$WPM)
#hist(alphabet[alphabet$Alphabet == "Graffiti",]$WPM)
#hist(alphabet[alphabet$Alphabet == "Unistrokes",]$WPM)
#plot(WPM ~ Alphabet, data=alphabet)

shapiro.test(alphabet[alphabet$Alphabet == "EdgeWrite",]$WPM)
m = aov(WPM ~ Alphabet, data= alphabet)
shapiro.test(residuals(m))
qqnorm(residuals(m)); qqline(residuals(m))

shapiro.test(alphabet[alphabet$Alphabet == "Graffiti",]$WPM)
m = aov(WPM ~ Alphabet, data= alphabet)
shapiro.test(residuals(m))
qqnorm(residuals(m)); qqline(residuals(m))

shapiro.test(alphabet[alphabet$Alphabet == "Unistrokes",]$WPM)
m = aov(WPM ~ Alphabet, data= alphabet)
shapiro.test(residuals(m))
qqnorm(residuals(m)); qqline(residuals(m))

library(car)
leveneTest(WPM ~ Alphabet, data=alphabet, center=median)


m = aov(WPM ~ Alphabet, data=alphabet)
anova(m)

library(multcomp)
summary(glht(m, mcp(Alphabet="Tukey")), test=adjusted(type="holm"))

library(coin)
kruskal_test(WPM ~ as.factor(Alphabet), data=alphabet, distribution="asymptotic")

u.g = wilcox.test(alphabet[alphabet$Alphabet == "Unistrokes",]$WPM, alphabet[alphabet$Alphabet == "Graffiti",]$WPM, exact=FALSE)
u.e = wilcox.test(alphabet[alphabet$Alphabet == "Unistrokes",]$WPM, alphabet[alphabet$Alphabet == "EdgeWrite",]$WPM, exact=FALSE)
e.g = wilcox.test(alphabet[alphabet$Alphabet == "EdgeWrite",]$WPM, alphabet[alphabet$Alphabet == "Graffiti",]$WPM, exact=FALSE)


p.adjust(c(u.g$p.value,u.e$p.value,e.g$p.value), method="holm")