websearch = read.csv("materials\\websearch2.csv")
websearch$Subject = factor(websearch$Subject)
websearch$Engine = factor(websearch$Engine)
websearch$Order = factor(websearch$Order)
summary(websearch)

library(plyr)
ddply(websearch, ~ Engine, function(data) summary(data$Searches))
ddply(websearch, ~ Engine, summarise, Searches.mean=mean(Searches), Searches.sd=sd(Searches))


#hist(websearch[websearch$Engine == "Bing",]$Searches)
#hist(websearch[websearch$Engine == "Google",]$Searches)
#plot(Searches ~ Engine, data=websearch) # boxplot


library(reshape2)
websearch.wide.order = dcast(websearch, Subject ~ Order, value.var="Searches")
t.test(websearch.wide.order$"1", websearch.wide.order$"2", paired=TRUE, var.equal=TRUE)


websearch.wide.tech = dcast(websearch, Subject ~ Engine, value.var="Searches")
t.test(websearch.wide.tech$Bing, websearch.wide.tech$Google, paired=TRUE, var.equal=TRUE)
#plot(Searches ~ Engine, data=websearch)


library(coin)
wilcoxsign_test(Effort ~ Engine | Subject, data=websearch, distribution="exact")

#-------------------------------------------------#

websearch3 = read.csv("materials\\websearch3.csv")
websearch3$Subject = factor(websearch3$Subject)
websearch3$Engine = factor(websearch3$Engine)
websearch3$Order = factor(websearch3$Order)
summary(websearch3)

library(plyr)
ddply(websearch3, ~ Engine, function(data) summary(data$Searches))
ddply(websearch3, ~ Engine, summarise, Searches.mean=mean(Searches), Searches.sd=sd(Searches))


library(ez)
m = ezANOVA(dv=Searches, within=Order, wid=Subject, data=websearch3)
m$Mauchly
m$ANOVA

library(ez)
m = ezANOVA(dv=Searches, within=Engine, wid=Subject, data=websearch3)
m$Mauchly
m$ANOVA


library(reshape2)
websearch3.wide.tech = dcast(websearch3, Subject ~ Engine, value.var="Searches")
gg.bi = t.test(websearch3.wide.tech$Google, websearch3.wide.tech$Bing, paired=TRUE)
bi.ya = t.test(websearch3.wide.tech$Bing, websearch3.wide.tech$Yahoo, paired=TRUE)
ya.gg = t.test(websearch3.wide.tech$Yahoo, websearch3.wide.tech$Google, paired=TRUE)
p.adjust(c(gg.bi$p.value, bi.ya$p.value, ya.gg$p.value), method="holm")

library(coin)
friedman_test(Effort ~ Engine | Subject, data=websearch3, distribution="asymptotic")

se.sc = wilcox.test(websearch3[websearch3$Engine == "Google",]$Effort, websearch3[websearch3$Engine == "Bing",]$Effort, paired=TRUE, exact=FALSE)
se.vc = wilcox.test(websearch3[websearch3$Engine == "Bing",]$Effort, websearch3[websearch3$Engine == "Yahoo",]$Effort, paired=TRUE, exact=FALSE)
sc.vc = wilcox.test(websearch3[websearch3$Engine == "Google",]$Effort, websearch3[websearch3$Engine == "Yahoo",]$Effort, paired=TRUE, exact=FALSE)
p.adjust(c(se.sc$p.value, se.vc$p.value, sc.vc$p.value), method="holm")