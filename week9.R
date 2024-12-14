web = read.csv("materials\\websearch3.csv")
View(web)

web$Subject = factor(web$Subject)
web$Engine = factor(web$Engine)
web$Order = factor(web$Order)
summary(web)

boxplot(Searches ~ Engine, data=web, xlab="Engine", ylab="Searches")

library(plyr)
ddply(web, ~ Engine, function(data) summary(data$Searches))
ddply(web, ~ Engine, summarise, Searches.mean=mean(Searches), Searches.sd=sd(Searches))


library(lme4)
library(lmerTest)
library(car)


contrasts(web$Engine) <- "contr.sum"


m = lmer(Searches ~ Engine  + (1|Subject), data=web)
Anova(m, type=3, test.statistic="F")

library(multcomp)
summary(glht(m, mcp(Engine="Tukey")), test=adjusted(type="holm"))

#-------------------------------------------------------#

social = read.csv("materials\\socialvalue.csv")
View(social)

social$Subject = factor(social$Subject)
social$Clip = factor(social$Clip)
social$Social = factor(social$Social)
social$ClipOrder = factor(social$ClipOrder)
social$SocialOrder = factor(social$SocialOrder)
summary(social)

library(plyr)
ddply(social, ~ Clip * Social, function(data) summary(data$Valued))
ddply(social, ~ Clip * Social, summarise, Valued.mean=mean(Valued), Valued.sd=sd(Valued))


library(lme4)
library(lmerTest)
library(car)

contrasts(social$Clip) <- "contr.sum"
contrasts(social$Social) <- "contr.sum"

m = lmer(Valued ~ Clip * Social  + (1|Subject), data=social)
Anova(m, type=3, test.statistic="F")


library(multcomp)
library(emmeans)
summary(glht(m, emm(pairwise ~  Clip*Social)), test=adjusted(type="none"))
p.adjust(c(0.000225,0.594397), method="holm")

#-------------------------------------------------------#

teaser = read.csv("materials\\teaser.csv")
View(teaser)

teaser$Subject = factor(teaser$Subject)
teaser$Order = factor(teaser$Order)
teaser$Teaser = factor(teaser$Teaser)
teaser$Liked = factor(teaser$Liked)
summary(teaser)

library(plyr)
ddply(teaser, ~ Teaser, function(data) summary(data$Liked))

library(lme4)
library(car)
contrasts(teaser$Order) <- "contr.sum"
m = glmer(Liked ~ Order + (1|Subject), data=teaser, family=binomial, nAGQ=1)
Anova(m, type=3)

contrasts(teaser$Teaser) <- "contr.sum"
m = glmer(Liked ~ Teaser  + (1|Subject), data=teaser, family=binomial, nAGQ=1)
Anova(m, type=3)

library(multcomp)
summary(glht(m, mcp(Teaser="Tukey")), test=adjusted(type="holm"))

#-------------------------------------------------------#

vocab = read.csv("materials\\vocab.csv")
View(vocab)

vocab$Subject = factor(vocab$Subject)
vocab$Heavy = factor(vocab$Heavy)
vocab$Order = factor(vocab$Order)
summary(vocab)

with(vocab, interaction.plot(Social, Heavy, Vocab, ylim=c(0, max(vocab$Vocab))))

library(MASS)
fit = fitdistr(vocab[vocab$Social == "Facebook",]$Vocab, "exponential")$estimate
ks.test(vocab[vocab$Social == "Facebook",]$Vocab, "pexp", rate=fit[1], exact=TRUE)
fit = fitdistr(vocab[vocab$Social == "Gplus",]$Vocab, "exponential")$estimate
ks.test(vocab[vocab$Social == "Gplus",]$Vocab, "pexp", rate=fit[1], exact=TRUE)
fit = fitdistr(vocab[vocab$Social == "Twitter",]$Vocab, "exponential")$estimate
ks.test(vocab[vocab$Social == "Twitter",]$Vocab, "pexp", rate=fit[1],exact=TRUE)


library(car)
contrasts(vocab$Heavy) <- "contr.sum"
contrasts(vocab$Order) <- "contr.sum"
m = glmer(Vocab ~ Heavy * Order + (1|Subject), data=vocab, family=Gamma(link="log"), nAGQ=1)
Anova(m, type=3)


m = glmer(Vocab ~ Heavy * Social + (1|Subject), data=vocab, family=Gamma(link="log"))
Anova(m, type=3)

summary(glht(m, mcp(Social="Tukey")), test=adjusted(type="holm"))
summary(glht(m, lsm(pairwise ~ Heavy * Social)), test=adjusted(type="holm"))

#-------------------------------------------------------#

web = read.csv("materials\\websearch3.csv")
View(web)

web$Subject = factor(web$Subject)
web$Effort = ordered(web$Effort)
summary(web)

library(ordinal)
library(RVAideMemoire)
web2 = as.data.frame(web)
m = clmm(Effort ~ Engine + (1|Subject), data=web2)
Anova.clmm(m, type=3)

#plot(as.numeric(Effort) ~ Engine, data=web2)
library(lme4)
library(multcomp)
m = lmer(as.numeric(Effort) ~ Engine + (1|Subject), data=web2)
summary(glht(m, mcp(Engine="Tukey")), test=adjusted(type="holm"))