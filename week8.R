devicepref = read.csv("materials\\deviceprefsSr.csv")
View(devicepref)

devicepref$Senior = factor(devicepref$Senior)
devicepref$Subject = factor(devicepref$Subject)
devicepref$Disability = factor(devicepref$Disability)

library(nnet)
library(car)

contrasts(devicepref$Senior) <- "contr.sum"
contrasts(devicepref$Disability) <- "contr.sum"

m = glm(as.factor(Pref) ~ Disability*Senior, data=devicepref, family=binomial)
Anova(m, type=3)

contrasts(devicepref$Senior) <- "contr.sum"
contrasts(devicepref$Disability) <- "contr.sum"
m = multinom(Pref ~ Disability*Senior, data=devicepref)
Anova(m, type=3)

trackball.m0 = binom.test(sum(devicepref[devicepref$Senior == "1" & devicepref$Disability == "0",]$Pref == "trackball"), nrow(devicepref[devicepref$Senior == "1" & devicepref$Disability == "0",]), p=1/2)
trackball.m1 = binom.test(sum(devicepref[devicepref$Senior == "1" & devicepref$Disability == "1",]$Pref == "trackball"), nrow(devicepref[devicepref$Senior == "1" & devicepref$Disability == "1",]), p=1/2)
touchpad.f0 = binom.test(sum(devicepref[devicepref$Senior == "0" & devicepref$Disability == "0",]$Pref == "touchpad"), nrow(devicepref[devicepref$Senior == "0" & devicepref$Disability == "0",]), p=1/2)
touchpad.f1 = binom.test(sum(devicepref[devicepref$Senior == "0" & devicepref$Disability == "1",]$Pref == "touchpad"), nrow(devicepref[devicepref$Senior == "0" & devicepref$Disability == "1",]), p=1/2)
p.adjust(c(trackball.m0$p.value, trackball.m1$p.value, touchpad.f0$p.value,touchpad.f1$p.value), method="holm")


#---------------------------------------------------------#


hwreco = read.csv("materials\\hwreco.csv")
View(hwreco)
hwreco$Subject = factor(hwreco$Subject)
hwreco$Recognizer = factor(hwreco$Recognizer)
hwreco$Hand = factor(hwreco$Hand)
summary(hwreco)



with(hwreco, interaction.plot(Recognizer, Hand, Errors, ylim=c(0, max(hwreco$Errors)))) # interaction plot

library(fitdistrplus)
fit = fitdist(hwreco[hwreco$Recognizer == "A",]$Errors, "pois", discrete=TRUE)
gofstat(fit)

fit = fitdist(hwreco[hwreco$Recognizer == "B",]$Errors, "pois", discrete=TRUE)
gofstat(fit)
fit = fitdist(hwreco[hwreco$Recognizer == "C",]$Errors, "pois", discrete=TRUE)
gofstat(fit)

contrasts(hwreco$Recognizer) <- "contr.sum"
contrasts(hwreco$Hand) <- "contr.sum"
m = glm(Errors ~ Recognizer * Hand, data=hwreco, family=poisson)
Anova(m, type=3)

library(multcomp)
library(emmeans)
summary(glht(m, emm(pairwise ~  Recognizer*Hand)), test=adjusted(type="none"))
p.adjust(c(0.001925, 0.095955, 0.243171), method="holm")

with(hwreco, interaction.plot(Recognizer, Hand, Errors, ylim=c(0, max(hwreco$Errors)))) # interaction plot


#---------------------------------------------------------#

flightbooking = read.csv("materials\\bookflights.csv")
View(flightbooking)

flightbooking$Subject = factor(flightbooking$Subject)
flightbooking$International = factor(flightbooking$International)

library(MASS)
flightbooking$Ease = ordered(flightbooking$Ease)
flightbooking$Website = factor(flightbooking$Website)
flightbooking$International = factor(flightbooking$International)
summary(flightbooking)

with(flightbooking, interaction.plot(Website, International, as.numeric(Ease), ylim=c(0, max(flightbooking$Ease))))

flightbooking$Ease = ordered(flightbooking$Ease)
contrasts(flightbooking$Website) <- "contr.sum"
contrasts(flightbooking$International) <- "contr.sum"
m = polr(Ease ~ Website * International, data=flightbooking, Hess=TRUE)
Anova(m, type=3)

library(multcomp)
summary(as.glht(pairs(emmeans(m, ~ Website * International))), test=adjusted(type="none"))