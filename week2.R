prefs = read.csv("materials\\deviceprefs.csv")
#View(prefs)

prefs$Subject = factor(prefs$Subject)
prefs$Disability = factor(prefs$Disability)

summary(prefs)
#plot(prefs$Pref)

#plot(prefs[prefs$Disability == "0",]$Pref)
#plot(prefs[prefs$Disability == "1",]$Pref)

prfs = xtabs( ~ Pref, data=prefs)
prfs
chisq.test(prfs)

binom.test(sum(prefs[prefs$Disability == "0",]$Pref == "touchpad"), nrow(prefs[prefs$Disability == "0",]), p=1/2)
binom.test(sum(prefs[prefs$Disability == "1",]$Pref == "touchpad"), nrow(prefs[prefs$Disability == "1",]), p=1/2)

prfs2 = xtabs( ~ Pref + Disability, data=prefs)
#View(prfs2)
chisq.test(prfs2)

library(RVAideMemoire)
G.test(prfs2)

fisher.test(prfs2)