mwa = read.csv("materials\\avatars.csv")
View(mwa)
mwa$Subject = factor(mwa$Subject)
summary(mwa)


library(plyr)
ddply(mwa, ~ Height * Avatar, function(data) summary(data$Positives))
ddply(mwa, ~ Height * Avatar, summarise, Positives.mean=mean(Positives), Positives.sd=sd(Positives))

boxplot(Positives ~ Height * Avatar, data=mwa, xlab="Height.Avatar", ylab="Positives")
with(mwa, interaction.plot(Height, Avatar, Positives, ylim=c(0, max(mwa$Positives))))
with(mwa, interaction.plot(Avatar, Height, Positives, ylim=c(0, max(mwa$Positives))))

library(ez)
m = ezANOVA(dv=Positives, between=c("Height","Avatar"), wid=Subject, data=mwa)
m$Mauchly
m$ANOVA

library(reshape2)

X= t.test(mwa[mwa$Height == "short" & mwa$Avatar == "tall",]$Positives, mwa[mwa$Height == "short" & mwa$Avatar == "short",]$Positives, paired=FALSE, var.equal=TRUE)
Y = t.test(mwa[mwa$Height == "tall" & mwa$Avatar == "tall",]$Positives, mwa[mwa$Height == "tall" & mwa$Avatar == "short",]$Positives, paired=FALSE, var.equal=TRUE)
p.adjust(c(X$p.value, Y$p.value), method="holm")

#------------------------------------------------------------------#

notes = read.csv("materials\\notes.csv")
View(notes)
notes$Subject = factor(notes$Subject)
notes$Order = factor(notes$Order)
summary(notes)

library(plyr)
ddply(notes, ~ Phone * Notes, function(data) summary(data$Words))
ddply(notes, ~ Phone * Notes, summarise, Words.mean=mean(Words), Words.sd=sd(Words))

boxplot(Words ~ Phone * Notes, data=notes, xlab="Phone.Notes", ylab="Words")
with(notes, interaction.plot(Phone, Notes, Words, ylim=c(0, max(notes$Words))))
with(notes, interaction.plot(Notes, Phone, Words, ylim=c(0, max(notes$Words))))


library(ez)
m = ezANOVA(dv=Words, between=Phone, within=Order, wid=Subject, data=notes)
m$Mauchly
m$ANOVA

m = ezANOVA(dv=Words, between=Phone, within=Notes, wid=Subject, data=notes)
m$Mauchly
m$ANOVA


library(reshape2)
notes2.wide = dcast(notes, Subject + Phone ~ Notes, value.var="Words")
View(notes2.wide)
iphone2 = t.test(notes2.wide[notes2.wide$Phone == "iPhone",]$`Add-on`,  notes2.wide[notes2.wide$Phone == "iPhone",]$`Built-in`, paired=TRUE, var.equal=TRUE)
android2 = t.test(notes2.wide[notes2.wide$Phone == "Android",]$`Add-on`,  notes2.wide[notes2.wide$Phone == "Android",]$`Built-in`, paired=TRUE, var.equal=TRUE)
p.adjust(c(iphone2$p.value, android2$p.value), method="holm")


#------------------------------------------------------------------#

socialvalue = read.csv("materials\\socialvalue.csv")
View(socialvalue)
socialvalue$Subject = factor(socialvalue$Subject)
socialvalue$SocialOrder = factor(socialvalue$SocialOrder)
socialvalue$ClipOrder = factor(socialvalue$ClipOrder)
summary(socialvalue)


library(plyr)
ddply(socialvalue, ~ Clip * Social, function(data) summary(data$Valued))
ddply(socialvalue, ~ Clip * Social, summarise, Valued.mean=mean(Valued), Valued.sd=sd(Valued))


boxplot(Valued ~ Clip * Social, data=socialvalue, xlab="Clip.Social", ylab="Valued")
with(socialvalue, interaction.plot(Social, Clip, Valued, ylim=c(0, max(socialvalue$Valued))))
with(socialvalue, interaction.plot(Clip, Social, Valued, ylim=c(0, max(socialvalue$Valued))))

library(ez)
m = ezANOVA(dv=Valued, within=c("ClipOrder","SocialOrder"), wid=Subject, data=socialvalue)
m$Mauchly
m$ANOVA

library(ez)
m = ezANOVA(dv=Valued, within=c("Clip","Social"), wid=Subject, data=socialvalue)
m$Mauchly
m$ANOVA


library(reshape2)
socialvalue.wide = dcast(socialvalue, Subject ~ Social * Clip, value.var="Valued")
View(socialvalue.wide)

fb = t.test(socialvalue.wide$Facebook_negative,socialvalue.wide$Facebook_positive,paired = TRUE,var.equal = TRUE)
twitter = t.test(socialvalue.wide$Twitter_negative,socialvalue.wide$Twitter_positive,,paired = TRUE,var.equal = TRUE)
p.adjust(c(fb$p.value, twitter$p.value), method="holm")

library(ARTool)
socialvalue$Clip = factor(socialvalue$Clip)
socialvalue$Social = factor(socialvalue$Social)
m = art(Valued ~ Clip * Social + (1|Subject), data=socialvalue)
anova(m)

art.con(m, ~ Clip*Social, adjust="holm")