pgviews = read.csv("materials\\timeonsite.csv")
#View(pgviews)
pgviews$Subject = factor(pgviews$Subject)
summary(pgviews)

library(plyr)
ddply(pgviews, ~ Site, function(data) summary(data$Time))
ddply(pgviews, ~ Site, summarise, Time.mean=mean(Time), Time.sd=sd(Time))

#hist(pgviews[pgviews$Site == "A",]$Time)
#hist(pgviews[pgviews$Site == "B",]$Time)
#plot(Time ~ Site, data=pgviews)

t.test(Time ~ Site, data=pgviews, var.equal=TRUE)

# Welch t-test
t.test(Time ~ Site, data=pgviews, var.equal=FALSE)
