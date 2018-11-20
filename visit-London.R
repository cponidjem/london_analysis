##*****************************************
##
##international visitors London
##
##*****************************************


library(ggplot2)
library(plyr)
library(gdata)
library(readxl)

visits <- read_excel("international-visitors-london2.xlsx", sheet=5)
View(visits)

perCountry <- ddply(visits, .(market), summarise, sum=sum(sample))
topCountry <- head(arrange(perCountry, -sum),10)
topNameF <- head(arrange(nameF, -sum),5)
ggplot(topCountry, aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")
