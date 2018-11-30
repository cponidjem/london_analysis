##*****************************************
##
##international visitors London
##
##*****************************************

library(ggplot2)
library(plyr)
library(dplyr)
library(gdata)
library(readxl)

visits <- read_excel("international-visitors-london2.xlsx", sheet=5)
View(visits)

p1 <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
p2 <- c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009")
p3 <- c("2015", "2016", "2017")
p4 <- c("2012", "2013", "2014")
p5 <- c("2013", "2014","2015", "2016", "2017")

visitsp5 <- subset(visits, year %in% p5)
length(visits)


##Volume de donnée selon les années
dataPerYear <- ddply(visitsp5, .(year), summarise, sum=sum(sample))
ggplot(dataPerYear, aes(x=year, y=sum))+geom_bar(stat="identity", fill="steelblue") + geom_text(aes(label=sum), vjust=1.6, color="white", size=3.5)


##*************************************
#
#  Profil des visiteurs
#
##*************************************

perCountry <- ddply(visitsp5, .(market), summarise, sum=sum(sample))
topCountry <- head(arrange(perCountry, -sum),4)
#topNameF <- head(arrange(nameF, -sum),5)
ggplot(topCountry, aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")
#ou sans passer par topCountry (plus compacte)
ggplot(head(arrange(perCountry, -sum), 6), aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")


#nombre de visiteurs par motifs de venue
perPurpose <- ddply(visitsp5, .(market, purpose), summarise, sum2=sum(sample))
ggplot(perPurpose, aes(x=purpose, y=sum2, fill=purpose))+geom_bar(stat="identity",position="dodge")

#nombre de visiteurs par motifs de venue et par ans
purposePerYear <-ddply(visitsp5, .(year, purpose), summarise, sum=sum(visits))
ggplot(purposePerYear, aes(x=year, y=sum, colour=purpose))+geom_line() + geom_point()

#motifs de venue pour les visiteurs US
USpurpose <- ddply(subset(visits, market=="USA"), .(market, purpose, sample), summarise, sum=sum(sample))
ggplot(USpurpose, aes(x=purpose, y=sum, fill=purpose))+geom_point(stat="identity",position="dodge")



#nombre de visiteur par motifs pour les pays dans le topCountry
testCountry <- ddply(subset(visitsp5, market %in% topCountry$market), .(market, purpose), summarise, sum=sum(visits))
ggplot(testCountry, aes(x=market, y=sum, fill=purpose))+geom_bar(stat="identity")+geom_text(aes(y=sum, label=sum), color="white", size=3.5)

#on remarque que les 3 principaux pays venant pour le business sont les USA, l'Allemagne et la france
#peut être peux-t-on changer la visualisation des données


##********************************************
#
#  Evolution des visites dans le temps
#
##********************************************

#progression du nombre de visite en fonction du temps
perYear <- ddply(visitsp5, .(year), summarise, sum=sum(sample))
ggplot(perYear, aes(x=year, y=sum))+ geom_line(size=2, inherit.aes = FALSE)+ geom_point(size=2.5)

#en gardant tous les point
perYearMarket <- ddply(visits, .(year, market), summarise, sum=sum(sample))
ggplot(perYearMarket, aes(x=year, y=sum)) + geom_point(size=2.5) 


#évolution de la part des gens venu pour la travail de 2010 à 2017
data1 <- subset(visits, year %in% p1)
businessData <- ddply(subset(data1, purpose == "Business"), .(year), summarise, sum=sum(sample))
ggplot(businessData, aes(x=year, y=sum)) + geom_point(size=4) + geom_line(size=2)
ggplot(businessData, aes(x=year, y=sum)) +geom_bar(stat="identity")
#il y a vraiment une différence entre les point et les barres pour la visualisation
#baisse du au brexit ? il faudrait que regarder le pourcentage, voir si c'est significatif

#évolution pour les vacances
holidayData <- ddply(subset(data1, purpose == "Holiday"), .(year), summarise, sum=sum(sample))
ggplot(holidayData, aes(x=year, y=sum, fill=year)) +geom_bar(stat="identity")
#ça semple rester stable

#spend evolution
spendEvol <- ddply(visits, .(year, dur_stay, spend, sample))
ggplot(spendEvol, aes(x= spend, y=dur_stay)) + geom_bar(stat="identity")

##********************************************
##
##Evolution des moyens de transports dans le temps
##
##********************************************

Last3Year <- ddply(subset(visits, year %in% p3), .(year, mode), summarise, sum=sum(sample))
ggplot(Last3Year, aes(x=year, y=sum, fill=mode))+geom_bar(stat="identity", position="dodge")
p4Mode <- ddply(subset(visits, year %in% p4), .(year, mode), summarise, sum=sum(sample))
ggplot(p4Mode, aes(x=year, y=sum, fill=mode))+geom_bar(stat="identity", position="dodge") 
#toujours majoritairement par avion


##***************************************************************************************
##
## Chercher une correlation entre le nombre de crimes et l'afluence de touristes a Londres 
##
##**************************************************************************************

# Afluence de touristes par quart d'annee de 2008 ? 2016
perQuarter <- ddply(visits, .(year,quarter),summarise, sum=sum(sample))
perQuarter2008_2016 <- subset(perQuarter,(year>=2008&year<=2016))
p <- ggplot(perQuarter2008_2016, aes(x=factor(year), y=sum, fill=quarter))+geom_bar(stat="identity",position="dodge")
p+labs(title="Afluence de touristes par quart d'ann?e \n de 2008 ? 2016",x ="Ann?e", y = "Nombre de touristes")

# Recuperer le dataset sur les crimes a Londres
crimes <- read.table(file="london_crime_by_lsoa.csv",header=TRUE,sep=",")
# Traiter les donnees sur le crime pour qu'elles soient comparables avec les donn?es sur l'affluence : ajout de la colonne quarter
getQuarter <- function(x) {
  case_when(
    x >= 1 & x <= 3 ~ "Q1",
    x >= 4 & x <= 6 ~ "Q2",
    x >= 7 & x <= 9 ~ "Q3",
    x >= 10 & x <= 12 ~ "Q4"
  )
}
crimes <- cbind(crimes,quarter=getQuarter(crimes$month))

# Decommenter la ligne suivante pour voir les grandes categories de crime
#levels(factor(crimes$major_category))

# Nombres de vols par quart d'annee de 2008 a 2016
theft <- subset(crimes,(major_category=="Burglary"|major_category=="Robbery"|major_category=="Theft and Handling"),select=c(major_category,value,year,quarter))
theftPerQuarter <- ddply(theft, .(year,quarter), summarise, sum=sum(value))
p <- ggplot(theftPerQuarter, aes(x=factor(year), y=sum, fill=quarter))+geom_bar(stat="identity",position="dodge")
p+labs(title="Nombres de vols par quart d'ann?e \n de 2008 ? 2016",x ="Ann?e", y = "Nombre de vols")

# Nombres de violences contre la personne de 2008 a 2016
violenceAgainstPerson <- subset(crimes,(major_category=="Violence Against the Person"|major_category=="Sexual Offences"),select=c(major_category,value,year,quarter))
violenceAgainstPersonPerQuarter <- ddply(violenceAgainstPerson, .(year,quarter), summarise, sum=sum(value))
p <- ggplot(violenceAgainstPersonPerQuarter, aes(x=factor(year), y=sum, fill=quarter))+geom_bar(stat="identity",position="dodge")
p+labs(title="Nombres de cas violences contre la personne \n par quart d'ann?e  de 2008 ? 2016",x ="Ann?e", y = "Nombre de cas")

