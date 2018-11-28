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


##************************************
##
##Volume de donnée selon les années
##
##************************************
dataPerYear <- ddply(visits, .(year), summarise, sum=sum(sample))
ggplot(dataPerYear, aes(x=year, y=sum, fill=year))+geom_bar(stat="identity")
#on observe que nous avons moins de donnée en 2018, c'est normal puisque l'étude a été terminée avant la fin de l'année complète
#on peut enlever l'année 2018 pour ne pas fausser les résultats par an
#on pourrait faire une moyenne du nombre de données par an, voir si le nombre est conséquent pour prouver la validité de notre model


##*************************************
#
#  Profil des visiteurs
#
##*************************************

perCountry <- ddply(visits, .(market), summarise, sum=sum(sample))
topCountry <- head(arrange(perCountry, -sum),10)
#topNameF <- head(arrange(nameF, -sum),5)
ggplot(topCountry, aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")

#ou sans passer par topCountry (plus compacte)
ggplot(head(arrange(perCountry, -sum), 15), aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")

#nombre de visiteurs par motifs de venue
perPurpose <- ddply(visits, .(market, purpose), summarise, sum2=sum(sample))
ggplot(perPurpose, aes(x=purpose, y=sum2, fill=purpose))+geom_bar(stat="identity",position="dodge")

#motifs de venue pour les visiteurs US
USpurpose <- ddply(subset(visits, market=="USA"), .(market, purpose, sample), summarise, sum=sum(sample))
ggplot(USpurpose, aes(x=purpose, y=sum, fill=purpose))+geom_bar(stat="identity",position="dodge")

#nombre de visiteur par motifs et par pays, on prendre les 30 plus grandes valeurs 
testCountry <- ddply(visits, .(market, purpose), summarise, sum=sum(sample))
test2<-(head(arrange(testCountry, -sum),10))
ggplot(test2, aes(x=market, y=sum, fill=purpose))+geom_bar(stat="identity",position="dodge")
ggplot(test2, aes(x=market, y=sum, fill=purpose))+geom_bar(stat="identity",position="stack")

#on remarque que les 3 principaux pays venant pour le business sont les USA, l'Allemagne et la france
#peut être peux-t-on changer la visualisation des données


##********************************************
#
#  Evolution des visites dans le temps
#
##********************************************

#progression du nombre de visite en fonction du temps
perYear <- ddply(visits, .(year), summarise, sum=sum(sample))
ggplot(perYear, aes(x=year, y=sum))+ geom_bar(stat="identity")

#en gardant tous les point
perYearMarket <- ddply(visits, .(year, market), summarise, sum=sum(sample))
ggplot(perYearMarket, aes(x=year, y=sum, color=market)) + geom_point(size=2.5)


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

