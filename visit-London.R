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

##*************************************
#
#  Profil des visiteurs
#
##*************************************

perCountry <- ddply(visits, .(market), summarise, sum=sum(sample))
topCountry <- head(arrange(perCountry, -sum),10)
#topNameF <- head(arrange(nameF, -sum),5)
ggplot(topCountry, aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")

#nombre de visiteur en fonction du pays d'origine
perCountry <- ddply(visits, .(market), summarise, sum=sum(sample))

#trop de pays différents pour tous les afficher
ggplot(head(arrange(perCountry, -sum), 15), aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")

#autre méthode
#top 10 en nombre de visiteurs 
topCountry <- head(arrange(perCountry, -sum),10)
ggplot(topCountry, aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")

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

#trop de pays différents pour tous les afficher
ggplot(head(arrange(perCountry, -sum), 15), aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")


##********************************************
#
#  Evolution des visites dans le temps
#
##********************************************

#progression du nombre de visite en fonction du temps
perYear <- ddply(visits, .(year), summarise, sum=sum(sample))
ggplot(perYear, aes(x=year, y=sum))+geom_line()



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

