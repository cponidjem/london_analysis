##*****************************************
##
##international visitors London
##
##*****************************************
install.packages("cowplot")
library(ggplot2)
library(plyr)
library(dplyr)
library(gdata)
library(readxl)


visitsLondon <- read_excel("international-visitors-london2.xlsx", sheet=5)

View(visitsLondon)

p1 <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
p2 <- c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009")
p3 <- c("2015", "2016", "2017")
p4 <- c("2012", "2013", "2014")
p5 <- c("2013", "2014","2015", "2016", "2017")

visitsp5 <- subset(visitsLondon, year %in% p5)
length(visitsp5)


##Volume de visites selon les années de 2013 à 2017
dataPerYear <- ddply(visitsp5, .(year), summarise, sum=sum(visits))
ggplot(dataPerYear, aes(x=year, y=sum))+geom_bar(stat="identity", fill="steelblue")+ggtitle("Volume de visiteurs selon les années")+xlab("Année")+ylab("Nombre de visiteurs")


##*************************************
#
#  Profil des visiteurs
#
##*************************************

#par année
#2017
profil17 <- ddply(subset(visitsp5, year %in%"2017"), .(market), summarise, sum=sum(visits))
g7 <- ggplot(head(arrange(profil17, -sum), 6), aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")

#2016
profil16 <- ddply(subset(visitsp5, year %in%"2016"), .(market), summarise, sum=sum(visits))
g6 <- ggplot(head(arrange(profil16, -sum), 6), aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")

#2015
profil15 <- ddply(subset(visitsp5, year %in%"2015"), .(market), summarise, sum=sum(visits))
g5 <- ggplot(head(arrange(profil15, -sum), 6), aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")

#2014
profil14 <- ddply(subset(visitsp5, year %in%"2014"), .(market), summarise, sum=sum(visits))
g4 <- ggplot(head(arrange(profil14, -sum), 6), aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")

#2013
profil13 <- ddply(subset(visitsp5, year %in%"2013"), .(market), summarise, sum=sum(visits))
g3 <- ggplot(head(arrange(profil13, -sum), 6), aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")


plot_grid(g7, g6,g5, g4,g3,labels=c("2017", "2016","2015", "2014", "2013"), ncol = 3, nrow = 2)


#top des pays d'origine des visiteursles plus nombreux sur 2013-2017
perCountry <- ddply(visitsp5, .(market), summarise, sum=sum(visits))
topCountry <- head(arrange(perCountry, -sum),6)
#ggplot(topCountry, aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")
#ou sans passer par topCountry (plus compacte)
ggplot(head(arrange(perCountry, -sum), 6), aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")+labs(title="Les 6 premiers pays à venir en plus grand nombre à Londres",x="Pays",y="Nombre de visiteurs",fill = "Pays")

#nombre de visiteurs par motifs de venue
perPurpose <- ddply(visitsp5, .(market, purpose), summarise, sum2=sum(visits))
ggplot(perPurpose, aes(x=purpose, y=sum2, fill=purpose))+geom_bar(stat="identity",position="dodge")+labs(title="Volume de visites en fonction du motif",x="Motif",y="Somme",fill = "Motif")


#nombre de visiteur par motifs pour les pays dans le topCountry
testCountry <- ddply(subset(visitsp5, market %in% topCountry$market), .(market, purpose), summarise, sum=sum(visits))
ggplot(testCountry, aes(x=purpose, y=sum, fill=market))+geom_bar(stat="identity", position="dodge")

#nombre de visiteurs par motifs de venue et par ans
purposePerYear <-ddply(visitsp5, .(year, purpose), summarise, sum=sum(visits))
ggplot(purposePerYear, aes(x=year, y=sum, colour=purpose))+geom_line() + geom_point()+labs(title="Volume de visites en fonction du motif et de l'année",x="Années",y="Somme",fill = "Motif")



##********************************************
#
#  Evolution des visites dans le temps
#
##********************************************

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
spendEvol <- ddply(visitsp5, .(year, dur_stay, spend, visits))
ggplot(spendEvol, aes(x= spend, y=dur_stay)) + geom_bar(stat="identity")

##********************************************
##
##Evolution des moyens de transports dans le temps
##
##********************************************

transport <- ddply(visitsp5, .(year, mode), summarise, sum=sum(visits))
ggplot(transport, aes(x=year, y=sum, fill=mode))+geom_bar(stat="identity", position="dodge")+labs(title="Volume de visiteurs selon le mode de transport",x="Année",y="Somme",fill = "Mode")

#toujours majoritairement par avion

#transport utilisé en fonction du pays - pour les 6 principaux pays
transportPerCountry <- ddply(subset(visitsp5, market %in% topCountry$market), .(market, mode), summarise, sum=sum(visits))
ggplot(transportPerCountry, aes(x=market, y=sum, fill=mode))+geom_bar(stat="identity")+labs(title="Volume de visiteurs selon le mode de transport et le pays",x="Année",y="Somme",fill = "Mode")

transportPerQuarter <- ddply(visitsp5, .(quarter, mode), summarise, sum=sum(visits))
ggplot(transportPerQuarter, aes(x=quarter, y=sum, fill=mode))+geom_bar(stat="identity", position="dodge")

visitsPerQuarter <- ddply(visitsp5, .(quarter), summarise, sum=sum(visits))
ggplot(visitsPerQuarter, aes(x=quarter, y=sum))+geom_bar(stat="identity")
#moins de fréquentation en Q1

#en conclusion peu de variation pour le mode de transport que ce soit en fonction des années ou des saisons

#**************************************
#
#Montant des dépenses par jour
#
#**************************************

spend <- ddply(visitsp5, .(dur_stay, spend))


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

