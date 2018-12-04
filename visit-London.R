#*****************************************
#
# London international visitors
# Cindy Ponidjem & Lucie Helcmanocki
#
#*****************************************

# importation des librairies
library(ggplot2)
library(plyr)
library(dplyr)
library(gdata)
library(readxl)
library(cowplot)
library(randomForest)

#importation du fichier excel
visitsLondon <- read_excel("international-visitors-london2.xlsx", sheet=5)
View(visitsLondon)


# définissions des périodes & autres vecteurs
p5    <- c("2013", "2014","2015", "2016", "2017")
p1318 <- c("2013", "2014","2015", "2016", "2017", "2018")

topCountryName <- c("USA", "France", "Germany", "Spain", "Italy", "Netherlands",  
                    "Belgium","Irish Republic", "Sweden", "Australia")



# découpage du dataset
visitsp5 <- subset(visitsLondon, year %in% p5)
visitsp5 <- select(visitsp5, -sample, -area)
glimpse(visitsp5)

#test pour les dépenses par jour par personnes
visitsp6 <- data.frame(subset(visitsp5, market %in% topCountryName))
visitsp6$spend=(visitsp6$spend)*1000
visitsp6$spendDaysVisitor=(visitsp6$spend)/(visitsp6$nights)

visitsp6$nightsVisitor=(visitsp6$nights)/(visitsp6$visits)





##********************************************
#
#  RandomForest
#
##********************************************

#randomForest : regarder les variables impactant la dépense
visitsp6 <- select (visitsp6, -quarter, -dur_stay, -spend, -visits, -nights)
visitsp6 <- mutate(visitsp6, purpose = factor(purpose, level=c("Study", "Miscellaneous", "VFR", "Holiday", "Business")))
visitsp6 <- mutate(visitsp6, mode = factor(mode, level=c("Sea", "Tunnel", "Air")))
visitsp6 <- mutate(visitsp6, market = factor(market, level=c("USA", "France", "Germany", "Spain", "Italy", "Netherlands",  
                                                             "Belgium","Irish Republic", "Sweden", "Australia")))
model_randomForest <- randomForest(spendDaysVisitor ~ ., data=visitsp6, na.action=na.roughfix, localImp=TRUE)
print(fit1)
varImpPlot(fit1)
#on observe que le mode de transport et l'année n'influent pas sur les dépenses

#prediction des dépenses selon un profil donné
newdata=data.frame(year=2013,market="Spain", mode="Air",purpose="Business",spendDaysVisitor=154,
                   nightsVisitor=10)
pred_randomForest <- predict(model_randomForest,newdata, type = "response")


visitsp7 <- data.frame(visitsp5)
#on ajoute une colonne depense en livres par personne
visitsp7$spendPerVisitor<-((visitsp7$spend*1000)/visitsp7$visits)

#on ajoute une colonne depense en livres par jour par personne
visitsp7$spendPerDayPerVisitor<-((visitsp7$spend*1000)/visitsp7$nights)
visitsp7 <- select (visitsp7, -area, -quarter, -nights, -spend)




#*************************************************************
#
#  Evolution du volume de visiteurs à Londres de 2013 à 2017
#
#*************************************************************


# Volume de visites selon les années de 2013 à 2017
dataPerYear <- ddply(visitsp5, .(year), summarise, sum=sum(visits, na.rm=T))
ggplot(dataPerYear, aes(x=year, y=sum))+geom_bar(stat="identity", fill="steelblue")+ggtitle(
  "Volume de visiteurs par an de 2013 à 2017")+xlab("Année")+ylab("Nombre de visiteurs (en millier)")

# Par saison (quarter)
dataPerQuarter <- ddply(subset(visits, year %in% p1318), .(year, quarter), summarise, sum=sum(visits))
ggplot(dataPerQuarter, aes(x=year, y=sum, fill=quarter))+geom_bar(stat="identity", position="dodge")+ggtitle(
  "Volume de visiteurs de 2013 à 2017 par quarter")+xlab("Année")+ylab("Nombre de visiteurs(en milliers)")

# Comparaison de nos chiffres avec les chiffres des journaux sur 2015
test4 <- ddply(subset(visitsp6, year=="2015"), .(year), summarise, sum=sum(visits))
#response : 18.58115 millions
#d'après le figaro 18,6 millions, c'est cohérent

#*************************************
#
#  Profil des visiteurs
#
#*************************************

#par année
#2017
profil17 <- ddply(subset(visitsp5, year %in%"2017"), .(market), summarise, sum=sum(visits))
g7 <- ggplot(head(arrange(profil17, -sum), 6), aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")

#2016
profil16 <- ddply(subset(visitsp5, year %in%"2016"), .(market), summarise, sum=sum(visits))
g6 <- ggplot(head(arrange(profil16, -sum), 6), aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")
glimpse(g6)

#2015
profil15 <- ddply(subset(visitsp5, year %in%"2015"), .(market), summarise, sum=sum(visits))
g5 <- ggplot(head(arrange(profil15, -sum), 6), aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")

#2014
profil14 <- ddply(subset(visitsp5, year %in%"2014"), .(market), summarise, sum=sum(visits))
g4 <- ggplot(head(arrange(profil14, -sum), 6), aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")

#2013
profil13 <- ddply(subset(visitsp5, year %in%"2013"), .(market), summarise, sum=sum(visits))
g3 <- ggplot(head(arrange(profil13, -sum), 6), aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")

#visualisation sur une seule planche
plot_grid(g7, g6,g5, g4,g3,labels=c("2017", "2016","2015", "2014", "2013"), ncol = 3, nrow = 2)




#top des pays d'origine des visiteursles plus nombreux sur 2013-2017
perCountry <- ddply(visitsp5, .(market), summarise, sum=sum(visits))
topCountry <- head(arrange(perCountry, -sum),6)
#ggplot(topCountry, aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")
#ou sans passer par topCountry (plus compacte)
p1 <- ggplot(topCountry, aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")+labs(x="Pays",y="Nombre de visiteurs",fill = "Pays")

#on a ici les topCOuntry pour le nombre de visites, qu'en est-il pour le nombres de nuits passees et le depenses ?
nightsPerCountry  <- ddply(visitsp5, .(market), summarise, sum=sum(nights))
topCountryNights <- head(arrange(nightsPerCountry, -sum),6)
p2 <- ggplot(topCountryNights, aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")+labs(x="Pays",y="Nombre de nuits",fill = "Pays")

spendPerCountry  <- ddply(visitsp5, .(market), summarise, sum=sum(spend))
topCountrySpend <- head(arrange(spendPerCountry, -sum),6)
p3 <- ggplot(topCountrySpend, aes(x=market, y=sum, fill=market))+geom_bar(stat="identity",position="dodge")+labs(x="Pays",y="Millions de livres",fill = "Pays")

#get average spend per visitor on a set or subset
getAverageSpendPerVisitor <- function (mydata) {
  return (sum(mydata$spendPerVisitor*mydata$visits)/sum(mydata$visits))
}

#get average spend per day per visitor on a set or subset
getAverageSpendPerDayPerVisitor <- function (mydata) {
  return (sum(mydata$spendPerDayPerVisitor*mydata$visits)/sum(mydata$visits))
}

spendPerVisitorPerCountry  <- ddply(visitsp7, .(market), getAverageSpendPerVisitor)
topCountryspendPerVisitor<- head(arrange(spendPerVisitorPerCountry, -V1),6)
p4 <- ggplot(topCountryspendPerVisitor, aes(x=market, y=V1, fill=market))+geom_bar(stat="identity",position="dodge")+labs(x="Pays",y="Livres",fill = "Pays")

spendPerDayPerVisitorPerCountry  <- ddply(visitsp7, .(market), getAverageSpendPerDayPerVisitor)
topCountryspendPerDayPerVisitor<- head(arrange(spendPerDayPerVisitorPerCountry, -V1),6)
p5 <- ggplot(topCountryspendPerDayPerVisitor, aes(x=market, y=V1, fill=market))+geom_bar(stat="identity",position="dodge")+labs(x="Pays",y="Livres",fill = "Pays")


plot_grid(p1, p2,p3, p4, p5,labels=c("Les 6 premiers pays en nombre de visiteurs", "Les 6 premiers pays en nombre nuits","Les 6 premiers pays en d?pense totale","Les 6 premiers pays en d?pense par personne","Les 6 premiers pays en d?pense par jour, par personne"), ncol = 1, nrow = 5)


#nombre de visiteurs par motifs de venue
perPurpose <- ddply(visitsp5, .(market, purpose), summarise, sum2=sum(visits))
ggplot(perPurpose, aes(x=purpose, y=sum2, fill=purpose))+geom_bar(stat="identity",position="dodge")+labs(title="Volume de visites en fonction du motif",x="Motif",y="Somme",fill = "Motif")


#nombre de visiteur par motifs pour les pays dans le topCountry
testCountry <- ddply(subset(visitsp5, market %in% topCountry$market), .(market, purpose), summarise, sum=sum(visits))
ggplot(testCountry, aes(x=purpose, y=sum, fill=market))+geom_bar(stat="identity", position="dodge")

#nombre de visiteurs par motifs de venue et par ans
purposePerYear <-ddply(visitsp5, .(year, purpose), summarise, sum=sum(visits))
ggplot(purposePerYear, aes(x=year, y=sum, colour=purpose))+geom_line() + geom_point()+labs(title="Volume de visites en fonction du motif et de l'année",x="Années",y="Nombres de visites", color = "Motif")

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
ggplot(transport, aes(x=year, y=sum, fill=mode))+geom_bar(stat="identity", position="dodge")+labs(title="Volume de visiteurs selon le mode de transport",x="Année",y="Nombre de visites",fill = "Mode")

#toujours majoritairement par avion

#transport utilisé en fonction du pays - pour les 6 principaux pays
transportPerCountry <- ddply(subset(visitsp5, market %in% topCountry$market), .(market, mode), summarise, sum=sum(visits))
ggplot(transportPerCountry, aes(x=market, y=sum, fill=mode))+geom_bar(stat="identity")+labs(title="Volume de visiteurs selon le mode de transport et le pays",x="Pays",y="Nombre de visites",fill = "Mode")

transportPerQuarter <- ddply(visitsp5, .(quarter, mode), summarise, sum=sum(visits))
ggplot(transportPerQuarter, aes(x=quarter, y=sum, fill=mode))+geom_bar(stat="identity", position="dodge")

#faire une mediane 
#ou diagramme de bar par année et quarter
#incorporer 2018
visitsPerQuarter <- ddply(visitsp5, .(year, quarter), summarise, sum=sum(visits))
ggplot(visitsPerQuarter, aes(x=year, y=sum, fill=quarter))+geom_bar(stat="identity", position="dodge")
#moins de fréquentation en Q1

#transport utilise en fonction du motif
transportPerPurpose <- ddply(visitsp5, .(purpose, mode), summarise, sum=sum(visits))
ggplot(transportPerPurpose, aes(x=purpose, y=sum, fill=mode))+geom_bar(stat="identity",position = "dodge")+labs(title="Volume de visiteurs selon le mode de transport et le motif de visite",x="Motif",y="Nombre de visites",fill = "Mode")


#en conclusion peu de variation pour le mode de transport que ce soit en fonction des années ou des saisons

#**************************************
#
#Montant des dépenses par jour
#
#**************************************


spendPerPurpose <- ddply(visitsp5, .(year, purpose, spend), summarise, sum=sum(visits))
ggplot(spendPerPurpose, aes(x=year, y=sum, fill=purpose))+geom_bar(stat="identity", position="dodge")
spend <- ddply(visitsp5, .(dur_stay, spend))

#**************************************
#
#Montant des depenses par jour et par personne au fil des annees, suivant certains facteurs
#
#**************************************

#Facteur: Duree
spendPerDuration <- ddply(visitsp7, .(year, dur_stay),  getAverageSpendPerDayPerVisitor)
ggplot(spendPerDuration, aes(x=year, y=V1, colour=dur_stay))+geom_line() + geom_point()+labs(title="Depenses en fonction la duree et de l'annee",x="Annees",y="Livres",color = "Duree")

#Facteur: Mode
spendPerDuration <- ddply(visitsp7, .(year, mode),  getAverageSpendPerDayPerVisitor)
ggplot(spendPerDuration, aes(x=year, y=V1, colour=mode))+geom_line() + geom_point()+labs(title="Depenses en fonction du mode de transport et de l'annee",x="Annees",y="Livres",color = "Transport")

#Facteur: Motif
spendsPerPurpose <- ddply(visitsp7, .(year, purpose),  getAverageSpendPerDayPerVisitor)
ggplot(spendsPerPurpose, aes(x=year, y=V1, colour=purpose))+geom_line() + geom_point()+labs(title="Depenses en fonction du motif et de l'annee",x="Annees",y="Livres",color = "Motif")


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

