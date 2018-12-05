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
                    "Irish Republic")


# découpage du dataset pour la période 2013-2017
visitsp5 <- subset(visitsLondon, year %in% p5)
visitsp5 <- select(visitsp5, -sample, -area)
#on ajoute une colonne depense en livres par jour par personne
visitsp5$spendPerDayPerVisitor<-((visitsp5$spend*1000)/visitsp5$nights)
glimpse(visitsp5)


#test pour les dépenses en livres par personnes
visitsp7 <- data.frame(visitsp5)
visitsp7$spendPerVisitor<-((visitsp7$spend*1000)/visitsp7$visits)
visitsp7 <- select (visitsp7, -quarter, -nights, -spend)



##********************************************
#
#  RandomForest
#
##********************************************

moreVisits <- ddply(subset(visitsp5), .(market), summarise, sum=sum(visits))
moreVisits <- tail(arrange(moreVisits, sum),40)
View(moreVisits$market)

#randomForest : regarder les variables impactant la dépense
visitsp6 <- data.frame(subset(visitsp5))
visitsp6$nightPerVisitor <- (visitsp6$nights/visitsp6$visits) 
visitsp6 <- select (visitsp6, -quarter,-dur_stay, -spend, -visits, -nights)
visitsp6 <- mutate(visitsp6, purpose = factor(purpose, level=c("Study", "Miscellaneous", "VFR", 
                                                               "Holiday", "Business")))
visitsp6 <- mutate(visitsp6, mode = factor(mode, level=c("Sea", "Tunnel", "Air")))
#visitsp6 <- mutate(visitsp6, market = factor(market, level=c("Australia", "Irish Republic",
#                                                             "Netherlands","Spain","Italy",
#                                                             "Germany","France","USA")))
visitsp6 <- mutate(visitsp6, market = factor(market, level=moreVisits$market))

model_randomForest <- randomForest(spendPerDayPerVisitor ~ ., data=visitsp6, na.action=na.roughfix, localImp=TRUE)
print(fit1)
varImpPlot(fit1)
#le nombre de nuits influerait le plus sur la dépense journalière
#on observe que le mode de transport et l'année n'influent pas sur les dépenses



#*************************************************************
#
#  Evolution du volume de visiteurs à Londres de 2013 à 2017
#
#*************************************************************


# Volume de visites selon les années de 2013 à 2017
dataPerYear <- ddply(visitsp5, .(year), summarise, sum=sum(visits, na.rm=T))
ggplot(dataPerYear, aes(x=year, y=sum))+
  geom_bar(stat="identity", fill="steelblue")+
  ggtitle("Volume de visiteurs par an de 2013 à 2017")+
  xlab("Année")+ylab("Nombre de visiteurs (en millier)")

# Par saison (quarter)
dataPerQuarter <- ddply(subset(visits, year %in% p1318), .(year, quarter), summarise, sum=sum(visits))
ggplot(dataPerQuarter, aes(x=year, y=sum, fill=quarter))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Volume de visiteurs de 2013 à 2017 par quarter")+
  labs(x="Années",y="Nombre de visiteurs(en milliers)")
  

# Comparaison de nos chiffres avec les chiffres des journaux sur 2015
test4 <- ddply(subset(visitsp5, year=="2015"), .(year), summarise, sum=sum(visits))
#resultat : 18.58115 millions
#d'après le figaro 18,6 millions, c'est cohérent


#*********************************************************
#
#  Evolution de la somme dépensée par les visiteur par an 
#
#*********************************************************


# évolution globale de la somme dépensée par les visiteurs par années
spendEvol <- ddply(visitsp5, .(year), summarise, sum=sum(spend))
ggplot(spendEvol, aes(x= year, y=sum)) + 
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=sum), vjust=1.6, color="white", size=3.5)+
  labs(x="dépenses en millions",y="Années")+
  ggtitle("Somme dépensée par les visiteurs par année sur 2013-2017")

# on rajoute le motif 
spendPerPurpose <- ddply(visitsp5, .(year, purpose, spend), summarise, sum=sum(visits))
ggplot(spendPerPurpose, aes(x=year, y=sum, fill=purpose))+
  geom_bar(stat="identity", position="fill")+
  ggtitle("Somme dépensée en fonction du motif par année sur 2013-2017")
# les vacances rapporte le plus, suit le business et le VFR


#********************************************************
#
#  Somme des visiteurs par pays d'origine sur 2013-2017
#
#********************************************************

# somme des visiteurs par pays d'origine sur 2013-2017
perCountry <- ddply(visitsp5, .(market), summarise, sum=sum(visits))
topCountry <- head(arrange(perCountry, -sum),7)
# creation du vecteur TopCountryName contenant les noms du topCountry (voir def en haut)
ggplot(perCountry, aes(x=market, y=sum, colour = factor(market %in% topCountryName), size=sum))+
        geom_point(stat="identity",position="dodge")+
        labs(x="Pays",y="Nombre de visiteurs (en milliers)",fill = "Pays")+
        ggtitle("Nombre de visiteurs par pays sur la période 2013-2017")

# on a ainsi pu determiner les 6 pays amenant le plus de de visiteurs sur la période

p1 <- ggplot(topCountry, aes(x=market, y=sum, fill=market))+
  geom_bar(stat="identity",position="dodge")+
  labs(x="Pays",y="Nombre de visiteurs (en milliers)",fill = "Pays")
#  ggtitle("Les 7 premiers pays à venir en plus grand nombre à Londres sur 2013-2017")



#**************************************************************
#
#  top country pour le nombres de nuits passees et les depenses 
#
#*************************************************************

#on a ici les topCOuntry pour le nombre de visites, qu'en est-il pour le nombres de nuits passees et leS depenses ?

# Top country pour le nombre de nuits passées
nightsPerCountry  <- ddply(visitsp5, .(market), summarise, sum=sum(nights))
topCountryNights <- head(arrange(nightsPerCountry, -sum),7)
p2 <- ggplot(topCountryNights, aes(x=market, y=sum, fill=market))+
  geom_bar(stat="identity",position="dodge")+
  labs(x="Pays",y="Nombre de nuits",fill = "Pays")


# Top country pour la somme des dépense
spendPerCountry  <- ddply(visitsp5, .(market), summarise, sum=sum(spend))
topCountrySpend <- head(arrange(spendPerCountry, -sum),7)
p3 <- ggplot(topCountrySpend, aes(x=market, y=sum, fill=market))+
  geom_bar(stat="identity",position="dodge")+
  labs(x="Pays",y="Millions de livres",fill = "Pays")


#*************** Fonctions *****************

#get average spend per visitor on a set or subset
getAverageSpendPerVisitor <- function (mydata) {
  return (sum(mydata$spendPerVisitor*mydata$visits)/sum(mydata$visits))
}

#get average spend per day per visitor on a set or subset
getAverageSpendPerDayPerVisitor <- function (mydata) {
  return (sum(mydata$spendPerDayPerVisitor*mydata$visits)/sum(mydata$visits))
}

# dépense des visiteurs par pays
spendPerVisitorPerCountry  <- ddply(visitsp7, .(market), getAverageSpendPerVisitor)
topCountryspendPerVisitor<- head(arrange(spendPerVisitorPerCountry, -V1),7)
p4 <- ggplot(topCountryspendPerVisitor, aes(x=market, y=V1, fill=market))+
  geom_bar(stat="identity",position="dodge")+
  labs(x="Pays",y="Livres",fill = "Pays")

# dépense par jour des visiteurs par pays
spendPerDayPerVisitorPerCountry  <- ddply(visitsp7, .(market), getAverageSpendPerDayPerVisitor)
topCountryspendPerDayPerVisitor<- head(arrange(spendPerDayPerVisitorPerCountry, -V1),7)
p5 <- ggplot(topCountryspendPerDayPerVisitor, aes(x=market, y=V1, fill=market))+
  geom_bar(stat="identity",position="dodge")+
  labs(x="Pays",y="Livres",fill = "Pays")


#on affiche les résultats précedents
plot_grid(p1, p2,p3, p4, p5,labels=c("Les 7 premiers pays en nombre de visiteurs", 
  "Les 7 premiers pays en nombre nuits",
  "Les 7 premiers pays en depense totale",
  "Les 7 premiers pays en depense par personne",
  "Les 7 premiers pays en depense par jour, par personne"), ncol = 1, nrow = 5)


#******************************************************************************
#
#  On regarde l'évolution par année des 6 premiers pays en nombre de visiteurs
#
#******************************************************************************

#on souhaite regarder si le top country par nombre de visiteurs précedent varie en zoomant par années

#2017
profil17 <- ddply(subset(visitsp5, year %in%"2017"), .(market), summarise, sum=sum(visits))
g7 <- ggplot(head(arrange(profil17, -sum), 6), aes(x=market, y=sum, fill=market))+
  geom_bar(stat="identity",position="dodge")

#2016
profil16 <- ddply(subset(visitsp5, year %in%"2016"), .(market), summarise, sum=sum(visits))
g6 <- ggplot(head(arrange(profil16, -sum), 6), aes(x=market, y=sum, fill=market))+
  geom_bar(stat="identity",position="dodge")
glimpse(g6)

#2015
profil15 <- ddply(subset(visitsp5, year %in%"2015"), .(market), summarise, sum=sum(visits))
g5 <- ggplot(head(arrange(profil15, -sum), 6), aes(x=market, y=sum, fill=market))+
  geom_bar(stat="identity",position="dodge")

#2014
profil14 <- ddply(subset(visitsp5, year %in%"2014"), .(market), summarise, sum=sum(visits))
g4 <- ggplot(head(arrange(profil14, -sum), 6), aes(x=market, y=sum, fill=market))+
  geom_bar(stat="identity",position="dodge")

#2013
profil13 <- ddply(subset(visitsp5, year %in%"2013"), .(market), summarise, sum=sum(visits))
g3 <- ggplot(head(arrange(profil13, -sum), 6), aes(x=market, y=sum, fill=market))+
  geom_bar(stat="identity",position="dodge")

#visualisation sur une seule planche
plot_grid(g7, g6,g5, g4,g3,labels=c("2017", "2016","2015", "2014", "2013"), ncol = 3, nrow = 2)

# les premiers pays USA, France, Allemagne, Espagne, Italie ne changent pas
# l'Australie a disparue du top après 2013 en faveur des Pays-bas, ensuite remplacé en 2015 par l'Irlande



##**********************************
#
#  Evolution des visites par motifs
#
##**********************************

# nombre de visiteurs par motif de venue
perPurpose <- ddply(visitsp5, .(market, purpose), summarise, sum2=sum(visits))
ggplot(perPurpose, aes(x=purpose, y=sum2, fill=purpose))+
  geom_bar(stat="identity",position="dodge")+
  labs(title="Volume de visites en fonction du motif",x="Motif",y="Somme",fill = "Motif")

# nombre de visiteur par motif pour les pays dans le topCountry
testCountry <- ddply(subset(visitsp5, market %in% topCountryName), .(market, purpose), summarise, sum=sum(visits))
ggplot(testCountry, aes(x=purpose, y=sum, fill=market))+
  geom_bar(stat="identity", position="dodge")

# nombre de visiteurs par motif de venue et par ans
purposePerYear <-ddply(visitsp5, .(year, purpose), summarise, sum=sum(visits))
ggplot(purposePerYear, aes(x=year, y=sum, colour=purpose))+
  geom_line()+geom_point()+
  labs(title="Volume de visites en fonction du motif et de l'année",x="Années",y="Nombres de visites", color = "Motif")


# évolution de la part des gens venu pour la travail de 2010 à 2017
data1 <- subset(visits, year %in% p1)
businessData <- ddply(subset(data1, purpose == "Business"), .(year), summarise, sum=sum(sample))
ggplot(businessData, aes(x=year, y=sum)) + geom_point(size=4) + geom_line(size=2)
ggplot(businessData, aes(x=year, y=sum)) + geom_bar(stat="identity")
# il y a vraiment une différence entre les point et les barres pour la visualisation
# baisse du au brexit ? il faudrait que regarder le pourcentage, voir si c'est significatif

# évolution pour les vacances
holidayData <- ddply(subset(data1, purpose == "Holiday"), .(year), summarise, sum=sum(sample))
ggplot(holidayData, aes(x=year, y=sum, fill=year)) + geom_bar(stat="identity")
# ça semple rester stable



##********************************************
##
##Evolution des moyens de transports dans le temps
##
##********************************************

transport <- ddply(visitsp5, .(year, mode), summarise, sum=sum(visits))
ggplot(transport, aes(x=year, y=sum, fill=mode))+
  geom_bar(stat="identity", position="dodge")+
  labs(title="Volume de visiteurs selon le mode de transport",x="Année",y="Nombre de visites",fill = "Mode")
# toujours majoritairement par avion

# transport utilisé en fonction du pays - pour les 6 principaux pays
transportPerCountry <- ddply(subset(visitsp5, market %in% topCountry$market), .(market, mode), summarise, sum=sum(visits))
ggplot(transportPerCountry, aes(x=market, y=sum, fill=mode))+
  geom_bar(stat="identity")+
  labs(title="Volume de visiteurs selon le mode de transport et le pays",x="Pays",y="Nombre de visites",fill = "Mode")

transportPerQuarter <- ddply(visitsp5, .(quarter, mode), summarise, sum=sum(visits))
ggplot(transportPerQuarter, aes(x=quarter, y=sum, fill=mode))+
  geom_bar(stat="identity", position="dodge")
# stable selon les saisons

# transport utilise en fonction du motif
transportPerPurpose <- ddply(visitsp5, .(purpose, mode), summarise, sum=sum(visits))
ggplot(transportPerPurpose, aes(x=purpose, y=sum, fill=mode))+
  geom_bar(stat="identity", position="fill")+
  labs(title="Volume de visiteurs selon le mode de transport et le motif de visite",x="Motif",y="Nombre de visites",fill = "Mode")



#*************************************************************************************
#
# Montant des depenses par jour par personne et par annees, suivant certains facteurs
#
#*************************************************************************************

# Facteur: Duree
spendPerDuration <- ddply(visitsp7, .(year, dur_stay),  getAverageSpendPerDayPerVisitor)
ggplot(spendPerDuration, aes(x=year, y=V1, colour=dur_stay))+
  geom_line() + geom_point()+
  labs(title="Depenses en fonction la duree et de l'annee",x="Annees",y="Livres",color = "Duree")

# Facteur: Mode
spendPerDuration <- ddply(visitsp7, .(year, mode),  getAverageSpendPerDayPerVisitor)
ggplot(spendPerDuration, aes(x=year, y=V1, colour=mode))+
  geom_line() + geom_point()+
  labs(title="Depenses en fonction du mode de transport et de l'annee",x="Annees",y="Livres",color = "Transport")

# Facteur: Motif
spendsPerPurpose <- ddply(visitsp7, .(year, purpose),  getAverageSpendPerDayPerVisitor)
ggplot(spendsPerPurpose, aes(x=year, y=V1, colour=purpose))+
  geom_line() + geom_point()+
  labs(title="Depenses en fonction du motif et de l'annee",x="Annees",y="Livres",color = "Motif")











##***************************************************************************************
##
## Chercher une correlation entre le nombre de crimes et l'affluence de touristes a Londres 
##
##**************************************************************************************

# Afluence de touristes par quart d'annee de 2008 a 2016
perQuarter <- ddply(visits, .(year,quarter),summarise, sum=sum(sample))
perQuarter2008_2016 <- subset(perQuarter,(year>=2008&year<=2016))
p <- ggplot(perQuarter2008_2016, aes(x=factor(year), y=sum, fill=quarter))+
  geom_bar(stat="identity",position="dodge")+
  labs(title="Afluence de touristes par quart d'annee \n de 2008 ? 2016",x ="Annee", y = "Nombre de touristes")

# Recuperer le dataset sur les crimes a Londres
crimes <- read.table(file="london_crime_by_lsoa.csv",header=TRUE,sep=",")
# Traiter les donnees sur le crime pour qu'elles soient comparables avec les donnees sur l'affluence : ajout de la colonne quarter
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
# levels(factor(crimes$major_category))

# Nombres de vols par quart d'annee de 2008 a 2016
theft <- subset(crimes,(major_category=="Burglary"|major_category=="Robbery"|major_category=="Theft and Handling"),select=c(major_category,value,year,quarter))
theftPerQuarter <- ddply(theft, .(year,quarter), summarise, sum=sum(value))
p <- ggplot(theftPerQuarter, aes(x=factor(year), y=sum, fill=quarter))+
  geom_bar(stat="identity",position="dodge")+
  labs(title="Nombres de vols par quart d'ann?e \n de 2008 ? 2016",x ="Ann?e", y = "Nombre de vols")

# Nombres de violences contre la personne de 2008 a 2016
violenceAgainstPerson <- subset(crimes,(major_category=="Violence Against the Person"|major_category=="Sexual Offences"),select=c(major_category,value,year,quarter))
violenceAgainstPersonPerQuarter <- ddply(violenceAgainstPerson, .(year,quarter), summarise, sum=sum(value))
p <- ggplot(violenceAgainstPersonPerQuarter, aes(x=factor(year), y=sum, fill=quarter))+
  geom_bar(stat="identity",position="dodge")+
  labs(title="Nombres de cas violences contre la personne \n par quart d'ann?e  de 2008 ? 2016",x ="Ann?e", y = "Nombre de cas")

