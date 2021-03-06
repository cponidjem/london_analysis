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

#importation du fichier csv
visitsLondon <- read.table(file="international-visitors-london-raw.csv",header=TRUE,sep=",")
View(visitsLondon)


# definissions des periodes & autres vecteurs
p5    <- c("2013", "2014","2015", "2016", "2017")
p1318 <- c("2013", "2014","2015", "2016", "2017", "2018")

topCountryName <- c("USA", "France", "Germany", "Spain", "Italy", "Netherlands",  
                    "Irish Republic")


# decoupage du dataset pour la periode 2013-2017
visitsp5 <- subset(visitsLondon, year %in% p5)
visitsp5 <- select(visitsp5, -sample, -area)
# on ajoute une colonne depense en livres par jour par personne
visitsp5$spendPerDayPerVisitor<-((visitsp5$spend*1000)/visitsp5$nights)
glimpse(visitsp5)

test7 <- ddply(visitsp5, .(year), summarise, sum=sum(visits))
mean(test7$sum)

# test pour les depenses en livres par personnes
visitsp7 <- data.frame(visitsp5)
visitsp7$spendPerVisitor<-((visitsp7$spend*1000)/visitsp7$visits)
visitsp7$nightsPerVisitor<-visitsp7$nights/visitsp7$visits
visitsp7 <- select (visitsp7, -quarter, -nights, -spend)



##********************************************
#
#  RandomForest
#
##********************************************

moreVisits <- ddply(subset(visitsp5), .(market), summarise, sum=sum(visits))
moreVisits <- tail(arrange(moreVisits, sum),40)
View(moreVisits$market)

# randomForest : regarder les variables impactant la depense
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
# le nombre de nuits influerait le plus sur la depense journaliere
# on observe que le mode de transport et l'annee n'influent pas sur les depenses



#*************************************************************************************
#
#  Evolution du volume de visiteurs a Londres de 2013 a 2017 et leur dépenses totales
#
#*************************************************************************************


# Volume de visites selon les annees de 2013 a 2017
dataPerYear <- ddply(visitsp5, .(year), summarise, sum=sum(visits, na.rm=T))
v1 <- ggplot(dataPerYear, aes(x=year, y=sum))+
  geom_bar(stat="identity", fill="steelblue")+
  xlab("Annee")+ylab("Nombre de visiteurs (en milliers)")
# ggtitle("Volume de visiteurs par an de 2013 a 2017")+

# Par trimestre (quarter)
dataPerQuarter <- ddply(subset(visitsLondon, year %in% p1318), .(year, quarter), summarise, sum=sum(visits, na.rm=T))
v2 <- ggplot(dataPerQuarter, aes(x=year, y=sum, fill=quarter))+
  geom_bar(stat="identity", position="dodge")+
  labs(x="Annees",y="Nombre de visiteurs(en milliers)")
# ggtitle("Volume de visiteurs de 2013 a 2018 par quarter")+

plot_grid(v1, v2, labels=c("Volume de visiteurs par an de 2013 a 2017", 
                           "Volume de visiteurs de 2013 a 2018 par quarter"), ncol = 2, nrow = 1)


# Comparaison de nos chiffres avec les chiffres des journaux sur 2015
test4 <- ddply(subset(visitsp5, year=="2015"), .(year), summarise, sum=sum(visits))
#resultat : 18.58115 millions
#d'apres le figaro 18,6 millions, c'est coherent


# Evolution de la somme depensee par les visiteurs par annees sur 2013-2017
spendEvol <- ddply(visitsp5, .(year), summarise, sum=sum(spend))
spendEvol$sum <- spendEvol$sum/1000
ggplot(spendEvol, aes(x= year, y=sum)) + 
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=sum), vjust=1.6, color="white", size=3.5)+
  ggtitle("Depenses par annee sur 2013-2017")+
  labs(x="Annees",y="Milliards de livres")




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
  ggtitle("Nombre de visiteurs par pays sur la periode 2013-2017")

# on a ainsi pu determiner les 6 pays amenant le plus de de visiteurs sur la pÃ©riode

topCountry <- arrange(topCountry, sum)
topCountry$market <- factor(topCountry$market, levels=topCountry$market)
p1 <- ggplot(topCountry, aes(x=market, y=sum, fill=market))+
  geom_bar(stat="identity",position="dodge")+
  labs(x="Pays",y="Nombre de visiteurs (en milliers)",fill = "Pays")+
  ggtitle("Les 7 premiers pays a venir en plus grand nombre a Londres")



#**************************************************************
#
#  top country pour le nombres de nuits passees et les depenses 
#
#*************************************************************

#*************** Fonctions *****************

#get average spend per visitor on a set or subset
getAverageSpendPerVisitor <- function (mydata) {
  return (sum(mydata$spendPerVisitor*mydata$visits)/sum(mydata$visits))
}

#get average spend per day per visitor on a set or subset
getAverageSpendPerDayPerVisitor <- function (mydata) {
  return (sum(mydata$spendPerDayPerVisitor*mydata$visits)/sum(mydata$visits))
}

getAverageNbNightsPerVisitor <- function (mydata) {
  return (sum(mydata$nightsPerVisitor*mydata$visits)/sum(mydata$visits))
}

#*************** EndFonctions ***************

# qu'en est-il du top country pour le nombres de nuits passees et leS depenses ?

# Top country pour le nombre totale de nuits passees
nightsPerCountry  <- ddply(visitsp5, .(market), summarise, sum=sum(nights))
topCountryNights <- head(arrange(nightsPerCountry, -sum),7)
p2 <- ggplot(topCountryNights, aes(x=market, y=sum, fill=market))+
  scale_fill_brewer(palette="PuBuGn")+
  geom_bar(stat="identity",position="dodge")+
  labs(x="Pays",y="Nombre de nuits (en milliers)",fill = "Pays")
#  ggtitle("Les 7 premiers pays en nombre nuits")

# Top country pour le nombre moyen de nuits passees par visiteur
nightsPerVisitorPerCountry  <- ddply(visitsp7, .(market), getAverageNbNightsPerVisitor)
topCountryNightsPerVisitor <- head(arrange(nightsPerVisitorPerCountry, -V1),7)
p6 <- ggplot(topCountryNightsPerVisitor, aes(x=market, y=V1, fill=market))+
  scale_fill_brewer(palette="PuBuGn")+
  geom_bar(stat="identity",position="dodge")+
  labs(x="Pays",y="Nombre de nuits (en milliers)",fill = "Pays")
#  ggtitle("Les 7 premiers pays en nombre nuits")

# Top country pour la somme des depense
spendPerCountry  <- ddply(visitsp5, .(market), summarise, sum=sum(spend))
topCountrySpend <- head(arrange(spendPerCountry, -sum),7)
topCountrySpend <- arrange(topCountrySpend, sum)
topCountrySpend$market <- factor(topCountrySpend$market, levels=topCountrySpend$market)
p3 <- ggplot(topCountrySpend, aes(x=market, y=sum, fill=market))+
  scale_fill_brewer(palette="Paired")+
  geom_bar(stat="identity",position="dodge")+
  labs(x="Pays",y="Depenses (en millions de livres)",fill = "Pays")
# ggtitle("Les 7 premiers pays en depense totale sur 2013-2017")

# depense des visiteurs par pays
spendPerVisitorPerCountry  <- ddply(visitsp7, .(market), getAverageSpendPerVisitor)
topCountryspendPerVisitor<- head(arrange(spendPerVisitorPerCountry, -V1),7)
p4 <- ggplot(topCountryspendPerVisitor, aes(x=market, y=V1, fill=market))+
  geom_bar(stat="identity",position="dodge")+
  labs(x="Pays",y="Depenses en livres",fill = "Pays")
# ggtitle("Les 7 premiers pays en depenses par personne")

# Les 7 premiers pays en depense par jour, par personne
spendPerDayPerVisitorPerCountry  <- ddply(visitsp7, .(market), getAverageSpendPerDayPerVisitor)
topCountryspendPerDayPerVisitor<- head(arrange(spendPerDayPerVisitorPerCountry, -V1),7)
topCountryspendPerDayPerVisitor<- arrange(topCountryspendPerDayPerVisitor, V1)
topCountryspendPerDayPerVisitor$market <- factor(topCountryspendPerDayPerVisitor$market, levels=topCountryspendPerDayPerVisitor$market)
p5 <- ggplot(topCountryspendPerDayPerVisitor, aes(x=market, y=V1, fill=market))+
  scale_fill_brewer(palette="YlOrRd")+
  geom_bar(stat="identity",position="dodge")+
  labs(x="Pays",y="Depenses (en livres)",fill = "Pays")


#on affiche les resultats precedents
plot_grid(p1, p2,p3, p4, p5,labels=c(
  "Les 7 premiers pays en nombre de visiteurs", 
  "Les 7 premiers pays en nombre nuits",
  "Les 7 premiers pays en depense totale",
  "Les 7 premiers pays en depense par personne",
  "Les 7 premiers pays en depense par jour, par personne"), ncol = 1, nrow = 5)

plot_grid(p3, p5,labels=c("Les 7 premiers pays en depense totale sur 2013-2017",
                          "Les 7 premiers pays en depenses par jour et personne sur 2013-2017"), ncol = 1, nrow = 2)



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


#********** Par motif ***************

# Proportion des depenses en fonction du motif et par annee sur 2013-2017 
spendPerPurpose <- ddply(visitsp5, .(year, purpose), summarise, sum=sum(spend,na.rm=T))
d1 <- ggplot(spendPerPurpose, aes(x=year, y=sum, fill=purpose))+
  geom_bar(stat="identity", position="fill")+
  labs(x="Annees",y="Proportion des depenses",fill="Motif")
# les vacances rapportent le plus, suit le business et le VFR


# Somme depensee en fonction du motif par annee par jour et par personne sur 2013-2017
spendsPerPurpose <- ddply(visitsp7, .(year, purpose),  getAverageSpendPerDayPerVisitor)
d2 <- ggplot(spendsPerPurpose, aes(x=year, y=V1, colour=purpose))+
  geom_line(size=1) + geom_point(size=2)+
  labs(x="Annees",y="Depenses en livres",color = "Motif")


# nombre de visiteurs par motif de venue et par ans
purposePerYear <-ddply(visitsp5, .(year, purpose), summarise, sum=sum(visits))
d3 <- ggplot(purposePerYear, aes(x=year, y=sum, colour=purpose))+
  geom_line(size=1)+geom_point(size=2)+
  labs(x="Annees",y="Nombres de visites (en milliers)", color = "Motif")


d <- plot_grid(d3, d2, labels=c("Volume de visites par motif et annee","Depenses par jour et personne par motif et annee"), ncol = 1, nrow = 2)
plot_grid(d1, d, labels=c("Proportion des depenses par motif et annee",""), ncol = 2, nrow = 1)

##**********************************
#
#  Evolution des visites par motifs
#
##**********************************

# nombre de visiteurs par motif de venue de 2013 a 2017
perPurpose <- ddply(visitsp5, .(market, purpose), summarise, sum2=sum(visits))
ggplot(perPurpose, aes(x=purpose, y=sum2, fill=purpose))+
  geom_bar(stat="identity",position="dodge")+
  labs(title="Volume de visites en fonction du motif",x="Motif",y="Somme",fill = "Motif")

# nombre de visiteur par motif pour les pays dans le topCountry
testCountry <- ddply(subset(visitsp5, market %in% topCountryName), .(market, purpose), summarise, sum=sum(visits))
ggplot(testCountry, aes(x=purpose, y=sum, fill=market))+
  geom_bar(stat="identity", position="dodge")


##***********************************************
#
#  Evolution des visites par motifs et par années
#
##***********************************************


# Evolution de la part des gens venu pour la travail de 2013 a 2017
data1 <- visitsp5
businessData <- ddply(subset(data1, purpose == "Business"), .(year), summarise, sum=sum(visits))
ggplot(businessData, aes(x=year, y=sum)) + geom_bar(stat="identity")
# pas d'impact visible du brexit en 2017

# Evolution pour les vacances
holidayData <- ddply(subset(data1, purpose == "Holiday"), .(year), summarise, sum=sum(visits))
ggplot(holidayData, aes(x=year, y=sum, fill=year)) + geom_bar(stat="identity")
# augmente avec la croissance génerale du tourisme 



#******************************************************************************
#
#  On regarde l'evolution par annee des 6 premiers pays en nombre de visiteurs
#
#******************************************************************************

#on souhaite regarder si le top country par nombre de visiteurs prÃ©cedent varie en zoomant par annÃ©es

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
# l'Australie a disparue du top apres 2013 en faveur des Pays-bas, ensuite remplace en 2015 par l'Irlande



##********************************************
##
##Evolution des moyens de transports dans le temps
##
##********************************************

transport <- ddply(visitsp5, .(year, mode), summarise, sum=sum(visits))
t1 <- ggplot(transport, aes(x=year, y=sum, fill=mode))+
  geom_bar(stat="identity", position="dodge")+
  labs(title ="Volume de visiteurs selon le mode de transport",x="Annee",y="Nombre de visites",fill = "Mode")
# toujours majoritairement par avion

# transport utilise en fonction du pays - pour les 6 principaux pays
transportPerCountry <- ddply(subset(visitsp5, market %in% topCountry$market), .(market, mode), summarise, sum=sum(visits))
ggplot(transportPerCountry, aes(x=market, y=sum, fill=mode))+
  geom_bar(stat="identity")+
  labs(title="Volume de visiteurs selon le mode de transport et le pays",
       x="Pays",y="Nombre de visites",fill = "Mode")

transportPerQuarter <- ddply(visitsp5, .(quarter, mode), summarise, sum=sum(visits))
ggplot(transportPerQuarter, aes(x=quarter, y=sum, fill=mode))+
  geom_bar(stat="identity", position="dodge")
# stable selon les trimestres

# transport utilise en fonction du motif
transportPerPurpose <- ddply(visitsp5, .(purpose, mode), summarise, sum=sum(visits))
ggplot(transportPerPurpose, aes(x=purpose, y=sum, fill=mode))+
  geom_bar(stat="identity", position="fill")+
  labs(title="Pourcentage de visiteurs selon le mode de transport et le motif de visite",
       x="Motif",y="Pourcentage de visiteurs",fill = "Mode")


# Facteur: Mode
spendPerDuration <- ddply(visitsp7, .(year, mode),  getAverageSpendPerDayPerVisitor)
t2 <- ggplot(spendPerDuration, aes(x=year, y=V1, colour=mode))+
  geom_line(size=1) + geom_point(size=2)+
  labs(title="Depenses en moyenne par jour et par personnes
       en fonction du mode de transport et de l'annee", 
       x="Annee",y="Livres",color = "Transport")

plot_grid(t1, t2, ncol=2, nrow=1)







##***************************************************************************************
##
## Chercher une correlation entre le nombre de crimes et l'affluence de touristes a Londres 
##
##**************************************************************************************

# Afluence de touristes par trimestre de 2008 a 2016
perQuarter <- ddply(visits, .(year,quarter),summarise, sum=sum(sample))
perQuarter2008_2016 <- subset(perQuarter,(year>=2008&year<=2016))
p <- ggplot(perQuarter2008_2016, aes(x=factor(year), y=sum, fill=quarter))+
  geom_bar(stat="identity",position="dodge")+
  labs(title="Afluence de touristes par trimestre \n de 2008 a 2016",x ="Annee", y = "Nombre de touristes")

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
  labs(title="Nombres de vols par trimestre \n de 2008 a 2016",x ="Annee", y = "Nombre de vols")

# Nombres de violences (incluent violences sexuelles) contre la personne de 2008 a 2016
violenceAgainstPerson <- subset(crimes,(major_category=="Violence Against the Person"|major_category=="Sexual Offences"),select=c(major_category,value,year,quarter))
violenceAgainstPersonPerQuarter <- ddply(violenceAgainstPerson, .(year,quarter), summarise, sum=sum(value))
p <- ggplot(violenceAgainstPersonPerQuarter, aes(x=factor(year), y=sum, fill=quarter))+
  geom_bar(stat="identity",position="dodge")+
  labs(title="Nombres de cas de violences contre la personne \n par trimestre de 2008 a 2016",x ="Annee", y = "Nombre de cas")

