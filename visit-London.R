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
library(cowplot)


#importation du fichier csv
visitsLondon <- read.table(file="international-visitors-london-raw.csv",header=TRUE,sep=",")
#decommenter la ligne suivante pour verifier l'import
#View(visitsLondon)


# definissions des vecteurs de periodes
p5    <- c("2013", "2014","2015", "2016", "2017")
p1318 <- c("2013", "2014","2015", "2016", "2017", "2018")


# decoupage du dataset pour la periode 2013-2017
visitsp5 <- subset(visitsLondon, year %in% p5)
visitsp5 <- select(visitsp5, -sample, -area)
# on ajoute une colonne depense en livres par jour par personne
visitsp5$spendPerDayPerVisitor<-((visitsp5$spend*1000)/visitsp5$nights)


# test pour les depenses en livres par personnes
visitsp7 <- data.frame(visitsp5)
visitsp7$spendPerVisitor<-((visitsp7$spend*1000)/visitsp7$visits)
visitsp7$nightsPerVisitor<-visitsp7$nights/visitsp7$visits
visitsp7 <- select (visitsp7, -quarter, -nights, -spend)


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


#********************************************************
#
#  Somme des visiteurs par pays d'origine sur 2013-2017
#
#********************************************************

# somme des visiteurs par pays d'origine sur 2013-2017
perCountry <- ddply(visitsp5, .(market), summarise, sum=sum(visits))
topCountry <- head(arrange(perCountry, -sum),7)
# creation du vecteur TopCountryName contenant les noms du topCountry (voir def en haut)
ggplot(perCountry, aes(x=market, y=sum, colour = factor(market %in% topCountry$market), size=sum))+
        geom_point(stat="identity",position="dodge")+
        labs(x="Pays",y="Nombre de visiteurs (en milliers)",fill = "Pays",colour="Le pays appartient au top",size="Visiteurs (milliers)")+
        ggtitle("Nombre de visiteurs par pays sur la periode 2013-2017")

# on a ainsi pu determiner les 7 pays amenant le plus de de visiteurs sur la periode

topCountry <- arrange(topCountry, sum)
topCountry$market <- factor(topCountry$market, levels=topCountry$market)
p1 <- ggplot(topCountry, aes(x=market, y=sum, fill=market))+
  geom_bar(stat="identity",position="dodge")+
  labs(x="Pays",y="Nombre de visiteurs (en milliers)",fill = "Pays")+
  ggtitle("Les 7 premiers pays a venir en plus grand nombre a Londres")
p1


#**************************************************************
#
#  top country pour les depenses 
#
#*************************************************************

#*************** Fonctions *****************

#get average spend per day per visitor on a set or subset
getAverageSpendPerDayPerVisitor <- function (mydata) {
  return (sum(mydata$spendPerDayPerVisitor*mydata$visits)/sum(mydata$visits))
}

#*************** EndFonctions ***************

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
plot_grid(p3, p5,labels=c("Les 7 premiers pays en depense totale sur 2013-2017",
                          "Les 7 premiers pays en depenses par jour et personne sur 2013-2017"), ncol = 1, nrow = 2)



#*************************************************************************************
#
# Montant des depenses par jour par personne et par annees, par motif
#
#*************************************************************************************

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


##********************************************
##
##Evolution des moyens de transports dans le temps
##
##********************************************

# Nombre de visiteurs total
transport <- ddply(visitsp5, .(year, mode), summarise, sum=sum(visits))
t1 <- ggplot(transport, aes(x=year, y=sum, fill=mode))+
  geom_bar(stat="identity", position="dodge")+
  labs(title ="Volume de visiteurs selon le mode de transport",x="Annee",y="Nombre de visites",fill = "Mode")
# toujours majoritairement par avion


# Depenses moyennes par jour et par personnes
spendPerDuration <- ddply(visitsp7, .(year, mode),  getAverageSpendPerDayPerVisitor)
t2 <- ggplot(spendPerDuration, aes(x=year, y=V1, colour=mode))+
  geom_line(size=1) + geom_point(size=2)+
  labs(title="Depenses en moyenne par jour et par personnes
       en fonction du mode de transport et de l'annee", 
       x="Annee",y="Livres",color = "Transport")

plot_grid(t1, t2, ncol=2, nrow=1)
