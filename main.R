# Importation des biblioth�ques n�cessaires #
library(dplyr)
library(ggplot2)
library(ade4)
library(factoextra)



#install.packages("ade4")
#install.packages("factoextra")
#install.packages("lattice")
#install.packages("geojsonsf")
#install.packages("sf")
#install.packages("readxl")
#install.packages("xlsx")

#install.packages("GGally")
#library(xlsx)

library(geojsonsf)
library(sf)
library(lattice)
library(GGally)
library(readxl)
setwd("C:/Users/victo/Desktop/geostat/grid_analysis")
conso_elec<- read.csv("./data/consommation_annuelle_residentielle_adresse_paris.csv",sep=";")
prod_elec<- read.csv("./data/production-electrique-par-filiere-a-la-maille-region.csv", sep=";")
iris_paris<-st_read(dsn="data",layer="iris_paris")
plot(iris_paris)
# Data preperation #
iris_paris_joined_conso<-merge(iris_paris,conso_elec,by.x="CODE_IRIS",by.y="Code.IRIS",type="full")
iden<-c("Consommation.annuelle.moyenne.par.logement.de.l.adresse..MWh.","Consommation.annuelle.totale.de.l.adresse..MWh.","Nombre.de.logements")
#plot(iris_paris_joined_conso$geometry)
c2018=filter(iris_paris_joined_conso,Ann�e=="2018")
c2018$Consommation.annuelle.moyenne.par.logement.de.l.adresse..MWh.= as.numeric(sub(",",".",c2018$Consommation.annuelle.moyenne.par.logement.de.l.adresse..MWh.,fixed=TRUE))
c2018$Consommation.annuelle.totale.de.l.adresse..MWh.= as.numeric(sub(",",".",c2018$Consommation.annuelle.totale.de.l.adresse..MWh.,fixed=TRUE))

c2018_agr=aggregate(c2018[iden], list(c2018$CODE_IRIS), sum)
names(c2018_agr)[1] <- "code_iris" ;names(c2018_agr)[2] <- "sum_conso_annuelle_moyenne_iris" ; names(c2018_agr)[3]<-"sum_conso_annuelle_totale_iris";names(c2018_agr)[4]<-"nb_logements_totale_iris"

c2019=filter(iris_paris_joined_conso,Ann�e=="2019")
c2019$Consommation.annuelle.moyenne.par.logement.de.l.adresse..MWh.= as.numeric(sub(",",".",c2019$Consommation.annuelle.moyenne.par.logement.de.l.adresse..MWh.,fixed=TRUE))
c2019$Consommation.annuelle.totale.de.l.adresse..MWh.= as.numeric(sub(",",".",c2019$Consommation.annuelle.totale.de.l.adresse..MWh.,fixed=TRUE))

c2019_agr=aggregate(c2019[iden], list(c2019$CODE_IRIS), sum)
names(c2019_agr)[1] <- "code_iris" ;names(c2019_agr)[2] <- "sum_conso_annuelle_moyenne_iris" ;names(c2019_agr)[3]<-"sum_conso_annuelle_totale_iris"; names(c2019_agr)[4]<-"nb_logements_totale_iris"

c2020=filter(iris_paris_joined_conso,Ann�e=="2020")
c2020$Consommation.annuelle.moyenne.par.logement.de.l.adresse..MWh.= as.numeric(sub(",",".",c2020$Consommation.annuelle.moyenne.par.logement.de.l.adresse..MWh.,fixed=TRUE))
c2020$Consommation.annuelle.totale.de.l.adresse..MWh.= as.numeric(sub(",",".",c2020$Consommation.annuelle.totale.de.l.adresse..MWh.,fixed=TRUE))

c2020_agr=aggregate(c2020[iden], list(c2020$CODE_IRIS), sum)
names(c2020_agr)[1] <- "code_iris" ;names(c2020_agr)[2] <- "sum_conso_annuelle_moyenne_iris" ;names(c2020_agr)[3]<-"sum_conso_annuelle_totale_iris"; names(c2020_agr)[4]<-"nb_logements_totale_iris"

# Analyse univari�e #
moyenne_de_conso_annuelle_totale_des_iris2018<-mean(c2018_agr$sum_conso_annuelle_totale_iris)
moyenne_de_conso_annuelle_totale_des_iris2019<-mean(c2019_agr$sum_conso_annuelle_totale_iris)
moyenne_de_conso_annuelle_totale_des_iris2020<-mean(c2020_agr$sum_conso_annuelle_totale_iris)

variance_de_conso_annuelle_totale_des_iris2018<-var(c2018_agr$sum_conso_annuelle_totale_iris)
variance_de_conso_annuelle_totale_des_iris2019<-var(c2019_agr$sum_conso_annuelle_totale_iris)
variance_de_conso_annuelle_totale_des_iris2020<-var(c2020_agr$sum_conso_annuelle_totale_iris)

# Analyse bivari�e - Tests de Correlation #
cor.test(c2018_agr$sum_conso_annuelle_totale_iris,c2018_agr$nb_logements_totale_iris)#, method=c("pearson", "kendall", "spearman"))#
cor.test(c2019_agr$sum_conso_annuelle_totale_iris,c2019_agr$nb_logements_totale_iris)# Conso electrique % nb de logements (qui est proportionnelle au nombre d'habitant)
cor.test(c2020_agr$sum_conso_annuelle_totale_iris,c2020_agr$nb_logements_totale_iris)#

# Test de corr�lation de la conso �lectrique entre deux ann�es successives #
cor.test(c2019_agr$sum_conso_annuelle_totale_iris,c2018_agr$sum_conso_annuelle_totale_iris)
cor.test(c2020_agr$sum_conso_annuelle_totale_iris,c2019_agr$sum_conso_annuelle_totale_iris)

# ... #
c2018_agr$annee<-rep(2018,nrow(c2018_agr))
c2019_agr$annee<-rep(2019,nrow(c2019_agr))
c2020_agr$annee<-rep(2020,nrow(c2020_agr))
conso_regrouped<-rbind(c2018_agr,c2019_agr,c2020_agr)

# Modelling and prediction # 


ggplot() +
  geom_sf(data = c2018_agr, aes(fill = sum_conso_annuelle_totale_iris )) 


ggplot() +
  geom_sf(data = c2018_agr, aes(fill = sum_conso_annuelle_moyenne_iris )) 
