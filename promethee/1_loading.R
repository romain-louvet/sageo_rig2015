# AUthor : Romain Louvet
# Date   : 08/10/2015
#
# Load and clean data.

#############
# LIBRARIES #
#############
library(dplyr) # %>%
library(data.table) # fread
library(sp) # spatial objects spsample()
library(rgdal) # readOGR (open and read/write shape)
library(GISTools) # poly.counts()

#############
# FUNCTIONS #
#############
source('functions.R')

##############
# INPUT DATA #
##############
###
# Promethee database
###
## origin data: liste_incendies_du_20_04_2015.csv data set,
## fires from 1997 to 2013
## downloaded on http://www.promethee.com

## colClasses & read csv
class <- c(rep("character",10),"double")
fires <- fread("1-inputdata/promethee97_13.csv",sep=";", colClasses=class)

## change communes ID for consistency with geofla 2014 shape file
### 05138: Saint disdier, merged with "DEVOLUY" 05139
### 05042: La Cluze, merged with "DEVOLUY" 05139
### 05002 : Agnières-en-Dévoluy, merged with "DEVOLUY" 05139 
### 05020 : Bénévent et charbillac, merged with Saint-Bonnet-en-Champsaur  05132
### 83999 : military camp of Campjuers, mainly within Montferrat 83082 

fires$Code_INSEE[fires$Code_INSEE=="05138"] <- "05139"
fires$Code_INSEE[fires$Code_INSEE=="05042"] <- "05139"
fires$Code_INSEE[fires$Code_INSEE=="05002"] <- "05139"
fires$Code_INSEE[fires$Code_INSEE=="05020"] <- "05132"
fires$Code_INSEE[fires$Code_INSEE=="83999"] <- "83082" 

## sum fire surfaces, count number of fires
## write result as new tables
Surface_parcourue_m2<-fires$Surface_parcourue_m2
Code_INSEE<-fires$Code_INSEE

sumfire <- aggregate(Surface_parcourue_m2 ~ Code_INSEE, fires, FUN=sum)
countfire <- aggregate(Surface_parcourue_m2 ~ Code_INSEE, fires, FUN=length)
names(countfire)[2]<-"count"

write.table(sumfire, file = "2-outputdata/table/sumfire1997_2013.csv", row.names = FALSE, sep = ";")
write.table(countfire, file = "2-outputdata/table/countfire1997_2013.csv", row.names = FALSE, sep = ";")

###
# other data
###
## CORINE LAND COVER
### origin data: stats_clc_commune_niveau_3_RGF.7z
### downloaded on 
### http://www.statistiques.developpement-durable.gouv.fr/donnees-ligne/t/donnees.html?tx_ttnews[tt_news]=24275&cHash=fc83c4f9bef57fb40874fde73387da4c
### mean of CLC 2000 revised & CLC 2006,
### the land cover total was calculated. These land cover variables were selected:
#### 21 arable land
#### 22 permanent crops, summed with 241 Annual crops associated with permanent crops
#### & 244 Agro-forestry areas
#### 243 friches agricolesLand principally occupied by agriculture, with significant areas of natural vegetation
#### 31 forests
#### 322 Moors and heathland, summed with 323 Sclerophyllous vegetation and 324 Transitional woodland/shrub
#### 231 Pastures summed with 321 Natural grassland
  
class <- c("character",rep("numeric",7))
clc <- fread("1-inputdata/stats_clc_commune_niveau_3_RGF_00_06.csv",sep=";",header=TRUE, colClasses=class)

## ROUTES 500, 2012
### downloaded on http://professionnels.ign.fr/route500
### origin data: ROAD500_2012.7z & COMMUNE_2014.7Z
### road density, route 500 de 2012
### densité féroviaire, route 500 2012
### both with ArcGIS and linear_density.py
class <- c("character","double","double")
road <- fread("1-inputdata/routes_summ2012.csv",sep=";",header=TRUE, colClasses=class)
rail <- fread("1-inputdata/fer_summ2012.csv",sep=";",header=TRUE, colClasses=class)

## POPULATION
### 1999-1997-2012 mean population
### origin data: HIST_POP_COM_RP12.7z
### downloaded on: http://www.insee.fr/fr/ppp/bases-de-donnees/recensement/populations-legales/
### downloading link: http://www.insee.fr/fr/ppp/bases-de-donnees/recensement/populations-legales/pages2014/zip/HIST_POP_COM_RP12.zip

class <- c("character","double")
pop <- fread("1-inputdata/HIST_POP_COM_RP12_99_97_12.csv",sep=";",header=TRUE, colClasses=class)

## TOURISME 2013
### number of beds for tourism, INSEE
### cf. "base-cc-tourisme-13_2.7z", origin data, for a definition of beds for tourism
### downloaded on : http://www.insee.fr/fr/themes/detail.asp?reg_id=99&ref_id=base-cc-tourisme
### downloading link : http://www.insee.fr/fr/ppp/bases-de-donnees/recensement/populations-legales/pages2014/zip/HIST_POP_COM_RP12.zip
tour <- fread("1-inputdata/tourisme2013.csv",sep=";",header=TRUE, colClasses=class)

## CHOMAGE, 1999, 2006, 2011
### 1999, 2006, 2011, mean of unemployment
### origin data : pop-act2554-empl-sexe-cd-6811.7z
### downloaded on : http://www.insee.fr/fr/themes/detail.asp?reg_id=99&ref_id=pop-act-csp-dipl
### downloading link : http://www.insee.fr/fr/ppp/bases-de-donnees/donnees-detaillees/pop-act-csp-dipl/pop-act2554-empl-sexe-cd-6811.zip
class <- c("character",rep("numeric",10))
chom <- fread("1-inputdata/pop_act99_06_11.csv",sep=";",header=TRUE, colClasses=class)

## SAU
### origin data: recensementagri2010.7z
### agricultural area and livestock evolution from 1988 to 2010
### downloaded on : http://agreste.agriculture.gouv.fr/enquetes/structure-des-exploitations-964/recensement-agricole-2010/resultats-donnees-chiffrees/
### downloading link : http://agreste.agriculture.gouv.fr/IMG/zip/Donnees_principales__commune.zip
class <- c("character",rep("double",8))
agri <- fread("1-inputdata/SAU88_10.csv",sep=";",header=TRUE, colClasses=class)

###
# shape file
###
## promethee map
### origin data : COMMUNE_2014.7Z
### downloaded on: http://professionnels.ign.fr/geofla
### and epcicom2014.7z
### from : http://www.collectivites-locales.gouv.fr/liste-et-composition-des-epci-a-fiscalite-propre
### communes covered by the promethée database extent were selected
### cf. http://www.promethee.com/map.php?map=nombres&t=1444055611

map <- readOGR(dsn="1-inputdata/SHP", layer="map_promethee")

### create canton's code and arrondissement's code
map@data$CANTON <- paste(map@data$CODE_DEP,map@data$CODE_CANT,sep="")
map@data$ARRON <- paste(map@data$CODE_DEP,map@data$CODE_ARR,sep="")

### subset
### calc "KM2" field (areas in kilometers square)
map@data <- data.frame(CODE_COM=map@data$INSEE_COM,EPCI=map@data$siren_EPCI,CODE_CANT=map@data$CANTON,
                       CODE_ARR=map@data$ARRON,CODE_DEPT=map@data$CODE_DEP,
                       CODE_REG=map@data$CODE_REG,KM2=map@data$SUPERFICIE/1000000
                       ,stringsAsFactors=FALSE)
map@data <- conv_char(map@data,list("CODE_COM","EPCI","CODE_CANT","CODE_ARR","CODE_DEPT","CODE_REG"))

### join by attribute
map@data <- jointure(map, sumfire, map@data$CODE_COM,sumfire$Code_INSEE)
map@data <- jointure(map, countfire, map@data$CODE_COM,countfire$Code_INSEE)
map@data <- jointure(map, clc, map@data$CODE_COM,clc$NUM_COM)
map@data <- jointure(map, road, map@data$CODE_COM,road$INSEE_COM)
map@data <- jointure(map, rail, map@data$CODE_COM,rail$INSEE_COM)
map@data <- jointure(map, pop, map@data$CODE_COM,pop$DEPCOM)
map@data <- jointure(map, chom, map@data$CODE_COM,chom$INSEE_COM)
map@data <- jointure(map, tour, map@data$CODE_COM,tour$CODGEO)
map@data <- jointure(map, agri, map@data$CODE_COM,agri$CODE_GEO)

# replace NA by 0
map@data$count[is.na(map@data$count)] <- 0
map@data$Surface_parcourue_m2[is.na(map@data$Surface_parcourue_m2)] <- 0
map@data$FER_KM[is.na(map@data$FER_KM)] <- 0
map@data$FER_kmkm2[is.na(map@data$FER_kmkm2)] <- 0

############
# controls #
############
# is there stil NAs?
# result: NA only for target join variables (Code_INSEE=130, Code_INSEE.1=130, INSEE_COM=2817)
# which is normal since not every commune has fire data or rail density
print('NAs?')
for(name in names(map@data)){
  test <- sum(is.na(map@data[,name]))
  if(test>0){
    print(paste(name,test,sep=" : "))
  }
}

# same values in origin and target table after joining them?
# only for variable with origin table with same geo extent as target table
print("")
print("Same sums?")
tab <- list(sumfire,countfire,road,rail)
var <- c("Surface_parcourue_m2","count",'ROAD_KM','FER_KM')
for(i in 1:length(tab)){
  print(paste(var[[i]],
    sum(as.numeric(data.frame(tab[[i]])[,var[[i]]]))==sum(map@data[,var[i]]),
    sep=' : ')
    )
}