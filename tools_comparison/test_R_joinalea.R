# ---------------------------------------------------------------------------
# test_R_joinalea.R
# Created on: 2015-06-11 
# Author : Romain Louvet, PhD student
# Contact : romain.louvet@alumni.univ-avignon.fr
# Description: 
# This script must be executed with R. 
# It was tested with R 3.1.2 and Rstudio 0.98.1091 
#
# Join random points created by test_arcgis_alea.py to polygons created by test_arcgis_aggre.py
# and sum random points values by polygon. Output is a dataframe, not a shape.
# ---------------------------------------------------------------------------

## import libraries
library(sp) # spatial objects
library(rgdal) # readOGR (open and read/write shape)
library(data.table) #fread

# START: execution time
start <- Sys.time()

# FUNCTION
### Join by attributes
# - works only with characters
# - target must not be @data
# - apply to @data
# - column complete name
jointure <- function(target,origin,col_target,col_origin){
  if(class(col_target)!="character"){stop("Target column has to be character")}
  if(class(col_origin)!="character"){stop("Origin column has to be character")}
  target <- data.frame(target@data, origin[match(col_target,col_origin),])
}

### join by location
# alealiste = result from point alea function (origin)
# polylist = list of spatial objects, polygons (target) to join the points (output listscales)
# idpolyliste = list of id field in polygons (target)
# fieldaleaY = field from alealiste to add to join by location
# sfile : .Rdata file for saving the results
#
# output : a list of list of dataframe
over_join <- function(alealiste,polyliste,idpolyliste,fieldaleaY,sfile){
  idi <- 0
  result <- list()
  for(poly in polyliste){
# idpoly index
    idi <- idi + 1
# id alea
    ida <- 0
    result0 <- list()
    
# loop
    for(alea in alealiste){
# id alea
      ida <- ida + 1
      
      idpoly <- idpolyliste[[idi]]
# jointure par localisation
      joinloc <- over(alea,poly[,idpoly])
      joinloc <- data.frame(joinloc,attribut=alea@data[,fieldaleaY])
      
# sum alea values (for point counts, attribut must be "1" for each row)
      sum_alea <- aggregate(joinloc[,"attribut"] ~ joinloc[,idpoly], joinloc, FUN=sum)
      
# jointure attributaire
      sum_alea[,"joinloc[, idpoly]"] <- as.character(sum_alea[,"joinloc[, idpoly]"]) 
      poly@data[,idpoly] <- as.character(poly@data[,idpoly])
      jo <- jointure(poly,sum_alea,poly@data[,idpoly],sum_alea[,"joinloc[, idpoly]"])
      
      data <- data.frame(ID=jo$joinloc...idpoly.,SUM=jo$joinloc....attribut..)
      
# count
      count_alea <- aggregate(joinloc[,"attribut"] ~ joinloc[,idpoly], joinloc, FUN=length)
      count_alea[,"joinloc[, idpoly]"] <- as.character(count_alea[,"joinloc[, idpoly]"]) 
      jo <- jointure(poly,count_alea,poly@data[,idpoly],count_alea[,"joinloc[, idpoly]"])
      
      data <- data.frame(data,COUNT=jo$joinloc....attribut..)
      
# result
      result0[[ida]] <- data
    }
    result[[idi]] <- result0
  }
#results: list of list
# first list: each scale level
# second list within first list: each alea by scale level
  joinalearesult <- result
  save(joinalearesult,file=sfile)
  return(joinalearesult)
}

# parameters
## output from test_R_aggregation.R
## name "agg_result"
load("prom_scales.Rdata")
## output from test_R_alea.R
## name "alealiste"
load("pointalea.Rdata")

# process
## scale names
codes <- list("CODE_CO","CODE_CA","EPCI","CODE_A","CODE_D","CODE_R")
joinalea <- over_join(alealiste,agg_result,codes,"SURFM2",'joinalea.Rdata')

# STOP : execution time
end <- Sys.time()
print(end - start)