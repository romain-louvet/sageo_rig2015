# ---------------------------------------------------------------------------
# test_R_aggre.R
# Created on: 2015-06-11 
# Author : Romain Louvet, PhD student
# Contact : romain.louvet@alumni.univ-avignon.fr
# Description: 
# This script must be executed with R. 
# It was tested with R 3.1.2 and Rstudio 0.98.1091 
#
# Dissolve shape according to a list of aggregation fields and
# sum values of a list of fields for each aggregation.
# ---------------------------------------------------------------------------

## import libraries
library(rgdal) # readOGR (open and read/write shape)
library(rgeos) # aggregate (spatial object)
library(sp) # spatial objects

# START : execution time
start <- Sys.time()

# FUNCTION
### Agregation shp, list ; return a list of spatial object (and create corresponding shapefiles)
# data : input shape file, spatial object name
# IDS_col : IDS column names, character, list
# values_col : values column names, character, list
# folder : output folder, character
# names : output shp file name, character, list
# sfile : .Rdata file for saving the results
aggr_shp_list <- function(data,IDS_col,values_col,folder,names,sfile){
# error
  if(length(IDS_col)!=length(names)){stop("Must have same IDs columns and names number")}
# result list
  result <- list()
  
  for(i in 1:length(IDS_col)){
    name <- names[[i]]
# if already exists: reads
    if(file.exists(paste("./",folder,"/",name,".shp",sep=""))){
      data1 <- readOGR(dsn="SHP", layer=name)
      
      nc <- ncol(data1@data)
      for(j in 1:nc){
        if(j==1){
          names(data1@data)[j] <- IDS_col[[i]]
        }else{
          names(data1@data)[j] <- values_col[[j-1]]
        }
      }
      
# else: creates
    }else{
# aggregate and dissolve geometry according to ID
      ID <- IDS_col[[i]]
      data0 <- aggregate(data,by=list(data@data[,ID]),dissolve=TRUE)
      
# reformat data.frame...
      data1 <- data0
#... ID
      data1@data <- data.frame(ID=data0@data$Group.1)
#... values
      for(j in 1:length(values_col)){
        value <- values_col[[j]]
        calc <- aggregate(data@data[,value], by=list(data@data[,ID]), FUN=sum)$x
        data1@data <- data.frame(data1@data,x=calc)
        names(data1@data)[names(data1@data)=="x"] <- value
        names(data1@data)[names(data1@data)=="ID"] <- ID
      }
      
# create a new shapefile for each scale level  
      writeOGR(data1,folder,name,driver="ESRI Shapefile")      
    }
# add to result list
    result <- c(result,data1)
  }
# return a list, each element has a name which is
# the scale level ("names" option of this function)
  agg_result <- result
  save(agg_result,file=sfile)
  return(agg_result)
}

# parameters
## scale names
codes <- list("CODE_CO","CODE_CA","EPCI","CODE_A","CODE_D","CODE_R")
## fields to sum
fields <- list("KM2","count","ROAD_K","POP_M","CHOM1")
## output names
names <- list("prom_com","prom_canton","prom_EPCI","prom_arron","prom_dep","prom_region")
##set working directory
setwd("C:/Users/Romain Louvet/Desktop/test_comparatif_outils")

# PROCESS
## open base shape
map <- readOGR(dsn="SHP", layer="commune_base")
## call function
list_scales <- aggr_shp_list(map,codes,fields,"results",names,'prom_scales.Rdata')

## display
for(i in 1:length(codes)){plot(list_scales[[i]])}

# STOP : execution time
end <- Sys.time()
print(end-start)