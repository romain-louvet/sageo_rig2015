# ---------------------------------------------------------------------------
# test_R_alea.R
# Created on: 2015-06-11 
# Author : Romain Louvet, PhD student
# Contact : romain.louvet@alumni.univ-avignon.fr
# Description: 
# This script must be executed with R. 
# It was tested with R 3.1.2 and Rstudio 0.98.1091 
#
# Create random points based on a table values (one point per line) and within one given shape spatial extent.
# Used in order to spatially randomized variables.
# ---------------------------------------------------------------------------

## import libraries
library(rgdal) # readOGR (open and read/write shape)
library(sp) # spatial objects
library(data.table) #fread

# START: execution time
start <- Sys.time()

# FUNCTION
### POINTS ALEA : create alea point within polygon and merge attributes
# poly = polygon shape
# data = data frame of points to randomized (used to calculate n points=> nrow)
# iteal = number of random points to generate
# aleapara = radom, regular, etc. cf ?spsample
# file = .Rdata file for saving results
points_alea <- function(poly,data,iteal,aleapara="random",sfile){
# list of random points shapes
  alealiste <- list()
# loop
  for(ite in 1:iteal){
    n <- nrow(data)
# random points
    alea <- spsample(poly,n,aleapara)
# add values from table to randomize
    alea <- SpatialPointsDataFrame(
      coords=alea@coords,
      data=data.frame(ID=c(1:n),data),
      proj4string =alea@proj4string,bbox = alea@bbox)
    alealiste <- c(alealiste, alea) 
  }
  save(alealiste,file=sfile)
  return(alealiste)
}

# parameters
class <- c(rep("character",10),"double")
tab <- fread("promethee1997_2013.csv",sep=";", colClasses=class)
tab <- data.frame(SURFM2=tab$Surface_parcourue_m2)

# process
## spatial extent
map <- readOGR(dsn="SHP", layer="commune_base")
## number of random points shapes
nalea <- 2

## function call
testalea <- points_alea(map,tab,nalea,"random","pointalea.Rdata")

## display
for(i in 1:nalea){plot(testalea[[i]])}

# STOP : execution time
end <- Sys.time()
print(end - start)