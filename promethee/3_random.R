# AUthor : Romain Louvet
# Date   : 08/10/2015
#
# Create spatially randomized points with same values as "fires" data
# In other words, it randomized fires location.
# Then, it will join by location the points for each scale level from
# list_scales.

########################
# create random points #
########################

fires2 <- data.frame(SURFM2=fires$Surface_parcourue_m2)

# call "points_random" function, create 100 spatially randomized points within "map"
# extent and with fires2 values (take +- 25 min to generate)
if(file.exists("./2-outputdata/pointrandom100.Rdata")){
  load("./2-outputdata/pointrandom100.Rdata")
}else{
  randomliste <- points_random(map,fires2,100,"random")
  save(randomliste,file="./2-outputdata/pointrandom100.Rdata")
}

######################
# join random points #
######################

# (take +- 60 min to generate)
if(file.exists("./2-outputdata/pointrandom100.Rdata")){
  load("./2-outputdata/joinrandom100.Rdata")
}else{
  joinrandom <- over_join_random(randomliste,list_scales,scalenames,"SURFM2")
  save(joinrandom,file="./2-outputdata/joinrandom100.Rdata")
}