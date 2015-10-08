# AUthor : Romain Louvet
# Date   : 08/10/2015
#
# use aggr_shp_list function to aggregate (merge according to variable)
# polygons from the most disaggregated level to the most aggregated level.
# Here from (french political boundaries) communes to régions (and
# cantons, EPCI, arrondissements, départements)
# Then calculate again some variables (such as rates).
# The result is the "list_scales" values, large list of shape objects, each
# element of the list is a scale level.

###############
# aggregation #
###############

# variable names list
var <- list("Surface_parcourue_m2","count","KM2","CLC_TTL0006","CLC21_0006",
            "CLC22_241_244_0006","CLC243_0006","CLC31_0006","CLC322_3_4_0006","CLC231_321_0006",
            "FER_KM","ROAD_KM","POP_MOYENNE99_07_12","CHOM1999","CHOM2006","CHOM2011","POPACT1999",
            "POPACT2006","POPACT2011","SAU1988_ha","SAU2010_ha","UGB1988","UGB2010","LITSTOUR2013")

# scale level names
scalenames <- list("CODE_COM","CODE_CANT","EPCI","CODE_ARR","CODE_DEPT","CODE_REG")

# output shape file names
sorties <- list("prom_comm","prom_canton","prom_EPCI","prom_arron","prom_dep","prom_region")

# use function
# only if .Rdata list file doesn't exist yet
# else load Rdata output
if(!file.exists('./2-outputdata/prom_scales.Rdata')){
  list_scales <- aggr_shp_list(map,scalenames,var,"./2-outputdata/SHP",sorties)
  
  ##########################
  # calc ratio, indicators #
  ##########################
  # calc densities and add log variables
  for(i in 1:length(list_scales)){
    # variables dépendantes X
    list_scales[[i]]@data$countkm2 <- round(list_scales[[i]]@data$count/list_scales[[i]]@data$KM2,2)
    list_scales[[i]]@data$firesurfm2_km2<- round(list_scales[[i]]@data$Surface_parcourue_m2/list_scales[[i]]@data$KM2,2)
    list_scales[[i]]@data$surf_count<- round(list_scales[[i]]@data$Surface_parcourue_m2/list_scales[[i]]@data$count,2)
    # for surf_count, if divided by zero, replace NA
    list_scales[[i]]@data[is.na(list_scales[[i]]@data$surf_count),'surf_count']<- 0
    
    # log (base 1) dependent variable (X, to explain)
    list_scales[[i]]@data$logcount <- log(list_scales[[i]]@data$count+1)
    list_scales[[i]]@data$logSurface <- log(list_scales[[i]]@data$Surface_parcourue_m2+1)
    list_scales[[i]]@data$logcountkm2 <- log(list_scales[[i]]@data$countkm2+1)
    list_scales[[i]]@data$logfiresurfm2_km2 <- log(list_scales[[i]]@data$firesurfm2_km2+1)
    list_scales[[i]]@data$logsurf_count <- log(list_scales[[i]]@data$surf_count+1)
    
    # independant variables (Y, explicanatory)
    list_scales[[i]]@data$POPKM2 <- round(list_scales[[i]]@data$POP_MOYENNE99_07_12/list_scales[[i]]@data$KM2,1)
    list_scales[[i]]@data$ROADKM2 <- round(list_scales[[i]]@data$ROAD_KM/list_scales[[i]]@data$KM2,2)
    list_scales[[i]]@data$RAILKM2 <- round(list_scales[[i]]@data$FER_KM/list_scales[[i]]@data$KM2,2)
    
    list_scales[[i]]@data$TXCHOM <- ((list_scales[[i]]@data$CHOM1999/list_scales[[i]]@data$POPACT1999)*100 +
                                       (list_scales[[i]]@data$CHOM2006/list_scales[[i]]@data$POPACT2006)*100 +
                                       (list_scales[[i]]@data$CHOM2011/list_scales[[i]]@data$POPACT2011)*100)/3
    list_scales[[i]]@data$TXCHOM[is.na(list_scales[[i]]@data$TXCHOM)] <- 0
    
    list_scales[[i]]@data$LITSTOUR_HAB <- round(list_scales[[i]]@data$LITSTOUR2013/list_scales[[i]]@data$POP_MOYENNE99_07_12,1)
    list_scales[[i]]@data$LITSTOUR_KM2 <- round(list_scales[[i]]@data$LITSTOUR2013/list_scales[[i]]@data$KM2,1)
    
    list_scales[[i]]@data$UGB88_10 <- round((list_scales[[i]]@data$UGB2010-list_scales[[i]]@data$UGB1988)/list_scales[[i]]@data$UGB1988*100,2)
    list_scales[[i]]@data$SAU88_10 <- round((list_scales[[i]]@data$SAU2010_ha-list_scales[[i]]@data$SAU1988_ha)/list_scales[[i]]@data$SAU1988_ha*100,2)
    list_scales[[i]]@data$UGB88_10[list_scales[[i]]@data$UGB2010>0&list_scales[[i]]@data$UGB1988==0] <- 100 
    list_scales[[i]]@data$SAU88_10[list_scales[[i]]@data$SAU2010>0&list_scales[[i]]@data$SAU1988==0] <- 100
    list_scales[[i]]@data$SAU88_10[list_scales[[i]]@data$SAU2010==0&list_scales[[i]]@data$SAU1988==0] <- 0
    list_scales[[i]]@data$UGB88_10[list_scales[[i]]@data$UGB2010==0&list_scales[[i]]@data$UGB1988==0] <- 0
    
    list_scales[[i]]@data$PRCT_CLC21 <- round(list_scales[[i]]@data$CLC21_0006/list_scales[[i]]@data$CLC_TTL0006*100,1)
    list_scales[[i]]@data$PRCT_CLC22_241_244 <- round(list_scales[[i]]@data$CLC22_241_244_0006/list_scales[[i]]@data$CLC_TTL0006*100,1)
    list_scales[[i]]@data$PRCT_CLC243 <- round(list_scales[[i]]@data$CLC243_0006/list_scales[[i]]@data$CLC_TTL0006*100,1)
    list_scales[[i]]@data$PRCT_CLC31 <- round(list_scales[[i]]@data$CLC31_0006/list_scales[[i]]@data$CLC_TTL0006*100,1)
    list_scales[[i]]@data$PRCT_CLC322_3_4 <- round(list_scales[[i]]@data$CLC322_3_4_0006/list_scales[[i]]@data$CLC_TTL0006*100,1)
    list_scales[[i]]@data$PRCT_CLC231_321 <- round(list_scales[[i]]@data$CLC231_321_0006/list_scales[[i]]@data$CLC_TTL0006*100,1)   
    # two communes have no CLC areas (06121, 34192), so PRCT = NA
    list_scales[[i]]@data[is.na(list_scales[[i]]@data$PRCT_CLC21),'PRCT_CLC21']<- 0
    list_scales[[i]]@data[is.na(list_scales[[i]]@data$PRCT_CLC22_241_244),'PRCT_CLC22_241_244']<- 0
    list_scales[[i]]@data[is.na(list_scales[[i]]@data$PRCT_CLC243),'PRCT_CLC243']<- 0
    list_scales[[i]]@data[is.na(list_scales[[i]]@data$PRCT_CLC31),'PRCT_CLC31']<- 0
    list_scales[[i]]@data[is.na(list_scales[[i]]@data$PRCT_CLC322_3_4),'PRCT_CLC322_3_4']<- 0
    list_scales[[i]]@data[is.na(list_scales[[i]]@data$PRCT_CLC231_321),'PRCT_CLC231_321']<- 0
  }
  
  ############
  # controls #
  ############
  # test if there is a difference between variable sum from
  # input and aggr_shp_list function output
  # RESULT: no problem, there is a difference for one variable, only an error of 5.82e-11
  print("Control 1: sum differences")
  for(scale in list_scales){
    for(v in var){
      if(!(sum(scale@data[,v])==sum(map@data[,v]))){
        print(names(scale@data)[1])
        print(paste(sum(scale@data[,v])==sum(map@data[,v]),v))
        print('Difference =')
        print(sum(scale@data[,v])-sum(map@data[,v]))
        print('')
      }
    }
  }
  
  # test if there is still NA in list_scales
  # RESULT: no
  print("NA?")
  for(i in 1:6){
    for(v in names(list_scales[[i]]@data)){
      if(sum(is.na(list_scales[[i]]@data[,v])>0)){
        print(scalenames[[i]])
        print(paste(sum(is.na(list_scales[[i]]@data[,v])),v))
      }
    }
  }
  
  #########################
  # save results as Rdata #
  #########################
  save(list_scales,file="./2-outputdata/prom_scales.Rdata")

  }else{
  # load file
  load('./2-outputdata/prom_scales.Rdata')
}