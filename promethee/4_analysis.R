# AUthor : Romain Louvet
# Date   : 05/10/2015
#
# EXPLORATORY ANALYSES
# plots are svg files. Html files are created in order to open several svg files
# (for example histogramms for one variable for each scale level)

# all variable names (dependent and explanatory)
varnames <- list("count","Surface_parcourue_m2","surf_count","countkm2","firesurfm2_km2","logcount","logSurface",
                 "logcountkm2","logsurf_count","logfiresurfm2_km2",
                 "POPKM2","ROADKM2","RAILKM2","TXCHOM","LITSTOUR_HAB","LITSTOUR_KM2","UGB88_10","SAU88_10",
                 "PRCT_CLC21","PRCT_CLC22_241_244","PRCT_CLC243","PRCT_CLC31","PRCT_CLC322_3_4","PRCT_CLC231_321")

##########################
# descriptive statistics #
##########################

# desc stat summary
desc_stat <- list()

for(i in 1:length(varnames)){
  desc_stat[[i]] <- summary_list(list_scales,varnames[[i]],"KM2",paste("./2-outputdata/table/",varnames,"_summ",sep="")[[i]],scalenames,ro=4)
}

# desc stat variations
rows = c("MEAN","MEDIAN","SD")
var_stat_standdev<- desc_stat_var(desc_stat,varnames,rows,paste("./2-outputdata/table/","descstat_standdev",sep=""),4)

#########
# PLOTS #
#########

# MAPS
for(var in varnames){map_list(list_scales,var,paste("./2-outputdata/MAP/","MAP_",var,sep=""),
                              discreti="quantile",nclc=5,lgdsize=0.5)}
## html plot maps
for(var in varnames){
  html_plot_var(paste('MAP_',var,sep=''),"2-outputdata/MAP",500,500,6)
}
for(i in 1:6){
  html_plot_index(paste('MAP_',varnames,sep=''),paste("MAP_",scalenames[i],sep=""),i,"2-outputdata/MAP",400,400)
}

# BASE PLOT : variables stat through scales
plot_list(desc_stat,scalenames,varnames,"./2-outputdata/PLOT/")
plot_list(desc_stat,scalenames,varnames,"./2-outputdata/PLOT/",TRUE)
## HTML plot var through scales
for(var in varnames){html_plot_var(paste("plot",var,sep="_"),"2-outputdata/PLOT",400,400,12)}
for(i in 1:12){
  html_plot_index(paste("plot",varnames,sep="_"),paste("plot_",colnames(desc_stat[[1]])[i],sep=''),i,"2-outputdata/PLOT",400,400)
}   

for(var in varnames){html_plot_var(paste("plotlog",var,sep="_"),"2-outputdata/PLOT",400,400,12)}
for(i in 1:12){
  html_plot_index(paste("plotlog",varnames,sep="_"),paste("plotlog_",colnames(desc_stat[[1]])[i],sep=''),i,"2-outputdata/PLOT",400,400)
}   

# HISTOGRAMMS
hist_list(list_scales,varnames,scalenames,folder="./2-outputdata/PLOT/")
## html plot histo
for(var in varnames){html_plot_var(paste('His',var,sep="_"),"./2-outputdata/PLOT",500,500,6)}
for(i in 1:length(scalenames)){
  html_plot_index(paste("His",varnames,sep="_"),paste("His",scalenames[i],sep='_'),i,"2-outputdata/PLOT",400,400)
}

# BOXPLOT
for(var in varnames){boxplot_list(list_scales,var,FALSE,scalenames,2,"./2-outputdata/PLOT/")}
for(i in 1:length(list_scales)){
  boxplot_var(list_scales[[i]],scalenames[i],varnames,out=FALSE,la=2,folder="./2-outputdata/PLOT/")
}

for(i in 1:length(list_scales)){
  boxplot_var(list_scales[[i]],scalenames[i],varnames[3:8],
              out=FALSE,la=2,folder="./2-outputdata/PLOT/",ext="1")
}

## html plot boxplot
html_plot(paste("bxplot_",varnames,sep=""),"bxplot_var","./2-outputdata/PLOT",500,500)
html_plot(paste("bxplot_",scalenames,sep=""),"bxplot_scale","./2-outputdata/PLOT",500,500)
html_plot(paste("bxplot_",scalenames,1,sep=""),"bxplot_scale1","./2-outputdata/PLOT",500,500)

########################
# correlation analysis #
########################

# R / scatter-plot
# nb : dependent variable (to explain / Y) is log transformed
# nb : subset count or surf fire > 0 (try to explain where fire occurs)

# depedent var
ylist <- list("count","Surface_parcourue_m2","countkm2","firesurfm2_km2","surf_count","logcount",
              "logsurf_count","logSurface","logcountkm2","logfiresurfm2_km2")

# explanatory var // covariates
xlist <- list("POPKM2","ROADKM2","RAILKM2","TXCHOM","LITSTOUR_HAB","LITSTOUR_KM2","UGB88_10","SAU88_10",
              "PRCT_CLC21","PRCT_CLC22_241_244","PRCT_CLC243","PRCT_CLC31","PRCT_CLC322_3_4","PRCT_CLC231_321")

for(i in 1:length(list_scales)){
  scatterplot_corrlist(ylist,xlist,FALSE,list_scales[[i]]@data,scalenames[[i]],"./2-outputdata/PLOT/")
}

# HTML by y xvar
for(xvar in xlist){
  plotname <- c()
  for(i in 1:length(ylist)){
    plotname<-c(plotname,paste("XY",scalenames,xvar,ylist[[i]],sep=""))
  }
  
  for(i in 1:length(list_scales)){
    html_plot(plotname,paste("XY",xvar,sep=""),"./2-outputdata/PLOT",500,500)
  }
}

# HTML by scale
for(scale in scalenames){
  plotname <- c()
  for(i in 1:length(xlist)){
    plotname<-c(plotname,paste("XY",scale,xlist[[i]],ylist,sep=""))
  }
  
  for(i in 1:length(list_scales)){
    html_plot(plotname,paste("XY",scale,sep=""),"./2-outputdata/PLOT",500,500)
  }
}

### R, R2, p-value through scales
# log transformation : if X and Y are transformed
# semi log transformation : if only Y (dependent variable) is tranformed

varcorrliste <- plot_corrlist(ylist,xlist,list_scales,scalenames,
                              "./2-outputdata/PLOT/","./2-outputdata/table/",ro=0,"KM2",logoption=FALSE)

for(yname in ylist){
  html_plot(paste("RR2Pval",xlist,yname,sep=""),paste("RR2Pval",yname,sep=""),"./2-outputdata/PLOT",500,500)
}

varcorrliste_log <- plot_corrlist(ylist,xlist,list_scales,scalenames,
                                  "./2-outputdata/PLOT/log","./2-outputdata/table/log",ro=0,"KM2",logoption=TRUE)

for(yname in ylist){
  html_plot(paste("logRR2Pval",xlist,yname,sep=""),paste("logRR2Pval",yname,sep=""),"./2-outputdata/PLOT",500,500)
}

##############################################
# compare corr results with randomized fires #
##############################################
# output from over_join_random

### calc surf_count, km2, pour alea
for(i in 1:length(joinrandom)){
  alealiste <- joinrandom[[i]]
  surfaces <- list_scales[[i]]@data[,c(1,4)]
  
  for(j in 1:length(alealiste)){
    alea <- alealiste[[j]]

    alea$ID <- as.character(alea$ID)
    surfaces[,1] <- as.character(surfaces[,1])
    alea <- jointure2(alea, surfaces, alea$ID,surfaces[,1])
   
    joinrandom[[i]][[j]]$SUM_COUNT <- alea$SUM/alea$COUNT
    joinrandom[[i]][[j]]$SUMKM2 <- alea$SUM/alea$KM2
    joinrandom[[i]][[j]]$COUNTKM2 <- alea$COUNT/alea$KM2
  }
}

# list of var Y random
aleafield <- c("SUM","COUNT","SUM_COUNT","SUMKM2","COUNTKM2")
# liste des var Y
fieldY <- list(c("Surface_parcourue_m2","logSurface"),
               c("count","logcount"),
               c("surf_count","logsurf_count"),
               c("firesurfm2_km2","logfiresurfm2_km2"),
               c("countkm2","logcountkm2"))

# list of var X = xlist
index <- 1
i <- 1
for(alea in aleafield){
  fY <- fieldY[i]
  k <- 0
  for(yyy in fY[[1]]){
    if(k==0){option=FALSE}else{option=TRUE}
    for(xxx in xlist){
      if(index==1){
        r2aleavar <- over_corr(joinrandom,list_scales,alea,yyy,xxx,"KM2",option,FALSE,"./2-outputdata/PLOT/")
        index <- index + 1
      }else{
        interm <- over_corr(joinrandom,list_scales,alea,yyy,xxx,"KM2",option,FALSE,"./2-outputdata/PLOT/")
        r2aleavar <- rbind(r2aleavar,interm)
      }
    }
    k <-1
  }
  i <- i + 1
}
# same with logscale
index <- 1
i <- 1
for(alea in aleafield){
  fY <- fieldY[i]
  k <- 0
  for(yyy in fY[[1]]){
    if(k==0){option=FALSE}else{option=TRUE}
    for(xxx in xlist){
      if(index==1){
        r2aleavar <- over_corr(joinrandom,list_scales,alea,yyy,xxx,"KM2",option,TRUE,"./2-outputdata/PLOT/log")
        index <- index + 1
      }else{
        interm <- over_corr(joinrandom,list_scales,alea,yyy,xxx,"KM2",option,TRUE,"./2-outputdata/PLOT/log")
        r2aleavar <- rbind(r2aleavar,interm)
      }
    }
    k <-1
  }
  i <- i + 1
}

for(yname in ylist){
  html_plot(paste("R2alea",xlist,yname,sep=""),paste("R2alea",yname,sep=""),"./2-outputdata/PLOT",500,500)
}
for(yname in ylist){
  html_plot(paste("logR2alea",xlist,yname,sep=""),paste("logR2alea",yname,sep=""),"./2-outputdata/PLOT",500,500)
}

#################################
# remove intermediary variables #
#################################
rm(list=ls()[!(ls()=='fires'|ls()=='scalenames'|ls()=='list_scales'|
                 ls()=="joinrandom"|ls()=="desc_stat"|ls()=="r2aleavar"|
                 ls()=="varnames"|ls()=="var_stat"|ls()=='varcorrliste'|
                 ls()=='varcorrliste_log')])