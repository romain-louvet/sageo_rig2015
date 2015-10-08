# AUthor : Romain Louvet
# Date   : 08/10/2015
#
# Functions used for handling shape files and statistics for multiple scale
# levels.

#############
# LIBRARIES #
#############

library(sp) # spatial objects
library(rgdal) # readOGR (open and read/write shape)
library(rgeos) # aggregate (spatial object)
library(fBasics) # skewness and kurtosis

#####################
# LIST OF FUNCTIONS #
#####################
# spatial objects handling, mapping
# 1) aggr_shp_list
# 2) points_random
# 3) over_join_random
# 4) map_list
#
# descriptive statistics, calc, plots
# 5) summary_list
# 6) desc_stat_var
# 7) plot_list
# 8) hist_list
# 9) boxplot_list
# 10) boxplot_var
#
# correlation, calc, plots
# 11) scatterplot_corrlist
# 12) plot_corrlist
# 13) over_corr
# 14) add_residuals
# 15) hist_norm_residuals
#
# miscellaneous
# 16) html_plot
# 17) html_plot_var
# 18) html_plot_index
# 19) jointure
# 20) jointure2
# 21) conv_char

#############
# FUNCTIONS #
#############

# 1) aggr_shp_list
### Aggregate polygons from shape file based on aggregation codes,
### sums selected variables, and create a list of spatial object
### (and corresponding shapefiles)
#
# data : input shape file, spatial object name
# IDS_col : IDS column names, character, list
# values_col : values column names, character, list
# folder : output folder, character
# names : output shape file name, character, list
#
# Example : 
# >test <- aggregate_list(vaucluse,list("SIREN_EPCI","CODE_CANT","CODE_ARR",
# + "CODE_DEPT"),list("POPULATION","SUPERFICIE"),"SHP",
# + list("epci84","canton84","arron84","dep84"))
# > head(test[[4]]@data)
# CODE_DEPT POPULATION SUPERFICIE
# 1        84     546630     357811

aggr_shp_list <- function(data,IDS_col,values_col,folder,names){
  # error
  if(length(IDS_col)!=length(names)){stop("Must have same IDs columns and names number")}
  
  # result list
  result <- list()
  
  for(i in 1:length(IDS_col)){
    name <- names[[i]]
    # aggregate and dissolve geometry according to ID
    ID <- IDS_col[[i]]
    data0 <- aggregate(data,by=list(data@data[,ID]),dissolve=TRUE)
      
    # reformat data.frame...
    # (keep only ID and values columns, recalc values
    # because aggregate geometries only correctly aggregate
    # geom)
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
    
    # add to result list
    result <- c(result,data1)
  }
  # return a list, each element has a name which is
  # the scale level ("names" option of this function)
  return(result)
}

# 2) points_random
### create random points within shape file extent and merge attributes
### output: list of spatialpointsdataframe
#
# poly = polygon shape extent (dont use big one polygon shape file)
# data = data frame of points to randomized (used to calculate n points=> nrow)
# iteal = number of random points to generate
# randompara = radom, regular, etc. cf ?spsample
# file = .Rdata file for saving results
points_random <- function(poly,data,iteal,randompara="random"){
  # list of random points shapes
  randomliste <- list()
  # loop
  for(ite in 1:iteal){
    n <- nrow(data)
    # random points
    random <- spsample(poly,n,randompara)
    # add values from table to randomize
    random <- SpatialPointsDataFrame(
      coords=random@coords,
      data=data.frame(ID=c(1:n),data),
      proj4string =random@proj4string,bbox = random@bbox)
    randomliste <- c(randomliste, random) 
  }
  return(randomliste)
}

# 3) over_join_random
### join by location output from points_random to each scale level from
### aggr_shp_list output.
### output : a list of list of dataframe
### Example : joinrandom[[i]][[j]]), where i is scale level and j radom points
### spatial objects number
### WARNING : NEED jointure function
#
# randomliste = result from point random function (origin)
# polylist = list of spatial objects, polygons (target) to join the points (output listscales)
# idpolyliste = list of id field in polygons (target)
# fieldrandom = field from randomliste to add to join by location
over_join_random <- function(randomliste,polyliste,idpolyliste,fieldrandom){
  idi <- 0
  result <- list()
  for(poly in polyliste){
    # idpoly index
    idi <- idi + 1
    # id random
    ida <- 0
    result0 <- list()
    
    # loop
    for(random in randomliste){
      # id random
      ida <- ida + 1
      
      idpoly <- idpolyliste[[idi]]
      # join by localisation
      joinloc <- over(random,poly[,idpoly])
      joinloc <- data.frame(joinloc,attribut=random@data[,fieldrandom])
      
      # sum random values (for point counts, attribut must be "1" for each row)
      sum_random <- aggregate(joinloc[,"attribut"] ~ joinloc[,idpoly], joinloc, FUN=sum)
      
      # join by attribute
      sum_random[,"joinloc[, idpoly]"] <- as.character(sum_random[,"joinloc[, idpoly]"]) 
      poly@data[,idpoly] <- as.character(poly@data[,idpoly])
      jo <- jointure(poly,sum_random,poly@data[,idpoly],sum_random[,"joinloc[, idpoly]"])
      
      data <- data.frame(ID=jo$joinloc...idpoly.,SUM=jo$joinloc....attribut..)
      
      # count
      count_random <- aggregate(joinloc[,"attribut"] ~ joinloc[,idpoly], joinloc, FUN=length)
      count_random[,"joinloc[, idpoly]"] <- as.character(count_random[,"joinloc[, idpoly]"]) 
      jo <- jointure(poly,count_random,poly@data[,idpoly],count_random[,"joinloc[, idpoly]"])
      
      data <- data.frame(data,COUNT=jo$joinloc....attribut..)
      
      # result
      result0[[ida]] <- data
    }
    result[[idi]] <- result0
  }
  #results: list of list
  # first list: each scale level
  # second list within first list: each random by scale level
  joinrandomresult <- result
  return(joinrandomresult)
}

# 4) map_list
### serialized mapping based on aggreg_shp_list output
### output : svg file
#
# liste = output from aggreg_shp_list
# var = variable name, character
# discreti = "quantile" ; "std dev"
# clr = color brewer parameter
# nclc = cluster number
# lgdsize = legend size
map_list <- function(liste,var,name,discreti,clr="Reds",nclc,lgdsize){
  
  # color legend
  library(RColorBrewer)
  
  for(i in 1:length(liste)){
    
    svg(filename = paste(name,i,".svg",sep=""))
    
    nclc2 <- nclc
    
    if(discreti=="quantile"){
      brks <- quantile(liste[[i]]@data[,var], probs = seq(0, 1, 1/nclc),names = TRUE, na.rm=TRUE)
      
      # vs error "breaks are not unique"
      if(length(unique(brks))!=length(brks)){
        while(length(unique(brks))!=length(brks)){
          nclc2 <- nclc2 - 1
          brks <- quantile(liste[[i]]@data[,var], probs = seq(0, 1, 1/nclc2),names = TRUE, na.rm=TRUE)
        }
        if(nclc2<3){
          warning(paste("Quantile discretisation is not working for ",
                        names(liste[[i]]@data)[1],", variable: ",var,sep=""))
          next 
        }
      }
  
      colorbrew <- brewer.pal(n = nclc2, name = clr)
      
    }else if(discreti=="std dev"){
      m <- mean(liste[[i]]@data[,var])
      s <- sd(liste[[i]]@data[,var])
      ctr_rdt <- (liste[[i]]@data[,var]-m)/s
      
      brks <- c(min(ctr_rdt))
      
      if(min(ctr_rdt)<-2){
        brks <- c(brks,-2,-1,0)
      }else{
        brks <- c(brks,-1,0)        
      }
      
      if(max(ctr_rdt)>2){
        brks <- c(brks,1,2,max(ctr_rdt))
      }else{
        brks <- c(brks,1,max(ctr_rdt))
      }
      colorbrew <- brewer.pal(n = length(brks)-1, name = clr)
      
    }else{
      #ERREUR
      stop("wrong discretisation parameter")
    }
    
    liste[[i]]@data$CLASS <- as.character(cut(liste[[i]]@data[,var], breaks = brks,
                                              labels = colorbrew, include.lowest = TRUE,
                                              right = FALSE))
    
    legend_plot <- as.character(cut(liste[[i]]@data[,var], breaks = brks,
                                    include.lowest = TRUE, right = FALSE))
    
    plot(liste[[i]], col = liste[[i]]@data$CLASS, border = "black",lwd = 0.2)
    label <- c()
    
    for(x in 1:(length(brks)-1)){
      if(discreti=="quantile"){
        #label <- c(label,paste("Q",x,sep=""))
        label <- c()
        for(jj in 1:length(brks)){
          if(jj!=length(brks)){
            if(jj==length(brks-1)){
              label <- c(label,paste("[",round(brks[jj],2),";",round(brks[jj+1],2),"]",sep=""))
            }else{
              label <- c(label,paste("[",round(brks[jj],2),";",round(brks[jj+1],2),"[",sep=""))
            } 
          }
        }
      }else if(discreti=="std dev"){
        label <- c(label,paste("SD",x,sep=""))
      }
    }
    legend("bottom",
           legend = label,
           bty = "n",
           fill = colorbrew ,
           cex = lgdsize,
           title = paste(discreti,' discretisation',sep=""),
           horiz=TRUE)
    title(paste(names(liste[[i]]@data)[1],var,sep=': ')) 
    dev.off()
  }
}

# 5) summary_list
### descriptive statistics summaries (mean, med, min, max, etc.)
### based on aggreg_shp_list output
### output : data frame and csv file
### WARING : need fBasics library
#
# list_data : output from aggreg_shp_list
# var : variable name (character)
# areavar : area variable name in aggreg_shp_list output
# output : output name
# scale_names : scale levels corresponding to elements in list_data
# ro : round up parameter
summary_list <- function(list_data,var,areavar,output,scale_names,ro=2){
  liste <- list()
  i <- 1
  for(data in list_data){
    # N
    n <- length(data@data[,var])
    # mean area
    area <- round(mean(data@data[,areavar]),1)
    # min
    mi <- min(data@data[,var],na.rm=TRUE)
    # which is min
    # 1 quartile
    q1 <- quantile(data@data[,var],c(0.25),na.rm=TRUE)
    # mean
    mean0 <- round(mean(data@data[,var],na.rm=TRUE),ro)    
    # median
    med <- median(data@data[,var],na.rm=TRUE)
    # mode
    #mode_f <- function(data){
    #  x <- table(as.vector(data))
    #  names(t)[t==max(t)]
    #}
    #mo <- mode_f(data@data[,var])
    # 3 quartile
    q3 <- quantile(data@data[,var],c(0.75),na.rm=TRUE)
    # max
    ma <- max(data@data[,var],na.rm=TRUE)
    # which is max
    # range
    ra <- ma-mi
    # variance
    va <- round(var(data@data[,var],na.rm=TRUE), ro)
    # standard deviation
    sd0 <- round(sd(data@data[,var],na.rm=TRUE), ro)
    # skewness
    sk <- round(skewness(data@data[,var],na.rm=TRUE), ro)
    # kurtosis
    ku <- round(kurtosis(data@data[,var],na.rm=TRUE), ro)
    
    #liste[[i]] <- c(n,mi,q1,mean0,med,mo,q3,ma,ra,va,sd0,sk,ku)
    liste[[i]] <- c(n,area,mi,q1,mean0,med,q3,ma,ra,va,sd0,sk,ku)
    i <- i + 1      
  }
  for(j in 1:length(liste)){
    if(j==1){
      data <- data.frame(liste[[j]])
    }else{
      data <- data.frame(data,liste[[j]])
    }
    names(data)[j]<- scale_names[j]
  }
  row.names(data) <- c("N","AREA_MEAN","MIN","Q1","MEAN","MEDIAN","Q3","MAX","RANGE","VAR","SD","SK","KU")
  
  data <- data.frame(t(data))
  write.table(data, file = paste(output,".csv",sep=""),sep=";",row.names=TRUE,col.names=NA)
  
  return(data)
}

# 6) desc_stat_var
### desc statistics summaries variation between scale levels
### need summary_list output
### The variation is calculated with standard deviation
### of a stat (mean for instance) for a variable considering
### the different scale level
### output : data frame and csv file
#
# desc_stat : summary_list output
# varnames : variable names list
# output : output name
# rows : stat names ex: c("MEAN")
# ro : round up parameter

desc_stat_var<-function(desc_stat,varnames=c("MEAN","MEDIAN","VAR"),rows,output,ro){
  for(i in 1:length(varnames)){
    col <- c()
    for(stat in rows){
      calc <- sd(desc_stat[[i]][[stat]])
      calc <- round(calc,ro)
      col <- c(col,calc)
    }
    if(i==1){
      data <- data.frame(col,row.names=rows)
    }else{
      data <- cbind(data,data.frame(col,row.names=rows))
    }
    names(data)[[i]] <- varnames[[i]]
  }
  write.table(data, file = paste(output,".csv",sep=""),sep=";",row.names=TRUE,col.names=NA)
  return(data)
}

# 7) plot_list
### serializing base ploting showing variation of a variable throught scales
### X axis : scales
### Y axis : var value
### output : svg file
#
# desc_stat : summary_list output
# lib : plot title
# varnames : variable names list
# folder : output folder, must be "./something/"
# logopt : log scale option
plot_list <- function(desc_stat,lib,varnames, folder,logopt=FALSE){
  for(i in 1:length(desc_stat)){
    var <- varnames[i]
    stat <- desc_stat[[i]]
    
    coln <- colnames(stat)
    
    lib <- stat$AREA_MEAN
    if(logopt){
      xv <- log(stat$AREA_MEAN)
    }else{
      xv <- stat$AREA_MEAN 
    }
    
    for(j in 1:ncol(stat)){
      col <- stat[,coln[j]]
      if(logopt){
        svg(filename = paste(folder,"plotlog_",var,j,".svg",sep=""))
      }else{
        svg(filename = paste(folder,"plot_",var,j,".svg",sep="")) 
      }
      
      par(mar = c(10,5,2,2))
      
      if(logopt){
        plot(x=xv,y=col,type="b",main=paste(var,": ",coln[j],"/scales",sep=""),
             xlab="Mean area (log.)",ylab=coln[j],xaxt = "n")
        
        axis(1,at=xv,labels=lib,las = 2)
      }else{
        plot(x=xv,y=col,type="b",main=paste(var,": ",coln[j],"/scales",sep=""),
             xlab="Mean area",ylab=coln[j])
      }
      dev.off()
    }
  }
}

# 8) hist_list
### serializing ploting of histogramms for variable from aggr_shp_list output
#
# list_scales = output from aggr_shp_list
# varlist = list of variable in data.frame from list_scales
# scale_names = scale corresponding to element from list_scales
# folder = output folder
# breakslist = breaks parameter from hist() function
hist_list <- function(list_scales,varlist,scale_names,folder="./",breakslist=0){
  for(j in 1:length(varlist)){
    for(i in 1:length(list_scales)){
      svg(filename=paste(folder,"His_",varlist[[j]],i,".svg",sep=""))
      
      var <- varlist[[j]]
      x <- list_scales[[i]]@data[,var]
          
      par(mar = c(5,5,2,5))
      if(breakslist==0){
        hist(x,xlab=var,ylab="Frequency (grey) + mean (blue) & median (green)",
             main=scale_names[[i]],col="grey")
      }else{
        hist(x,xlab=var,ylab="Frequency (grey) + mean (blue) & median (green)",
             main=scale_names[[i]],col="grey",breaks=breakslist[i])
      }
      
      abline(v = mean(x), col = "blue", lty = 2, lwd = 2)
      abline(v = median(x), col = "green", lty = 2, lwd = 2)
      
      par(new=TRUE)
      plot(x=x[order(x)],
           y=c(1:length(x))/length(x)*100,
           type="l",xaxt='n',yaxt='n',xlab=NA,ylab=NA,col="red",lwd=2)
      axis(side = 4)
      mtext(side = 4, line = 3, "Cumulative frequency - % (red)")
      
      dev.off()
    }
  }
}

# 9) boxplot_list
### BOXPLOT 1: boxplot one variable for multiple scales
#
# list_scales : output from aggr_shp_list
# var : var name
# out : TRUE or FALSE, outline
# nm : scales names
# la : las (horizontal by default)
# folder : output folder
# ext : file name extension
boxplot_list <- function(list_scales,var,out,nm,la=2,folder="./",ext=""){
  
  svg(filename=paste(folder,"bxplot_",var,ext,".svg",sep=""))
  
  par(mar = c(8,5,2,2))
  liste <- c()
  for(i in 1:length(list_scales)){
    liste <- c(liste,length(list_scales[[i]]@data[,var]))
  }
  ind <- which(liste==max(liste))
  
  nm2 <- c(nm[ind])
  
  data <- data <- data.frame(list_scales[[ind]]@data[,var])
  for(i in 1:length(list_scales)){
    if(i==ind){
      
    }else{
      N <- length(list_scales[[ind]]@data[,var])-length(list_scales[[i]]@data[,var])
      data <- data.frame(data, c(list_scales[[i]]@data[,var],rep(NA,N)))
      nm2 <- c(nm2,nm[i])
    }
  }  
  boxplot(data,outline=out,names=nm2,las=la,main=var)
  
  dev.off()
}

# 10)
### BOXPLOT 2: boxplot multiple variable for one scale
#
# scale : element of output from aggr_shp_list
# scale_name : main title
# var_list : var names list
# out : TRUE or FALSE, outline
# la : las (horizontal by default)
# folder : folder name
# ext : file name extension
boxplot_var <- function(scale,scale_name,var_list,out,la=2,folder="./",ext=""){
  
  svg(filename=paste(folder,"bxplot_",scale_name,ext,".svg",sep=""))
  
  par(mar = c(10,5,2,2))
  for(i in 1:length(var_list)){
    if(i==1){
      data <- data.frame(scale@data[,var_list[[i]]])
    }else{
      data <- data.frame(data,scale@data[,var_list[[i]]])
    }
  }
  boxplot(data,outline=out,names=var_list,las=la,main=scale_name)
  
  dev.off()
}

# 11) scatterplot_corrlist
### serializing scatterplot, with regression line, r and r2 results
#
# varnamesy : y axis var name
# varnamesx : x axis var name
# intconf : confidence interval option (WARNING : take time, a lot)
# data : from aggr_shp_list out, must be @data
# scalename : which scale (plot's main title)
# folder : output folder
scatterplot_corrlist <- function(varnamesy,varnamesx,intconf,data,scalename,folder){
  for(yname in varnamesy){
    for(xname in varnamesx){
      svg(filename=paste(folder,"XY",scalename,xname,yname,".svg",sep=""))
      
      xname2 <- xname
      yname2 <- yname
      
      x <- data[,xname]
      y <- data[,yname]
            
      linmod <- lm(y~x)
      summa <- summary(linmod)
      
      r <- round(cor(x,y),5)
      r2 <- round(r^2,5)
      pvalue <- round(summa$coefficients[2,4],8)
      
      plot(y~x,xlab=xname2,
           ylab=yname2,main=scalename)
      abline(linmod, col="red")
      
      # confidence interval (should be change, take too much time)
      # par défaut : 95% de confiance
      if(intconf){
        conf <- predict(linmod, interval="confidence")      
        lines(x, conf[,2], lty=2)
        lines(x, conf[,3], lty=2)  
      }
      
      mtext(paste("r=",r," r2=",r2," p-value=",pvalue,sep=""), side = 3, line=0)
      
      dev.off()
    }
  }
}

# 12) plot_corrlist
### plot correlation results variation through scales
### x axis : mean area by scale level
### y axis : r, r2, pvalue results
#
# varnamesy : x variable name list
# varnamesx : y variable name list
# list_scales : from aggr_shp_list out, must be @data
# scalenames : which scale (plot's main title)
# folder : output folder for svg file
# folder1 = output folder for csv file
# ro = round up parameter (for variation % r2)
# varsurf : area variable
# logoption : log scale
plot_corrlist <- function(varnamesy,varnamesx,list_scales,scalenames,folder,folder1,ro,varsurf,logoption){
  data <- matrix(nrow=length(varnamesx),ncol=length(varnamesy),dimnames=list(varnamesx,varnamesy))
  data2 <- data.frame("scales"=rep(NA,length(varnamesx)*length(varnamesy)),
                      "X_Y"=rep(NA,length(varnamesx)*length(varnamesy)),
                      "r"=rep(NA,length(varnamesx)*length(varnamesy)),
                      "r2"=rep(NA,length(varnamesx)*length(varnamesy)),
                      "pvalue"=rep(NA,length(varnamesx)*length(varnamesy)))
                      #"r_random"=rep(NA,length(varnamesx)*length(varnamesy)),
                      #"r2_random"=rep(NA,length(varnamesx)*length(varnamesy)))
  liste <- list(r2_pvalsup5prct=data,scales=data2)
  
  z <- 0
  
  for(yname in varnamesy){
      for(xname in varnamesx){
        
        rlist <- c()
        r2list <- c()
        pvallist <- c()
        
        areameanlist <- c()
                
        for(i in 1:length(list_scales)){
          
          z2 <- z + i
          
          data <- list_scales[[i]]@data
                  
          xname2 <- xname
          yname2 <- yname
          
          x <- data[,xname]
          y <- data[,yname]
          
          linmod <- lm(y~x)
          summa <- summary(linmod)
          
          r <- cor(x,y)
          r2 <- r^2
          
          areamean <- round(mean(data[,varsurf]),1)
          
          pvalue <- summa$coefficients[2,4]
          
          rlist <- c(rlist,r)
          r2list <- c(r2list,r2)
          pvallist <- c(pvallist,pvalue)
          
          #rallist <- c(rallist,r_random)
          #r2allist <- c(r2allist,r2_random)
          
          areameanlist <- c(areameanlist, areamean)
          
          liste$scales[z2,"scales"] <- scalenames[i]
          liste$scales[z2,"r"]<- r
          liste$scales[z2,"r2"]<- r2
          liste$scales[z2,"pvalue"]<- pvalue
          liste$scales[z2,"X_Y"]<- paste(xname,yname,sep="_")
          
          #liste$scales[z2,"r_random"]<- r_random
          #liste$scales[z2,"r2_random"]<- r2_random

          if(i==length(scalenames)){z<-z+i}          
        }
        
        svg(filename=paste(folder,"RR2Pval",xname,yname,".svg",sep=""))
        
        par(mar = c(10,5,2,2))
        
        if(logoption==TRUE){
          plot(log(areameanlist),rlist,type="b",main=paste(yname,xname,sep="/"),
               xlab="Mean units' area (log scale)",ylab="Correlation analysis results",xaxt ="n",ylim=c(-1,1.2),lwd=1.5)
          grid(NA, NULL, col = "gray", lty = "dotted",lwd = par("lwd"))
          
          lines(log(areameanlist),r2list,type="b",col="blue",lwd=1.5)
          lines(log(areameanlist),pvallist,type="b",col="red",lwd=1.5)
          
          legend("topright", lty=1, lwd=1, col=c("black","blue","red"), legend=
                   c("r","r2","p-value"),horiz=TRUE, xjust=0.5)
          
          #axis(1,at=1:length(scalenames),labels=scalenames,las = 2)
          axis(1,at=log(areameanlist),labels=round(areameanlist,0),las = 1)
          
          axis(4,at=0.05,labels=0.05)
          abline(0.05,0,col="grey",lty="dotted",lwd = par("lwd"),las = 2)
          
        }else{
          plot(areameanlist,rlist,type="b",main=paste(yname,xname,sep="/"),
               xlab="Mean units' area",ylab="Correlation analysis results",ylim=c(-1,1.2),lwd=1.5)
          
          grid(NA, NULL, col = "gray", lty = "dotted",lwd = par("lwd"))
          
          lines(areameanlist,r2list,type="b",col="blue",lwd=1.5)
          lines(areameanlist,pvallist,type="b",col="red",lwd=1.5)
          
          legend("topright", lty=1, lwd=1, col=c("black","blue","red"), legend=
                   c("r","r2","p-value"),horiz=TRUE, xjust=0.5)
          
          #axis(1,at=1:length(scalenames),labels=scalenames,las = 2)
          #axis(1,at=log(areameanlist),labels=round(areameanlist,0),las = 1)
          
          axis(4,at=0.05,labels=0.05)
          abline(0.05,0,col="grey",lty="dotted",lwd = par("lwd"),las = 2)
        }

        dev.off()
        
        truefalse <- pvallist<=0.05
        r2list2 <- r2list[truefalse]
        if(length(r2list2>1)){
          liste$r2_pvalsup5prct[xname,yname] <- round(max(r2list)/min(r2list),ro)          
        }
      }
  }
  write.table(liste$r2_pvalsup5prct, file = paste(paste(folder1,"R2_pval+5%",sep=""),".csv",sep=""),sep=";",row.names=TRUE,col.names=NA)
  write.table(liste$scales, file = paste(paste(folder1,"R2_R_pval",sep=""),".csv",sep=""),sep=";",row.names=FALSE)
  return(liste)
}

# 13) over_corr
### compare correlation results with dependent variable randomized
#
# joinrandom = randomized data, over_join_random output
# list_scales = original data
# randomY = field name from joinrandom, must be randomized scalesY
# scalesY = field name from list scale
# scalesX = field name from list scale
# surffield = area field from list_scales
# logoption = option, log+1 for randomY
# logaxis = option, log scale for x axis
# folder = output folder for svg
over_corr <- function(joinrandom,list_scales,randomY,scalesY,scalesX,surffield,logoption=FALSE,logaxis=FALSE,folder){
  nb <- length(list_scales)
  
  # real correlation
  r2liste <- c()
  pvliste <- c()
  # random results list
  meanlsite <- c()
  maxliste <- c()
  minliste <- c()
  
  # for each scale
  for(i in 1:nb){
    x <- list_scales[[i]]@data[,scalesX]
    y <- list_scales[[i]]@data[,scalesY]
    
    # calculate real corr coef
    lin <- lm(y~x)
    summa <- summary(lin)
    pvalue <- summa$coefficients[2,4]
    r2 <- cor(x,y)^2
    r2liste <- c(r2liste,r2)
    pvliste <- c(pvliste,pvalue)
    
    # calc random corr
    r2random <- c()
    for(random in joinrandom[[i]]){
      
      list_scales[[i]]@data[,1] <- as.character(list_scales[[i]]@data[,1])
      random$ID <- as.character(random$ID)
      
      jo <- jointure2(list_scales[[i]]@data,random,list_scales[[i]]@data[,1],random$ID)
      yrandom <- jo[,randomY]
      yrandom[is.na(yrandom)] <- 0
      
      if(logoption){
        yrandom<-log(yrandom+1)
      }
            
      r2a <- cor(x,yrandom)^2
      r2random <- c(r2random,r2a)
    }
    meanlsite <- c(meanlsite,mean(r2random))
    maxliste <- c(maxliste,max(r2random))
    minliste <- c(minliste,min(r2random)) 
  }
  
  # SORTIE
  nmes <- c()
  for(k in 1:nb){
    nmes <- c(nmes,names(list_scales[[k]]@data)[1])
  }
  if(logoption){
    randomY <- paste("log",randomY,sep="")
  }
  
  bestmax <- r2liste-maxliste
  bestmaxrela <- bestmax/maxliste
  
  result <- data.frame(SCALES=nmes,X=rep(scalesX,nb),Y=rep(scalesY,nb),Yrandom=rep(randomY,nb),
                       R2=r2liste,PVALUE=pvliste,R2almax=maxliste,R2almin=minliste,R2almean=meanlsite,
                       Bmax=bestmax,Bmaxrela=bestmaxrela)
  ###
  # visu
  ###
  
  svg(filename=paste(folder,"R2random",scalesX,scalesY,".svg",sep=""))
  
  xaxis <- c()
  for(i in 1:nb){
    xaxis <- c(xaxis,round(mean(list_scales[[i]]@data[,surffield])))
  }
  if(logaxis){
    xaxis2 <- xaxis
    xaxis <- log(xaxis+1)
    plot(xaxis,r2liste,type="b",ylim=c(0,1),
         xlab="Mean area",ylab="r2",main=(paste(scalesY,scalesX,sep="/")),xaxt ="n")
    
    axis(1,at=log(xaxis2),labels=round(xaxis2,0),las = 1)
    
    xpoly <- c(xaxis,rev(xaxis))
    ypoly <- c(minliste,rev(maxliste))
    
  }else{
    plot(xaxis,r2liste,type="b",ylim=c(0,1),
         xlab="Mean area",ylab="r2",main=(paste(scalesY,scalesX,sep="/")))
    xpoly <- c(xaxis,rev(xaxis))
    ypoly <- c(minliste,rev(maxliste))
  }
  
  polygon(xpoly,ypoly, col = 'grey', border = NA)
  
  lines(xaxis,meanlsite,type="l",lty=2,col="black")
  lines(xaxis,r2liste,type="b")
  lines(xaxis,pvliste,type="l",lty=2,col="red")
  
  xbest <- xaxis[bestmax==max(bestmax)]
  xbestreal <- xaxis[bestmaxrela==max(bestmaxrela)]
  ybest <- r2liste[bestmax==max(bestmax)]
  ybestrelat <- r2liste[bestmaxrela==max(bestmaxrela)]
  
  if(ybest>maxliste[bestmax==max(bestmax)]){
    points(xbest,ybest,col="green",pch = 19)   
  }
  if(ybestrelat>maxliste[bestmaxrela==max(bestmaxrela)]){
    points(xbestreal,ybestrelat,col="darkgreen",pch = 19)
  }
  
  pvpointX <- xaxis[pvliste>=0.05]
  pvpointY <- r2liste[pvliste>=0.05]
  
  points(pvpointX,pvpointY,col="red",pch = 19)

  dev.off()
  return(result)
}

# 14) add_residuals
### residuals : for each couple X-Y, calcl residuals, add values to list scales
### output from aggr_shp_list
#
# varnamesy : list of X variable names
# varnamesx : list of Y variable names
# listescales : output from aggr_shp_list
add_residuals <- function(varnamesy,varnamesx,listescales){
  # if X!=Y (length), error message
  if(length(varnamesy)==length(varnamesx)){
    for(i in 1:length(varnamesy)){
      for(j in 1:length(listescales)){
        x <- listescales[[j]]@data[,varnamesx[i]]
        y <- listescales[[j]]@data[,varnamesy[i]]
        
        
        linmod <- lm(y~x)
        summa <- summary(linmod)
        
        residu <- summa$residuals
        
        # add residual column
        name <- paste("rsd_",varnamesx[i],varnamesy[i],sep="")
        listescales[[j]]@data$name <- residu
      }
    }
    return(listescales)
  }else{
    # ERROR MESSAGE
  }
}

# 15) hist_norm_residuals
### do a HENRY LINE // PP Plot in order to analyse residuals
### output : svg file
### NB: maybe use addresiduals output instead of calc them
#
# varnamesy: list of Y var names
# varnamesx: list of X var names
# data: 
# scalename: name of scale levels
# folder: output folder
# breakslist: breaks for hist() function
hist_norm_residuals <- function(varnamesy,varnamesx,data,scalename,folder,breakslist=0){
  for(yname in varnamesy){
    for(xname in varnamesx){    
      x <- data[,xname]
      y <- data[,yname]
      
      linmod <- lm(y~x)
      summa <- summary(linmod)
      
      residu <- summa$residuals
      
      # HIST residuals
      svg(filename=paste(folder,"histresidu_",scalename,xname,yname,".svg",sep=""))
      par(mar = c(5,5,2,5))
      if(breakslist==0){
        hist(residu,xlab=paste("Residuals Y=",yname," X=",xname,sep=""),ylab="Frequency (grey) + mean (blue) & median (green)",main=scalename,col="grey")
      }else{
        hist(residu,xlab=paste("Residuals Y=",yname," X=",xname,sep=""),ylab="Frequency (grey) + mean (blue) & median (green)",main=scalename,col="grey",breaks=breakslist[i])
      }
      
      abline(v = mean(residu), col = "blue", lty = 2, lwd = 2)
      abline(v = median(residu), col = "green", lty = 2, lwd = 2)
      
      par(new=TRUE)
      plot(x=residu[order(residu)],
           y=c(1:length(residu))/length(residu)*100,
           type="l",xaxt='n',yaxt='n',xlab=NA,ylab=NA,col="red",lwd=2)
      axis(side = 4)
      mtext(side = 4, line = 3, "Cumulative frequency - % (red)")
      
      dev.off()
      
      ## Y ~ residus
      # (this graph must show no structure)
      svg(filename=paste(folder,"YRESIDU_",scalename,'_',xname,yname,".svg",sep=""))
      plot(y,residu, xlab=yname, ylab=paste("Residuals (with ",xname,")",
                                            sep=""),main=scalename)
      
      dev.off()
      ## HENRY LINE // PP Plot (probability probability plot)
      ## the more it is a straigh line, the better
      theoric <- pnorm(sort(residu),mean(residu),sd(residu))
      empiric <- 1:length(residu)/length(residu)
      
      svg(filename=paste(folder,"PP-PLOT",scalename,xname,yname,".svg",sep=""))
      
      plot(theoric~empiric,xlab="Empirical probability distribution",ylab="Theoretical probability distribution",
           main=paste("PP plot of X=",xname,"and Y=",yname," residuals (",scalename,")",sep=""))
      
      line <- lm(theoric~empiric)
      abline(line, col="red")
      
      # intervalle confiance !!!
      
      r2 <- cor(theoric,empiric)^2
      
      mtext(paste("r2=",r2,sep=""), side = 3, line=0)
      
      dev.off()
    }
  }
}

# 16) html_plot
### HMTL file for visualization. Create a single html file calling several
### svg file created with serialized plotting previously described (cf. function n°
### 4, 8, 9, 10, 11, 12, 13)
### Example: create a HTML file "bxplot" which display a list of
### svg file of boxplot for various variable list("bxplot_varA", "bxplot_varb")
#
# liste: liste of svg file name
# name: ouput html file name
# folder: folder for inputs and outputs
# width: display width (pixels)
# heigth: display heigth (pixels)

html_plot <- function(liste,name,folder,width,heigth){
  file <-file(paste("./",folder,"/",name,".html",sep=""))
  code <- c()
  for(n in liste){
    code <- c(code,paste('<embed src="',n,'.svg" width="',width,'" heigth="',heigth,'" type="image/svg+xml" />\n',sep=""))
  }
  write(code,file)
  close(file)     
}

# 17) html_plot_var
### variation of html_plot. HMTL file for visualization.
### Create a single html file calling several
### svg file created with serialized plotting previously described (cf. function n°
### 4, 8, 9, 10, 11, 12, 13)
### Example: create "plot.html" displaying "plot1.svg", "plot2.svg"
# 
# name: name of output file and root name for input svg file name
# folder: folder for inputs and outputs
# width: display width (pixels)
# heigth: display heigth (pixels)
# iteration: index number for svg files

html_plot_var <- function(name,folder,width,heigth,iteration){
  file <-file(paste("./",folder,"/",name,".html",sep=""))
  code <- c()
  for(i in 1:iteration){
    code <- c(code,paste('<embed src="',name,i,'.svg" width="',width,'" heigth="',heigth,'" type="image/svg+xml" />\n',sep=""))
  }
  write(code,file)
  close(file)
}

# 18) html_plot_index
### variation of html_plot. HMTL file for visualization.
### Create a single html file calling several
### svg file created with serialized plotting previously described (cf. function n°
### 4, 8, 9, 10, 11, 12, 13)
### Example: create "map_LEVEL1.html" displaying all the maps for all
### the variables (listnames) and for the scale level named "LEVEL1"
# 
# listnames: list of variable names
# indexname: name of the scale level, output html file name
# indexnumber: index number of scale level, for ex: 1 for the first level
# folder: folder for inputs and outputs
# width: display width (pixels)
# heigth: display heigth (pixels)
html_plot_index <- function(listnames,indexname,indexnumber,folder,width,heigth){
  file <-file(paste("./",folder,"/",indexname,".html",sep=""))
  code <- c()
  for(name in listnames){
    code <- c(code,paste('<embed src="',name,indexnumber,'.svg" width="',width,'" heigth="',heigth,'" type="image/svg+xml" />\n',sep=""))
  }
  write(code,file)
  close(file)     
}

# 19) jointure
### join by attribute, target must be spatial dataframe
### origin table variable will be added to target table
### NB:
### - works only with characters
### - target must not be @data
### - apply to @data
### - column complete name
#
# target: target table
# origin: origin table
# col_target: target variable in target table
# col_origin: origin variable in origin table
jointure <- function(target,origin,col_target,col_origin){
  if(class(col_target)!="character"){stop("Target column has to be character")}
  if(class(col_origin)!="character"){stop("Origin column has to be character")}
  target <- data.frame(target@data, origin[match(col_target,col_origin),])
}

# 20) jointure2 
### another version of jointure, merging by attributes
### the only difference is target must not be spatial dataframe
#
# target: target table
# origin: origin table
# col_target: target variable in target table
# col_origin: origin variable in origin table
jointure2 <- function(target,origin,col_target,col_origin){
  if(class(col_target)!="character"){stop("Target column has to be character")}
  if(class(col_origin)!="character"){stop("Origin column has to be character")}
  target <- data.frame(target, origin[match(col_target,col_origin),])
}

# 21) conv_char
### convert character (for jointure)
### convert to character a list of columns, because jointure
### function works only with character variables
#
# data : data.frame, spatial object
# col_list : character (column name)
conv_char <- function(data,col_list){
  for(col in col_list){
    data[,col] <-as.character(data[,col])
  }
  return(data)
}