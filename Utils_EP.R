library(raster)
library(RSQLite)
library(jsonlite)
library(xts)
library(epiR)
library(gdata)
library(ggplot2)
library(mpspline2)
library(httr)
library(lubridate)



rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+09,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory


machineName <- as.character(Sys.info()['nodename'])
if(machineName == 'soils-discovery'){
  probeDBfile <- '/datasets/work/lw-soildatarepo/work/Ross/EP/EP_ProbeData.db'
  print(probeDBfile)
}else{
  probeDBfile <- 'C:/Projects/EP/ProbeCalibrations/EP_ProbeData.db'
}


plotProfile <- function( x, depths, pts=NULL, attName, title=''){
  
  if(!is.null(pts)){
   xmax <-  max(c(pts$x, x), na.rm = T)
  }else{
    xmax <-  max(c(x))
  }
  
  plot( x, depths, ylim=c(max(depths)+ (max(depths)/10),0), xlim=c(0,xmax), type='l', lwd=2, ylab='Depth', xlab=attName, col='blue', main=title)
  if(!is.null(pts)){
    points( pts$x, pts$y,  pch=18, col='orange', cex=2)
  }
}


runQuery <- function(sql){
  con <- dbConnect(RSQLite::SQLite(), probeDBfile )  
    res <- dbSendQuery(con, sql)
  df <- dbFetch(res)
  dbClearResult(res)
  RSQLite::dbDisconnect(con)
  return(df)
}



getTotalYearlyTS <- function(inTS){
  
  ts <- xts( rowSums(inTS), order.by = index(inTS))
  
  ts[is.na(ts)] <- 0 
  startDate <- start(ts)
  endDate<- end(ts)
  
  startYr <- year(startDate)
  endYr <- year(endDate)
  
  numYrs <- endYr - startYr
  yrs <- seq(from=startYr, to=endYr, by=1)
  
  for (i in 1:length(yrs)) {
    yr <- yrs[i]
    sst <- ts[as.character(yr)]
    index(sst) <- index(sst) - (365 * (i-1))
    
    if(i==1){
      odf <- sst
    }else{
      odf <- merge.xts(odf, sst)
    }
    
  }
  colnames(odf) <- yrs
  return(odf)
  
}



plotPAWProfile <- function(ts, calibs, title, bktSize=null){
  
  sbx <- c(calibs$modLL, rev(calibs$modDUL)) * 100
  sby <- c(calibs$depth/10, rev(calibs$depth/10))
  water <- c(calibs$modLL*100, rev((calibs$modLL*100)+as.numeric(ts)))
  
  plot( 0, type="n", main=paste(''), 
        xlab='Volumeteric Soil Moisture', ylab='Soil Depth (cm)',
        yaxs = "i", xaxs = "i", xlim = c((min(calibs$modLL)*100)-2, (max(calibs$modDUL)*100) + 2), ylim = rev(range(c(0,110))),
        cex.lab = 1.5
  )
  
  polygon(sbx,sby,
          col=c("navajowhite3"),
          border=c("navajowhite3"),
          lwd=1, lty=c("solid"))
  
  polygon(water,sby,
          col=c("lightskyblue"),
          border=c("lightskyblue"),
          lwd=1, lty=c("solid"))
  
  # if(!is.null(bktSize)){
  #     text(0.5,0.4, paste0("Total Bucket Size = ", round(bktSize), ' mm' ), font=2)
  # }
}





# siteName = 'Mathews'
# datasource = 'SoilWaterExpress'
# attribute = 'P3B_VL_03'


copyTable <- function(tableName, copyName){
  
  con <- dbConnect(RSQLite::SQLite(), probeDBfile )  
  
  sql <- paste0("select * from ", tableName )
  res <- dbSendQuery(con, sql)
  df <- dbFetch(res)
  dbClearResult(res)
  
  dbWriteTable( con, copyName, df )
  
  RSQLite::dbDisconnect(con)
  
  return(NULL)
}

getProjectSiteNames <- function(){
  
  con <- dbConnect(RSQLite::SQLite(), probeDBfile )  
  
  sql <- paste0("select * from Names" )
  res <- dbSendQuery(con, sql)
  df <- dbFetch(res)
  dbClearResult(res)
  RSQLite::dbDisconnect(con)
  
  return(df)
}


getProbeCalibrationData <- function(sid, datasource='SoilWaterExpress'){
  con <- dbConnect(RSQLite::SQLite(), probeDBfile )  
  
  sql <- paste0("select * from probeCalibration where calSource = '", datasource, "' and sid = '", sid, "'" )
  res <- dbSendQuery(con, sql)
  df <- dbFetch(res)
  dbClearResult(res)
  RSQLite::dbDisconnect(con)
  
  return(df)
}


getSoilAttribute <- function(siteName, datasource, attribute){
    con <- dbConnect(RSQLite::SQLite(), probeDBfile )  
    
    sql <- paste0("select * from soilData where source = '", datasource, "' and site = '", siteName, "' and method = '", attribute, "' order by upperDepth" )
    res <- dbSendQuery(con, sql)
    df <- dbFetch(res)
    dbClearResult(res)
    RSQLite::dbDisconnect(con)
    
    return(df)
}

getAllProbeInfo <- function(){
  url <- "http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?owner=EPARF&usr=ross.searle@csiro.au&pwd=S4QQBMk74zhnBnEpTcd6iLwlUredn6kekLkjFL"
  locs <- fromJSON(url)
  return(locs)
}

getAWCParamsForSite <- function(siteName, datasource){
 
  dulRecs <- getSoilAttribute(siteName=siteName, datasource=datasource, attribute='P3B_VL_03')
  llRecs <- getSoilAttribute(siteName=siteName, datasource=datasource, attribute='P3B_VL_15')
  odf <- data.frame(sid=siteName, upperDepth=dulRecs$upperDepth, lowerDepth=dulRecs$lowerDepth, LL15=llRecs$value, DUL=dulRecs$value)
  return(odf)
  
}

getDBProbeDepths<- function(sid){
  
  conp <- dbConnect(RSQLite::SQLite(), probeDBfile ) 
  
  sql <- paste0("select distinct sid, depth from ProbeData where sid = '", sid, "' and dataType = 'Soil-Moisture';")
  res <- dbSendQuery(conp, sql)
  df <- dbFetch(res)
  dbClearResult(res)
  RSQLite::dbDisconnect(conp)
  
  return(df)
  
}

getDBProbeSiteNames<- function(){
  
  conp <- dbConnect(RSQLite::SQLite(), probeDBfile ) 
  
  sql <- paste0("select distinct sid from ProbeData;")
  res <- dbSendQuery(conp, sql)
  df <- dbFetch(res)
  dbClearResult(res)
  RSQLite::dbDisconnect(conp)
  
  return(df)
  
}


getDBSiteNames<- function(sid = NULL){
  
  conp <- dbConnect(RSQLite::SQLite(), probeDBfile ) 
  
  if(!is.null(sid)){
    sql <- paste0("select * from Names where SiteID = '", sid, "';")
  }else{
    sql <- paste0("select * from Names;")
  }
  
  res <- dbSendQuery(conp, sql)
  df <- dbFetch(res)
  dbClearResult(res)
  RSQLite::dbDisconnect(conp)
  
  return(df)
  
}


getProbeDataDF <- function(sid, productType='Soil-Moisture', depth=0, startDate='2016-01-01', endDate='2021-07-14'){
  
  conp <- dbConnect(RSQLite::SQLite(), probeDBfile ) 
  
  sql <- paste0("select * from probeData where sid='", sid, 
                "' and dataType = '", productType, 
                "' and depth = ", depth, 
                " and date >= '", startDate, 
                "' and date <= '", endDate, "'",
                " order by date"  )
  res <- dbSendQuery(conp, sql)
  df <- dbFetch(res)
  dbClearResult(res)
  RSQLite::dbDisconnect(conp)
  
  return(df)
  
}

getProbeDataTS <- function(sid, productType='Soil-Moisture', depth=0, startDate='2016-01-01', endDate='2021-07-14', tempCorrection=F, correctionVal=0.3){
  
  df <- getProbeDataDF(sid=sid, productType=productType, depth, startDate=startDate, endDate=endDate)
  
  ts <- xts(x=df$value, order.by = as.Date(df$date))
  colnames(ts)<-'Val'
  
 
  if(tempCorrection){
    Tref=18
    f = correctionVal
    tdf <- getProbeDataDF(sid=sid, productType='Soil-Temperature', depth, startDate=startDate, endDate=endDate)
    
   
    tempts <- xts(x=tdf$value, order.by = as.Date(tdf$date))
    colnames(tempts)<-'Temp'
    mts <- merge.xts(ts,tempts)

    tsCorrected <- mts$Val  + (Tref-mts$Temp) * f
     return(tsCorrected)
  }else{
    return(ts)
  }
  
  
 
  
}

getAllProbeDataTS <- function(sid, productType='Soil-Moisture', depth=0, startDate='2016-01-01', endDate='2021-07-14', tempCorrection=F, correctionVal=0.3){
  
  depths <- getDBProbeDepths(sid)
  cnt=0
  cns <- c()
  cnts <- c()
  for (i in 1:nrow(depths)) {
    pd <- getProbeDataTS(sid=sid, depth=depths$depth[i], productType=productType, startDate=startDate, endDate=endDate, tempCorrection=tempCorrection, correctionVal)
    cnts <- c(cnts, nrow(pd))
    if(nrow(pd)!=0){
        cns <- c(cns, depths$depth[i])
        cnt=cnt+1
        if(cnt==1){
          od <- pd
        }else{
          od <- merge.xts(od, pd, join = 'outer')
        }
    }
  }
  
  if(all(cnts==0)){return(NULL)}
  
  colnames(od) <- paste0('Depth_', cns)
  return(od)
}

getModelDataDF <- function(sid=NULL, model='SMIPS2', version=NULL, productType='totalbucket', startDate='2016-01-01', endDate='2021-07-14'){
 
  con <- dbConnect(RSQLite::SQLite(), DrillDBfile ) 
  
  sql <- paste0("select * from drillData where sid='", sid, 
                                            "' and model = '", model, 
                                            "' and version = '", version, 
                                            "' and productType = '", productType, 
                                            "' and date >= '", startDate, 
                                            "' and date <= '", endDate, "'",
                                            " order by date"  )
  res <- dbSendQuery(con, sql)
  df <- dbFetch(res)
  dbClearResult(res)
  RSQLite::dbDisconnect(con)
  
  return(df)
}

getModelDataTS <- function(sid=NULL, model='SMIPS2', version=NULL, productType='totalbucket', startDate='2016-01-01', endDate='2021-07-14'){
  
  df <- getModelDataDF(sid=sid, model=model, version=version, productType=productType, startDate=startDate, endDate=endDate)
  
  ts <- xts(x=df$value, order.by = as.Date(df$date))
  colnames(ts)<-'Val'
  return(ts)
}

fitStatsx <- function(obsVal, modelVal, attName = 'Att', xName='Observed', yName='Predicted', outfilename = '', legPos='topright', subtitle='',  verbose = F)
{
  dfn <- na.omit(data.frame(obsVal,modelVal))
  obsVal <- dfn[,1]
  modelVal <- dfn[,2]
  
  cccC <- epi.ccc(obsVal, modelVal, ci = "z-transform",conf.level = 0.95)
  r.sqC <- cor(obsVal, modelVal)^2
  
  fitC <- lm(modelVal ~ obsVal-1, data=dfn)
  validC = data.frame(obsVal, modelVal)
  
  totAp <- sum(obsVal)
  totS <- sum(modelVal)
  prop <- totAp/totS
  
  minVal = 0
  maxX = max(obsVal)
  maxY = max(modelVal)
  maxVal = max(maxX, maxY)
  
  
  plot(validC, main=paste( 'Model Fit', attName ), xlab=xName, ylab = yName, pch=3, cex =0.5, xlim = c(minVal,maxVal), ylim = c(minVal,maxVal))
  abline(fitC, col="red")
  abline(0,1, col="green")
  mtext(subtitle, cex=0.5)
  
  tx =  maxVal *0.6
  ty1 =  maxVal * 0.05
  ty2 =  maxVal * 0.15
  
  legend(legPos, c('Regression Line', '1:1') , lty=1, col=c('red','green'), bty='n', cex=1)
  
  text(tx,ty1, paste("R2 = ",round(r.sqC, digits = 2)), pos=4)
  text(tx,ty2, paste("LCCC = ", round(cccC$rho.c[,1], digits = 2)), pos=4)
  
  
  l1 <- c('Model Fit Stats For ', '', 'R2', 'LCCC', 'RMSE', 'ME', 'Proportion', 'Observed Average', 'Modelled Average')
  l2 <- c(attName, '', round(r.sqC, digits = 2), round(cccC$rho.c[,1], digits = 2), round(mean((obsVal - modelVal)^2)^0.5, digits = 3),
          round(mean(modelVal-obsVal), digits = 3),  round(prop, digits = 3), round(mean(obsVal), digits = 3), round(mean(modelVal), digits = 3))
  
  outdf <- data.frame(Metric = l1, Value = l2)
  
  if(verbose){
    print(outdf)
  }
  
  
  if(!outfilename==""){
    file.create(outfilename)
    write(paste("Model Fit Stats For - ", attName, "run on", Sys.time(), Sys.Date() ), file=outfilename, append=TRUE)
    write(paste('-----------------------------------------------------'),file=outfilename, append=TRUE)
    write(paste("R2 = ", round(r.sqC, digits = 2) ), file=outfilename, append=TRUE)
    write(paste("LCCC = ", round(cccC$rho.c[,1], digits = 2) ), file=outfilename, append=TRUE)
    write(paste("RMSE = ", round(mean((obsVal - modelVal)^2)^0.5, digits = 3) ), file=outfilename, append=TRUE)
    write(paste("ME = ", round(mean(modelVal-obsVal), digits = 3)), file=outfilename, append=TRUE)
    write(paste('Proportion = ', round(prop, digits = 3)), file=outfilename, append=TRUE)
    write(paste('Overall average Observed = ', round(mean(obsVal), digits = 3)), file=outfilename, append=TRUE)
    write(paste('Overall average Modelled = ', round(mean(modelVal), digits = 3)), file=outfilename, append=TRUE)
  }
  
  #outText <- paste0( round(r.sqC, digits = 2), ",",round(cccC$rho.c[,1], digits = 2), ",", round(mean((obsVal - modelVal)^2)^0.5, digits = 3), ",", round(mean(modelVal-obsVal), digits = 3), ",", round(prop, digits = 3), ",",  round(mean(obsVal/numYears), digits = 3), ",", round(mean(modelVal/numYears), digits = 3) )
  
  
  
  return (outdf)
  
}

splineVals <- function(indf, atts){
  
  ol <- vector("list", length = length(atts))
  
  indf$LL15 <- as.numeric(as.character(indf$LL15))
  indf$DUL <- as.numeric(as.character(indf$DUL))
  
  for (n in 1:length(atts)) {
    att=atts[n]
    sdf <- data.frame(indf[,c(1:3)], as.numeric(indf[att][[1]]),stringsAsFactors = FALSE)
    colnames(sdf) <- c('SID','UD', 'LD', 'VAL' )
    spline <- mpspline_tidy(obj = sdf, var_name = 'VAL')
    ol[[n]] <- spline
  }
  names(ol) <- names(atts)
  return(ol)
}


getLayerVolumeticTS <- function(sid, rawProbeVals, calData, probeDepth=NULL, type=type){

  depth <-probeDepth
  rec <- calData[calData$depth==depth,]
  
  mmperpv <-  (rec$modDUL - rec$modLL) / (rec$maxProbe - rec$minProbe) 
  adj<-pmax(rec$minProbe,(rawProbeVals) )
  clippedTS<-pmin(rec$maxProbe,(adj) )
  availWater_mm <- mmperpv * (clippedTS-rec$minProbe) * rec$thickness
  
  if(type=='Available'){
    availWater_mm <- mmperpv * (clippedTS-rec$minProbe) * rec$thickness
    return(xts(availWater_mm, order.by = (index(rawProbeVals))))
  }else if(type=='Total'){
    volVals_mm <- (rec$modLL + (mmperpv * (clippedTS-rec$minProbe))) * rec$thickness
    return(xts(volVals_mm, order.by = (index(rawProbeVals))))
    #return(volVals_mm)
  }
}


getAllLayersVolumeticTS <- function(sid, startDate, endDate, dataSource='APSIM', type='Available', tempCorrection=F, correctionVal=0.3){
  
  dps <- getDBProbeDepths(sid = sid)
  calibs = getProbeCalibrationData(sid = sid, datasource=dataSource)
  
  cnt=0
  for (i in 1:nrow(dps)) {
    
    rawProbeTS <- getProbeDataTS(sid=sid, productType='Soil-Moisture', depth=dps$depth[i], startDate=startDate, endDate=endDate, tempCorrection=tempCorrection, correctionVal=correctionVal)
    
    if(nrow(rawProbeTS) > 0){
      cnt=cnt+1
      
      volTS <- getLayerVolumeticTS(sid=sid, rawProbeVals=rawProbeTS, calData=calibs, probeDepth=dps$depth[i], type=type)
      colnames(volTS) <- paste0('D_', dps$depth[i])
      
      if(cnt==1){
        ots <- volTS
      }else{
        ots <- merge.xts(ots, volTS)
      }
      
    }
    else{
     # print(paste0('No data for ', sid, ' Depth = ', dps$depth[i]))
    }
  }
  
  if(exists('ots'))
  {
    return(ots)
  }else{
    return(NULL)
  }
  
  
 
}


getAllLayersVolumeticTS2 <- function(sid, rawTS, calibs, type='Available', tempCorrection=F, correctionVal=0.3){
  
  dps <- getDBProbeDepths(sid = sid)
  #calibs = getProbeCalibrationData(sid = sid, datasource=dataSource)
  
  cnt=0
  for (i in 1:nrow(dps)) {
    
   #rawProbeTS <- getProbeDataTS(sid=sid, productType='Soil-Moisture', depth=dps$depth[i], startDate=startDate, endDate=endDate, tempCorrection=tempCorrection, correctionVal=correctionVal)
    
    rawProbeTS <- rawTS[,i]
    
    if(nrow(rawProbeTS) > 0){
      cnt=cnt+1
      
      volTS <- getLayerVolumeticTS(sid=sid, rawProbeVals=rawProbeTS, calData=calibs, probeDepth=dps$depth[i], type=type)
      colnames(volTS) <- paste0('D_', dps$depth[i])
      
      if(cnt==1){
        ots <- volTS
      }else{
        ots <- merge.xts(ots, volTS)
      }
      
    }
    else{
      # print(paste0('No data for ', sid, ' Depth = ', dps$depth[i]))
    }
  }
  
  if(exists('ots'))
  {
    return(ots)
  }else{
    return(NULL)
  }
  
  
  
}

plotPAW <- function(dfBucket, title='PAW', yscale=10){
  
  sbx <- c(dfBucket$LL, rev(dfBucket$DUL))
  sby <- c(dfBucket$Depths, rev(dfBucket$Depths)) /yscale 
  svw <- c(dfBucket$LL,  rev(dfBucket$VolWater)) 
  
  plot( 0, type="n", main=paste(title), 
        xlab='Volumteric Soil Moisture (cm/cm)', ylab='Soil Depth (cm)',
        yaxs = "i", xaxs = "i", xlim = c(min(dfBucket$LL)-5, max(dfBucket$DUL) + 5), ylim = rev(range(c(0,110))),
        cex.lab = 1.5
  )
  
  polygon(sbx,sby,
          col=c("navajowhite3"),
          border=c("navajowhite3"),
          lwd=1, lty=c("solid"))
  
  polygon(svw,sby,
          col=c("lightskyblue"),
          border=c("lightskyblue"),
          lwd=1, lty=c("solid"))
}

getBucket <- function(SiteID, soilAWCValues, volVals, soilInfo){
  
  bp<- soilAWCValues
  LL <- bp$ll * 100
  DUL <- bp$dul * 100
  Depths <- bp$depth
  VolWater <- volVals$Vol * 100
  Thickness= soilInfo$thickness
  outDF <- data.frame(LL,DUL, VolWater, Depths,Thickness)
  return(outDF)
  
}


clipTS <- function(TS, lowerQuant=0.05, upperQuant=0.95){
  
  qts <- quantile(TS, probs = c(lowerQuant, upperQuant), na.rm = T, names = TRUE)
  
  minVal <- as.numeric(qts[1])
  maxVal <- as.numeric(qts[2])
  idxs <- which(TS<minVal)
  TS[idxs] <- minVal
  idxs <- which(TS>maxVal)
  TS[idxs] <- maxVal
  summary(TS)
  return(TS)
}



showSiteMap <- function(sid, zoom=15){
  urlsitdf <- "http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?sensortype=Soil-Moisture&usr=ross.searle@csiro.au&pwd=S4QQBMk74zhnBnEpTcd6iLwlUredn6kekLkjFL"
  sitdf <- fromJSON(urlsitdf)
  locs <- sitdf
  rec<-locs[locs$SiteID==sid,]
  lon <- rec$Longitude
  lat <- rec$Latitude
  leaflet::leaflet() %>% addProviderTiles('Esri.WorldImagery') %>% 
    setView(lon, lat, zoom = zoom)  %>%
    addProviderTiles("CartoDB.PositronOnlyLabels") %>%
    addMarkers(lon, lat)
}


compareModelAtSite <- function(sid, dataType, depth, startDate, endDate, mVersion){
  
  modelTS <- getModelDataTS(sid=sid, version=mVersion, productType='totalbucket', startDate=startDate, endDate=endDate)
  modelTS <- clipTS(modelTS)
  head(modelTS)
  
  modelTS <- rollmean(modelTS,30)
  
  dps <- getDBProbeDepths(siteName=sid)
  
  if(nrow(dps)>0){
    
    upperList <- numeric(length = nrow(dps)+1)
    cnt=0
    for (i in 1:nrow(dps)) {
      
      rawProbeTS <- getProbeDataTS(sid=sid, productType='Soil-Moisture', depth=dps$depth[i], startDate=startDate, endDate=endDate)
      
      if(nrow(rawProbeTS) > 0){
        
        if(dps$depth[i] <= 900){
          cnt=cnt+1
          
          if(dps$depth[i]==0){
            pd <- 20
            dd <- 200
          }else{
            pd <- dps$depth[i]
            dd <- dps$depth[i]
          }
          
          thickness <- dd - max(upperList)
          upperList[i+1] <- dd
          
          mRaw <- merge.xts(rawProbeTS, modelTS)
          colnames(mRaw) <- c('rawProbe', 'Model')
          # dygraph(mRaw, main=sid) %>% dyRangeSelector()
          # rwaFit<-fitStatsx(obsVal=mRaw$rawProbe, modelVal=mRaw$Model, att=sid, xName='RawProbe', yName='Model', verbose = F)
          
          soilInfo <- getSoilInfo(sid)
          
          volTS <- getLayerVolumeticTS(type='Available',rawProbeVals=rawProbeTS, soilParams=soilInfo, probeDepth=pd, layerWidth = thickness)
          volTS <- rollmean(volTS,10)
          
          if(cnt==1){
            ots <- volTS
          }else{
            ots <- merge.xts(ots, volTS)
          }
        }
      }
      else{
        print(paste0('No data for ', sid, ' Depth = ', dps$depth[i]))
      }
    }
    
    rs <- rowSums(ots, na.rm = T)
    sumTS <- xts(x=rs, order.by = index(ots))
    mSum <- merge.xts(sumTS, modelTS)
    colnames(mSum) <- c('Probes', 'Model')
    
    # dygraph(mSum, main=paste0('Total Available Water (mm), - ', sid)) %>% dyRangeSelector()
    fs <- fitStatsx(obsVal=mSum$Probes, modelVal=mSum$Model, att=sid, xName='AvailWaterMM', yName='Model', verbose = F)
    print(paste0(sid, ' Model = ', mVersion, ' R2 = ',  fs[3,2], ' CCC = ',  fs[4,2]))
    return(mSum)
  }
}    