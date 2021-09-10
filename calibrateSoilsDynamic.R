
#sid='opSID_40105'
#CalibrateSoilsDynamic(rawTS)

CalibrateSoilsDynamic <- function(rawTS, sid, calSrc='APSIM'){
  
  
  minQt = 0.02
  maxQt = 0.98
  
  print(sid)
   siteNames <- getDBSiteNames()
   rec <- siteNames[siteNames$SiteID == sid, ]
   print(rec)
  
  awcs <- getAWCParamsForSite(siteName = rec$ProjectSiteName, datasource=calSrc)
  awcSpline <- splineVals(indf=awcs, atts=c('LL15', 'DUL'))
  
  spdfProbe <- data.frame(sid=character(), calSource=character(), depth=numeric(), minProbe=numeric(), minProbeDate=character(), 
                          maxProbe=numeric(), maxProbeDate=character(), thickness=numeric(),modLL=numeric(), modDUL=numeric())
  
  upperDepth=0
  
  for (j in 1:ncol(rawTS)) {
    
    colname <- colnames(rawTS)[j]
    depth <- as.numeric(str_split(colname, '_')[[1]][2])
    
    ts <- rawTS[,j]
    qts <- quantile(ts, probs = c(minQt, maxQt), na.rm = T, names = TRUE)
    
    minVal <- as.numeric(qts[1])
    idx <- which.min(abs(ts-minVal))
    minDt <- index(ts[idx])
    
    maxVal <- as.numeric(qts[2])
    idx <- which.min(abs(ts-maxVal))
    maxDt <- index(ts[idx])
    
    llspline <- awcSpline[[1]]
    
    if((depth/10) <= max( awcSpline[[1]]$est_1cm$UD ) ){
      
      sll <- awcSpline[[1]]$est_1cm[awcSpline[[1]]$est_1cm$UD == (depth/10),]$SPLINED_VALUE / 100
      sul <- awcSpline[[2]]$est_1cm[awcSpline[[2]]$est_1cm$UD == (depth/10),]$SPLINED_VALUE / 100
      
      width=depth-upperDepth
      upperDepth=depth
      
      ol <- data.frame(sid=sid, calSource=calSrc, depth=depth, minProbe=minVal, minProbeDate=minDt, maxProbe=maxVal, maxProbeDate=maxDt,
                       thickness=width, modLL=sll*100, modDUL=sul*100)
      spdfProbe <- rbind(spdfProbe, ol)
    }
    
  }
  
  return(spdfProbe)
}
