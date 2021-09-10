library(stringr)
library(jsonlite)
library(httr)
library(xts)
library(dygraphs)
library(RSQLite)
library(shiny)
library(lubridate)
library(htmlwidgets)
library(htmltools)
library(leaflet)

machineName <- as.character(Sys.info()['nodename'])
if(machineName == 'soils-discovery'){

  source('./Utils_EP.R')
  source('./calibrateSoilsDynamic.R')

  
}else{

  source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/EP/Utils_EP.R')
  source('./calibrateSoilsDynamic.R')
}

probeInfo <- getDBSiteNames()
probeNames <- runQuery("select distinct(site) from soilData where source = 'APSIM' order by site")
probeNames <- probeNames[-4,]

startDate = '2016-01-01'
endDate = '2021-08-30'


getMonth <- 'function(d){
               var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
               return monthNames[d.getMonth()];
               }'

#the x values are passed as milliseconds, turn them into a date and extract month and day
getMonthDay <- 'function(d) {
                var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
                date = new Date(d);
                return monthNames[date.getMonth()] + " " +date.getUTCDate(); }'



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("EP Volumetric Probe Info"),

    tags$style('.shiny-plot-output  {
display: inline-block;
    float: left;
}'),
    
    tags$style(
      ".irs-bar {",
      "  border-color: transparent;",
      "  background-color: transparent;",
      "}",
      ".irs-bar-edge {",
      "  border-color: transparent;",
      "  background-color: transparent;",
      "}"
    ),
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel( width = 3,
          selectInput("probeName", "Probe ", choices=NULL, selected = "Matthews", width = 250),
          helpText("Select map date below"), 
          sliderInput("datePker", "Dates:", min = as.Date("2017-01-01","%Y-%m-%d"), max = as.Date("2021-09-01","%Y-%m-%d"), value=as.Date("2021-07-01"), timeFormat="%Y-%m-%d",
                      animate = animationOptions(interval = 300, loop = TRUE)),
          sliderInput("tempCor", "Temp Correction:", min = 0, max = 0.5, value=0.3),
          
          HTML('<BR><BR><BR><BR><BR><BR>'),
          leafletOutput("siteMap",width = 350, height = 350)
          
          ),
        
        # Show a plot of the generated distribution
        mainPanel(
           
          fluidRow(  dygraphOutput("volTSChart", width = "800px", height = "250px"), HTML(''), dygraphOutput("rawTSChart", width = "800px", height = "150px"),
                     dygraphOutput("rainTSChart", width = "800px", height = "150px"),
                     ),
          
          fluidRow( column(6, plotOutput("volProfile", width = "450px", height = "450px")), 
                    column(6, 
                           fluidRow( htmlOutput('vtext') )),
                           fluidRow(plotOutput("dial", width = "450px", height = "450px")),
                           fluidRow(dygraphOutput("seasons", width = "800px", height = "250px")),
                          fluidRow(HTML('<br><br><br><br><br><br><br><br><br><br><br>')),
                            
        ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  RV <- reactiveValues()
  RV$currentTS <- NULL
  RV$currentCalibs <- NULL
  RV$sid <- NULL
  RV$currentChart <- NULL
  RV$currentRainTS <- NULL
  RV$currentRawTS <- NULL
  
  
  output$vtext <- renderText({
    
     req(RV$currentCalibs)
     dv <-input$datePker

     
     yr <- year(dv)
     annRain <- sum(RV$currentRainTS[paste0(yr,'-01-01/', dv)])
     inSeasonRain <- sum(RV$currentRainTS[paste0(yr,'-04-01/', dv)])
    
     dfBucket <- sum(RV$currentCalibs$modDUL-RV$currentCalibs$modLL)*100
     swTotal <- sum(as.numeric(RV$currentTS[dv,]))
     swPercent <- (swTotal/dfBucket) * 100
     round(swPercent/100, digits=2)
  HTML(  paste0("<p style='color:green; font-weight: bold;'><br><br>Total Bucket Size = ", round(dfBucket), " mm",
           "<br>Available Soil Water = ", round(swTotal), " mm",
           "<br>Total Rain = ", round(annRain), " mm",
           "<br>&nbsp;In Season Rain = ", round(inSeasonRain), " mm",
           "</p>"
           )
  )
    
    })

  output$volProfile <- renderPlot({
    if(!is.null(RV$currentTS)){
     
      dv <-input$datePker
      plotPAWProfile(ts=RV$currentTS[dv,], calibs=RV$currentCalibs, title=RV$sid)
           
    }
  })
  
  output$dial <- renderPlot({

    dv <-input$datePker

    req(RV$currentTS)

     dfBucket <- sum(RV$currentCalibs$modDUL-RV$currentCalibs$modLL)*100
     swTotal <- sum(as.numeric(RV$currentTS[dv,]))
     swPercent <- (swTotal/dfBucket) * 100

    swdf <- data.frame(percentage=round(swPercent/100, digits=2), group='blue')
    ggp <- ggplot(swdf[1,], aes(fill = group, ymax = percentage, ymin = 0, xmax = 2, xmin = 1)) +

    geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="#ece8bd") +
    geom_rect() +
    coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
    geom_text(aes(x=0, y=1, label=paste0(percentage*100, '%')), size=15) +
    theme_void() +
    scale_fill_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188", "blue" = "#242b8a")) +
    scale_colour_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188", "blue" = "#242b8a")) +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank()) +
    guides(fill=FALSE) +
    guides(colour=FALSE)#+ annotate("text", x = 0, y=-0.5, label = "shipshipship")

  ggp 
  })
  
  observe({
    updateSelectInput(session, "probeName", choices =  probeNames, selected = 'Matthews' )
  })
  
  observe({
    
    req(input$probeName, input$tempCor)
    rec <- probeInfo[probeInfo$ProjectSiteName == input$probeName, ]
    RV$sid <- rec$SiteID
    RV$currentRawTS <- getAllProbeDataTS(sid=RV$sid, tempCorrection=T, correctionVal=input$tempCor)
    
    calibs <- CalibrateSoilsDynamic(rawTS = RV$currentRawTS, sid=sid, calSrc='APSIM')
    RV$currentCalibs <- calibs

   # RV$currentCalibs <- getProbeCalibrationData(sid = RV$sid, datasource='APSIM')
    
    ts <- getAllLayersVolumeticTS2(sid=RV$sid, rawTS=RV$currentRawTS, calibs, type='Available', tempCorrection=T, correctionVal=0.3)
    ts[is.na(ts)] <- 0 
    
    RV$currentTS <- ts
    RV$currentRainTS <- getProbeDataTS(sid=RV$sid , productType = 'Rainfall', depth=0)
    
    })
  
  
  output$volTSChart <- renderDygraph({
    RV$currentChart
  })
  
  output$rawTSChart <- renderDygraph({

    req(RV$currentRawTS)
    dt <- as.Date(input$datePker)
    dygraph(RV$currentRawTS,  group = 'Main')%>%
      dyLegend(show = "follow", width = 250, showZeroValues = TRUE, labelsDiv = NULL, labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
      dyAxis("y", label ='Raw Probe Values', valueRange = c(0, maxVal))  %>%
    dyEvent( dt, label = dt, labelLoc = c("top"), color = "black", strokePattern = "solid")
  })
  

  
  observe({
    
    if(!is.null(RV$currentTS)){
      
      dt <- as.Date(input$datePker)
      yr <- year(dt)

      isolate({
        
        m <- rowSums(RV$currentTS,na.rm=TRUE)
        maxVal <- max(m)
        mts <- RV$currentTS
        colnames(mts) <- str_remove(colnames(mts), 'D_')
        
        RV$currentChart <- dygraph(mts,  ylab =  RV$currentSiteInfo$DataType, group = 'Main') %>%
          dyAxis("y", label ='Volumetric Moisture (mm)', valueRange = c(0, maxVal)) %>%
          
          dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = T, titleHeight = 26) %>%
          dyOptions(colors = RColorBrewer::brewer.pal(11, "Paired"), stackedGraph=T, retainDateWindow = TRUE) %>%
          dyEvent( dt, label = dt, labelLoc = c("top"), color = "black", strokePattern = "solid") %>%
          dyLegend(show = "follow", width = 250, showZeroValues = TRUE, labelsDiv = NULL, labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) #%>%

      })
    }
    
  })
  
  
  output$volTSChart <- renderDygraph({
    RV$currentChart
})
  
  output$rainTSChart <- renderDygraph({
    

    req(RV$currentRainTS)
    dt <- as.Date(input$datePker)

    yr <- year(dt)
    isolate({
      colnames(RV$currentRainTS)<-'Rain'
      dygraph(RV$currentRainTS,  ylab = 'Rain (mm)', group = 'Main') %>% 
      dyBarChart() %>% 
      dyOptions(colors = 'blue', retainDateWindow = TRUE) %>% 
      dyEvent( dt, label = dt, labelLoc = c("top"), color = "black", strokePattern = "solid") %>%
      dyRangeSelector(dateWindow = c(paste0(yr, "-01-01"), paste0(2021, "-08-31")), height = 20)  
    })
  })
  
  output$seasons <- renderDygraph({
    req(RV$currentTS)
    dt <- as.Date(input$datePker)
    isolate({
      sts <- getTotalYearlyTS(RV$currentTS)
      
      
      dygraph(sts,  ylab = 'Seasonal Soil Moisture (mm)', group = 'Seasons') %>% 
        dyOptions(colors = RColorBrewer::brewer.pal(ncol(sts), "Set1"), stackedGraph=F, strokeWidth=2) %>% 
      dyAxis("x",valueFormatter=JS(getMonthDay), axisLabelFormatter=JS(getMonth))
        
    })
  })
  

  output$siteMap <- renderLeaflet({
    req(input$probeName)
    
    rec <- probeInfo[probeInfo$ProjectSiteName == input$probeName, ]
    
    lon <- rec$Longitude
    lat <- rec$Latitude
    leaflet::leaflet() %>% addProviderTiles('Esri.WorldImagery') %>%
      setView(lon, lat, zoom = 7)  %>%
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      addMarkers(lon, lat)
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
