library(shiny)
library(shinythemes)
library(shinydashboard)
library(leaflet)
library(mapview)
library(raster)
library(RSAGA)
library(maptools)
library(rgdal)
library(sp)
library(RColorBrewer)
library(ggplot2)
library(rmarkdown)
library(sf)
library(dplyr)
library(caret)
library(ROCR)
library(viridis)
library(webshot)
library(plotKML)
library(colorRamps)
library(png)


rm(list=ls())
## rm(params) 
wd <- setwd("/srv/shiny-server/shalstabcv")   #getwd()
# wd <- setwd("/home/peterg/Peter/APPPPPPP/shalsatb")
## setwd(wd)
roctitle <<- ""
cmtitle <<- ""
outfile <<- tempfile(fileext='.png')
### https://rich.shinyapps.io/regression/
######################################### FUNCTION ###############################################
draw_confusion_matrix <- function(cm) {
  ## https://www.dataschool.io/simple-guide-to-confusion-matrix-terminology/
  
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('', cex.main=2)
  
  # create the matrix 
  rect(130, 430, 215, 380, col='#3F97D0')
  text(170, 435, 'Presence', cex=1.4)
  rect(220, 430, 305, 380, col='#F7AD50')
  text(265, 435, 'Absence', cex=1.4)
  text(105, 370, 'Actual', cex=1.6, srt=90, font=2)
  text(215, 450, 'Predicted', cex=1.6, font=2)
  rect(130, 325, 215, 375, col='#F7AD50')
  rect(220, 325, 305, 375, col='#3F97D0')
  text(120, 400, 'Presence', cex=1.4, srt=90)
  text(120, 345, 'Absence', cex=1.4, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  nall <- res[1] + res[2] + res[3] + res[4]
  text(170, 405, res[1], cex=1.6, font=2, col='white')
  text(170, 350, res[3], cex=1.6, font=2, col='white')
  text(265, 405, res[2], cex=1.6, font=2, col='white')
  text(265, 350, res[4], cex=1.6, font=2, col='white')
  text(320, 405, res[1] + res[2], cex=1.8, font=2, col='black')
  text(320, 350, res[3] + res[4], cex=1.8, font=2, col='black')
  text(170, 310, res[1] + res[3], cex=1.8, font=2, col='black')
  text(265, 310, res[2] + res[4], cex=1.8, font=2, col='black')
  text(315, 310, paste("n = ", nall), cex=1.8, font=2, col='black')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.4, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.4, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.4, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.4, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.4, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.6, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.6, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  
###############################################################################################

# td <- tempdir()

# setwd(td)

myrast <- c("Slope", "Catchement", "Continuous Critical Recharge (CR)", "Classified  Critical Recharge (CR)")
## Clean tmp directory -- will delete all files and folders older than 10 days crone
## sudo find /tmp -ctime +5 -exec rm -rf {} +


## setwd("/home/peterg/Peter/ShyniApps/pp")

# setwd("C:/Peter/shinyapps/pp")

## env = rsaga.env(path = "C:/Progra~1/SAGA-GIS")
env <- rsaga.env()

ui = navbarPage("Shallow Slope Stability", theme = shinytheme("flatly"),
                tabPanel("Modeling",
                         sidebarLayout(
                           sidebarPanel(width = 3,
                                        
                                        ##tags$hr(),
                                        ## tags$style(HTML('#q1 {margin-top: 30px}')),
                                        splitLayout(
                                          tags$div(
                                            fileInput("file1", "Upload Digital Elevation Model (GeoTIFF)",
                                                      multiple = FALSE,
                                                      accept = ".tif",
                                                      placeholder = "Max file size 10Mb")
                                          ),
                                          actionButton('infoTIFF', '', icon("info")),
                                          cellWidths = c("85%", "15%"),
                                          cellArgs = list(style = "vertical-align: middle")
                                        ),
                                        
                                        downloadLink('downloadData', class = "btn", icon("download"), 'Example Data Download'),
                                        # tags$a(href = "/home/peterg/Downloads/example.zip", class = "btn", icon("download"), 'Example Data'),
                                        # downloadButton("downloadData", label = "Download"),
                                        
                                        
                                        
                                        # value = range (0.0, 0.25)
                                        sliderInput("density", "Density (g/cm3)", min = 1.2, max = 2.7,
                                                    value = 1.6, step = 0.01),
                                        sliderInput("condact", "Conductivity (m/hr)", min = 0.0, max = 10.0,
                                                    value = 2.7, step = 0.05),
                                        sliderInput("frict", "Friction angle (degree)", min = 20.0, max = 50.0,
                                                    value = 33.0, step = 0.5),
                                        sliderInput("thickness", "Thickness (m)", min = 0.2, max = 3.0,
                                                    value = 1.0, step = 0.05),
                                        sliderInput("cohesion", "Bulk cohesion (MPa)", min = 0.0, max = 0.5,
                                                    value = 0.0, step = 0.001),
                                        
                                        # CoI credit tag
                                        HTML("<hr>"),
                                        ## checkboxInput("cont", HTML(paste(h5("Continuous Critical Recharge (CR)"))), value = FALSE),
                                        
                                        # Input: Select a dataset ----
                                        ## selectInput("crvalue", "Choose a dataset:",
                                        ##             choices = c("Classified", "Relative Humidity")),
                                        sliderInput("transpcol", "Transparency:", min = 0.0, max = 1.0, value = 0.7, step = 0.1),
                                        div(style = "padding: 10px;",
                                            helpText("Powered by", a("Geoinformatics Lab (GIL)",
                                                                     href = "https://geogis.bgsu.edu/",
                                                                     target = "_blank")))
                           ),
                           mainPanel(
                             ## h2("Victim tab")
                             #####################################################################
                             ## h3("This tab has a sidebar")
                             fluidRow(
                               tags$head(tags$style("#myMap{height:90vh !important;}")),
                               leafletOutput("mymap",height = 800),
                               
                               
                               ######################################
                               # Shiny versions prior to 0.11 should use class = "modal" instead.
                               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                             width = 330, height = "auto", style = "opacity: 0.7",
                                             selectInput("selrast", "Choose a layer:", choices = myrast, selected = myrast[4]),
                                             h2(""),
                                             
                                             
                                             plotOutput("hist", height = 250)
                               )
                               #######################################
                               # plotOutput("plot1", height=500)
                               # leafletOutput("myMap")
                               
                             ),
                             
                             fluidRow(
                               box(
                                 
                                 title = "", status = "warning", solidHeader = TRUE, width="100%",
                                 plotOutput("", height=1)
                                 # div(style = "height:2px;")
                                 
                               )
                             )
                             #####################################################################
                           )
                         )
                ),
                tabPanel("Validation",
                         sidebarLayout(
                           sidebarPanel(width = 3,
                                        ## tags$hr(),
                                        # Input: Select a file ----
                                        # tags$hr(),
                                        helpText("Input validation landslides data from the same study area.
                                                 First, upload the GeoTIFF file in the Modelling Tab!"),
                                        
                                        tags$style(HTML('#q1 {margin-top: 30px}')),
                                        splitLayout(
                                          tags$div(
                                            fileInput("csvFile", "Choose CSV File",
                                                      accept = c(
                                                        "text/csv",  
                                                        "text/comma-separated-values,text/plain",
                                                        ".csv"))
                                          ),
                                          actionButton('infoCSV', '', icon("info")),
                                          cellWidths = c("85%", "15%"),
                                          cellArgs = list(style = "vertical-align: middle")
                                        ),
                                        sliderInput("threshold", "CR Values Threshold", min = 0, max = 400,
                                                    value = 200, step = 1),
                                        HTML('</br>'),
                                        
                                        radioButtons('report_format', 'Generate report:', c('PDF', 'HTML', 'Word'), inline = TRUE),
                                        downloadButton('downloadReport'),
                                        
                                        ## includeHTML('C:/Downloads/Slides/help.html')
                                        HTML('<hr>'),
                                        h5("Save CR Value as GeoTIFF:"),
                                        downloadButton('downloadGeoTIFF'),
                                        HTML('<hr>'),
                                        h5("Save CR Value as KML:"),                                        
                                        downloadButton("downloadDataGEP", label = "Download")
                                        
                                        #####################################################################
                                        ##h3("This tab has a sidebar")
                                        #####################################################################
                                        ),
                           mainPanel(
                             ## h2("Trafficker tab"),
                             ## leafletOutput(outputId = "map")
                             fluidRow(
                               column(12,
                                      leafletOutput("SLplot", height = 500)
                               )),
                             fluidRow(
                               column(7,
                                      br(),
                                      h3(textOutput("cmtitle"),align = "center"),
                                      plotOutput("cm", height = 500)
                               ),
                               column(5,
                                      br(),
                                      ## h3("ROC Curve", align = "center"),
                                      h3(textOutput("roctitle"),align = "center"),
                                      plotOutput("roc",height = 500)
                               ))
                             ## verbatimTextOutput("summary")
                             ## tableOutput("table")
                             ## to require that the user types something, use: `req(input$data)`
                             ## but better: require that input$data is valid and leave the last
                             ## valid table up
                             ## req(exists(input$data, "package:datasets", inherits = FALSE),                                 cancelOutput = TRUE)
                             
                           )
                         )
                )
                )
# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 10*1024^2)
  dem <- tempfile()
  # print("###############DEMMMMMM###########################")
  # print(dem)
  
  observeEvent(input$infoTIFF, {
    showModal(modalDialog(
      title = "Important message",
      "The required format for the Digital Elevation Model (DEM) 
      is a raster GeoTIFF with WGS84/UTMzone projection. 
      The size of the file is limited to Max file size of 10Mb!"
    ))
  })
  
  observeEvent(input$infoCSV, {
    showModal(modalDialog(
      title = "Important message",
      "The required format for the landslides is comma separated 
      value (CSV) using coordinates that have the same WGS84/UTMzone 
      projection as the GeoTIFF.  The required columns and order 
      are: ID, X, Y, slides. The 'slides' column represents the scarp 
      and requires binary value 1 (presence) or 0 (absence).    
      The size of the file is limited to Max file size of 5Mb!"
    ))
  })
  
  observeEvent(input$file1, {
    #if (is.null(input$file1)) return(NULL)
    # isolate({
    #    inFile <- input$file1$datapath
    #  })
    inFile <- input$file1$datapath
    rsaga.import.gdal(inFile, paste(dem, ".sgrd", sep=""), env = rsaga.env())
    ## Create MASK
    ## rsaga.grid.calculus((paste(dem, ".sdat", sep="")), paste(dem, "_Mask.sdat", sep=""), "a / a")
    ## maskR <- raster(paste(dem, "_Mask.sdat", sep=""))
    ## plot(maskR)
    
  })
  
  # compute the slope
  shalstabmodINPUT <- reactive({
    
    req(input$file1)
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    # 
    progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...') 
    
    
    
    slp <- tempfile()
    rsaga.slope(in.dem = paste(dem, ".sgrd", sep=""),
                out.slope = paste(slp, "_Raw.sgrd", sep=""),
                method = "poly2zevenbergen", env = env)
    
    ### CORRECT SLOPE -- FLAT AREAS
    rsaga.grid.calculus(paste(slp, "_Raw.sgrd", sep=""), paste(slp, ".sgrd", sep=""), "ifelse(a = 0, 0.0001, a)")
    ## rSlope <- raster(paste(slp, ".sdat", sep=""))
    ## plot(rSlope)
    
    # compute the catchement
    catch <- tempfile()
    rsaga.topdown.processing(paste(dem, ".sgrd", sep=""), out.carea = paste(catch, ".sgrd", sep=""), method = "dinf", env = env)
    
    for (i in 1:15) {
      progress$set(value = i)
      Sys.sleep(0.1)
    }
    
    # create raster stack
    rSlope <- raster(paste(slp, ".sdat", sep=""))
    rCatch <- raster(paste(catch, ".sdat", sep=""))
    
    
    ## rMASK <- raster(paste(dem, "_Mask.sdat", sep=""))
    
    INRasters <- stack(rSlope, rCatch)
    return(INRasters)
  })
  
  shalstabMODEL <- reactive({  
    
    req(input$file1)
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    # 
    progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...') 
    
    rSIN <- shalstabmodINPUT()
    
    rSlope <- subset(rSIN, 1)
    rCatch <- subset(rSIN, 2)
    # rMsk <- subset(rSIN, 3)
    
    rSl <- rSlope@file@name
    rCa <- rCatch@file@name
    # rMk <- rMsk@file@name
    
    # compute the slope stability
    crv <- tempfile()
    crc <- tempfile()
    rsaga.geoprocessor(lib="ta_slope_stability", module=2,
                       param=list(A = paste(rSl),
                                  B = paste(rCa),
                                  fCmin=input$density,
                                  fCmax=input$density,
                                  fDmin=input$condact,
                                  fDmax=input$condact,
                                  fEmin=input$thickness,
                                  fEmax=input$thickness,
                                  fFmin=input$frict,
                                  fFmax=input$frict,
                                  fJmin=input$cohesion,
                                  fJmax=input$cohesion,
                                  fK=1,
                                  G=paste(crv, ".sgrd", sep=""),
                                  H=paste(crc, ".sgrd", sep="")),env = env)
    
    
    rcrv <- raster(paste(crv, ".sdat", sep=""))
    # rcrc <- raster(paste(crc, ".sdat", sep=""))
    
    # Get max vfrom CRV
    rcrv <- setMinMax(rcrv)
    maxVal <- maxValue(rcrv)[[1]] 
    
    
    rsaga.geoprocessor(lib="grid_tools", module=15,
                       param=list(INPUT = paste(crv, ".sdat", sep=""),
                                  RESULT = paste(crv, "_Cor.sdat", sep=""),
                                  METHOD = 0,
                                  OLD = -99999,
                                  NEW = maxVal,
                                  SOPERATOR = 0))
    
    rsaga.grid.calculus((paste(crc, ".sdat", sep="")), paste(crc, "_Mask.sdat", sep=""), "a / a")
    rCRC <- raster(paste(crc, ".sdat", sep=""))
    # r.CRC <- rCRC@file@name
    # maskR <- raster(paste(rMk, "_Mask.sdat", sep="")) 
    
    ###    rsaga.geoprocessor("grid_calculus", 1, list(INPUT="grid1.sgrd", 
    ###                 RESULT="grid2.sgrd", FORMUL="a*0.4"))
    
    ## MM <- raster(rMk)
    ## CRVN <- raster(paste(crv, "_Cor.sdat", sep=""))
    rsaga.grid.calculus(c(paste(crv, "_Cor.sdat", sep=""), paste(crc, "_Mask.sdat", sep="")), paste(crv, "_Final.sdat", sep=""), ~(a * b))
    rCRV <- raster(paste(crv, "_Final.sdat", sep=""))
    # r.CRV <- rCRV@file@name
    
    ############################
    #rndu <- paste("rand_u(",maxVal,",", maxVal*3.0,")", sep="")
    #rsaga.grid.calculus((paste(crv, "_Final.sdat", sep="")), paste(crv, "_Rnd.sdat", sep=""), paste(rndu))
    # 
    #eqrnd <- paste0("ifelse(lt(a,", maxVal ,"),a, b)", sep="")
    #rsaga.grid.calculus(c(paste(crv, "_Final.sdat", sep=""), paste(crv, "_Rnd.sdat", sep="")), paste(crv, "_MaxVals.sdat", sep=""), eqrnd)
    # 
    #rCRV <- raster(paste(crv, "_MaxVals.sdat", sep=""))
    ###########################
    
    for (i in 1:15) {
      progress$set(value = i)
      Sys.sleep(0.1)
    }
    
    # create raster stack
    allRaster <- stack(rSlope, rCatch, rCRV, rCRC)
    return(allRaster)
  })
  
  #########################################################################
  #LEAFLET
  ###############################################################################
  
  output$mymap <- renderLeaflet({
    
    rS <- shalstabMODEL()
    
    if (input$selrast == "Slope") {
      r <- subset(rS, 1)
      #crs(r) <- projection(r)
    } else if (input$selrast == "Catchement") {
      r <- subset(rS, 2)
      #crs(r) <- projection(r)
    } else if (input$selrast == "Continuous Critical Recharge (CR)") {
      r <- subset(rS, 4)
      #crs(r) <- projection(r)
    }else {
      r <- subset(rS, 3)
      #crs(r) <- projection(r)
    }
    # Set raster and projection
    rst <- r
    crs(rst) <- projection(r)
    # pal <- colorpalCRC()
    
    leaflet(df) %>% 
      clearTiles() %>%
      addRasterImage(rst, opacity = input$transpcol)
    
  })
  
  ########################################################################
  # ColPal Slope
  #####################################################################
  colorpalSlope <- reactive({
    rS <- shalstabMODEL()
    if (input$selrast == "Slope") {
      r <- subset(rS, 1)
      Rminmax <- setMinMax(r)
      vals1 <- as.numeric(c((minValue(Rminmax) - 1):(maxValue(Rminmax) + 1)))
      cols1 <- c( "#ff0000","#ff8040","#ffff00","#00ff00","#80ffff","#0000ff","#808080")
      pal <- colorNumeric(cols1, vals1, na.color = "transparent")
    }
  }) 
  
  ########################################################################
  # ColPal Catch
  #####################################################################  
  colorpalCatch <- reactive({
    rS <- shalstabMODEL()
    if (input$selrast == "Catchement") {
      r <- subset(rS, 2)
      r <- log(r)
      Rminmax <- setMinMax(r)
      vals1 <- as.numeric(c((minValue(Rminmax) - 1):(maxValue(Rminmax) + 1)))
      ## cols1 <- c( "#ff0000","#ff8040","#ffff00","#00ff00","#80ffff","#0000ff","#808080")
      ## pal <- colorNumeric(cols1, vals1, na.color = "transparent")
      ##colfunc <- colorRampPalette(c("#ff0000","#ff8040","#ffff00","#00ff00","#80ffff","#0000ff", "#808080", "#404040"))
      
      pal <- colorNumeric(palette = "viridis", vals1, na.color = "transparent", reverse = TRUE)
    }
    
  }) 
  
  ########################################################################
  # ColPal CRV
  #####################################################################  
  colorpalCRV <- reactive({
    rS <- shalstabMODEL()
    if (input$selrast == "Continuous Critical Recharge (CR)") {
      r <- subset(rS, 3)
      Rminmax <- setMinMax(r)
      
      if (maxValue(Rminmax) > 0) {
        range <- maxValue(Rminmax) - minValue(Rminmax)
        rng <- range / 10
        cuts <- seq(1,range, rng)
        
        colfunc <- colorRampPalette(c("#ff0000","#ff8040","#ffff00","#00ff00","#80ffff","#0000ff", "#808080", "#404040"))
        
        pal <- colorNumeric(palette = colfunc(50), cuts, na.color = "transparent", reverse = FALSE)
        #
      } else {
        pal <- colorBin("Greens", domain = 0:100, na.color = "transparent")
      }
      
    }
    
  }) 
  
  ########################################################################
  # ColPal CRC
  #####################################################################  
  colorpalCRC <- reactive({
    rS <- shalstabMODEL()
    if (input$selrast == "Classified  Critical Recharge (CR)") {
      r <- subset(rS, 4)
      cols <- c("#ff0000","#ff8040","#ffff00","#00ff00","#80ffff","#0000ff", "#808080")
      vals1 <- c("Uncond. unstable", "0-50 mm/day", "50-100 mm/day", "100-200 mm/day", "200-400 mm/day",">400 mm/day", "Uncond. stable" )
      
      pal <- colorFactor(cols, domain = 1:7,  na.color = "transparent")
      
    }
    
  }) 
  
  #########################################################################
  #LEAFLET PROXY Layer Slope
  ###############################################################################
  observe({
    rS <- shalstabMODEL()
    
    if (input$selrast == "Slope") {
      pal <- colorpalSlope()
      rst <- subset(rS, 1)
      crs(rst) <- projection(rst)
      
      leafletProxy("mymap", data=rst) %>%
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$OpenTopoMap, group = "Topo Map") %>%
        addRasterImage(rst, colors = pal, opacity = input$transpcol, group = "Slope") %>%
        addLayersControl(
          baseGroups = c("OSM", "Topo Map"),
          position = "topleft",
          overlayGroups = c("Slope"),
          options = layersControlOptions(collapsed = TRUE)) %>%
        addLegend("bottomright",
                  pal = pal, 
                  values = values(rst),
                  title= "Slope (radians)",
                  opacity = 1) 
      
      
    }
  })
  
  
  #########################################################################
  #LEAFLET PROXY Layer Catch
  ###############################################################################
  observe({   
    rS <- shalstabMODEL()
    
    if (input$selrast == "Catchement") {
      pal <- colorpalCatch()
      rst <- subset(rS, 2)
      rst <- log(rst)
      crs(rst) <- projection(rst)
      
      leafletProxy("mymap", data=rst) %>%
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$OpenTopoMap, group = "Topo Map") %>%
        addRasterImage(rst, colors = pal, opacity = input$transpcol, group = "Catchement") %>%
        addLayersControl(
          baseGroups = c("OSM", "Topo Map"),
          position = "topleft",
          overlayGroups = c("Catchement"),
          options = layersControlOptions(collapsed = TRUE))  %>%
        addLegend("bottomright",
                  pal = pal, 
                  values = values(rst),
                  title= "Catchement (log(m2))",
                  opacity = 1)
      
    }   
    
  })
  
  #########################################################################
  #LEAFLET PROXY Layer CRV
  ###############################################################################
  observe({   
    rS <- shalstabMODEL()
    
    if (input$selrast == "Continuous Critical Recharge (CR)") {
      pal <- colorpalCRV()
      #print(pal)
      ## colfunc <- colorRampPalette(c("#ff0000","#ff8040","#ffff00","#00ff00","#80ffff","#0000ff", "#808080", "#404040"))
      rst <- subset(rS, 3)
      crs(rst) <- projection(rst)
      
      leafletProxy("mymap", data=rst) %>%
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$OpenTopoMap, group = "Topo Map") %>%
        addRasterImage(rst, colors = pal, opacity = input$transpcol, group = "Continuous CR") %>%
        addLayersControl(
          baseGroups = c("OSM", "Topo Map"),
          position = "topleft",
          overlayGroups = c("Continuous CR"),
          options = layersControlOptions(collapsed = TRUE)) %>%
        addLegend("bottomright",
                  pal = pal, 
                  values = values(rst),
                  title= "CR Values",
                  opacity = 1)
      
    }   
    
  })
  
  #########################################################################
  #LEAFLET PROXY Layer CRC
  ###############################################################################
  observe({   
    rS <- shalstabMODEL()
    cols <- c("#ff0000","#ff8040","#ffff00","#00ff00","#80ffff","#0000ff", "#808080")
    vals1 <- c("Uncond. unstable", "0-50 mm/day", "50-100 mm/day", "100-200 mm/day", "200-400 mm/day",">400 mm/day", "Uncond. stable" )
    
    if (input$selrast == "Classified  Critical Recharge (CR)") {
      pal <- colorpalCRC()
      # pal <- colorFactor(cols, domain = 1:7,  na.color = "transparent")
      rst <- subset(rS, 4)
      crs(rst) <- projection(rst)
      rat <- levels(rst)[[1]]
      rat[["CRClass"]] <- c("No data", "Uncond. unstable", "0-50 mm/day", "50-100 mm/day", "100-200 mm/day", "200-400 mm/day",">400 mm/day", "Uncond. stable" )
      df <- as.data.frame(rat)  
      rst <-  as.factor(rst)
      rst[rst == 0] <- NA
      
      
      leafletProxy("mymap", data=rst) %>%
        clearControls() %>%
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$OpenTopoMap, group = "Topo Map") %>%
        addRasterImage(rst, colors = pal, opacity = input$transpcol, group = "Classified CR") %>%
        addLayersControl(
          baseGroups = c("OSM", "Topo Map"),
          position = "topleft",
          overlayGroups = c("Classified CR"),
          options = layersControlOptions(collapsed = TRUE)) %>%
        addLegend("bottomright",
                  colors = cols,
                  labels= vals1,
                  title= "Slope Stability",
                  opacity = 1)
      
    }   
    
  }) 
  
  
  #########################################################################
  #ROC CURVES
  ############################################################################### 
  
  
  output$hist <- renderPlot({
    # If no zipcodes are in view, don't plot
    # if (nrow(zipsInBounds()) == 0)
    #   return(NULL)
    
    rS <- shalstabMODEL()
    # r <- subset(rS, 3)
    
    vals1 <- c("No data", "Uncond. unstable", "0-50 mm/day", "50-100 mm/day", "100-200 mm/day", "200-400 mm/day",">400 mm/day", "Uncond. stable" )
    vals <- seq(0,8,1)
    cols <- c("#FFFFFF","#ff0000","#ff8040","#ffff00","#00ff00","#80ffff","#0000ff", "#808080")
    # cols <- c( "#ffffff", "#ff0000","#ff8040","#ffff00","#00ff00","#80ffff","#0000ff","#808080")
    
    #  myrast Rminmax<- c("Slope", "Catchement", "Slope Stability")
    if (input$selrast == "Slope") {
      r <- subset(rS, 1)
      mm <- "Slope Layer"
      xl <- "Slope (radians)"
      print(r)
      hist(r,
           main = mm,
           xlab = xl,
           # xaxt="n",
           # xlim = range(allzips$centile),
           col = '#00DD00',
           border = 'white')
      # names(s) <-c(0, "January")
    } else if (input$selrast == "Catchement") {
      r <- subset(rS, 2)
      mm <- "Catchement Layer"
      xl <- "log(Area (m2))"
      hist(log(r),
           main = mm,
           xlab = xl,
           # xlim = range(allzips$centile),
           col = '#00DD00',
           border = 'white')
      # names(s) <-c(1, "February")
    } else if (input$selrast == "Continuous Critical Recharge (CR)") {
      r <- subset(rS, 3)
      Rminmax <- setMinMax(r)
      
      valsMMax <- as.numeric(maxValue(Rminmax))
      mm <- "CR Layer"
      xl <- "Continuous CR (m/day)"
      density(r,
              main = mm,
              xlab = xl,
              xlim = c(0,(valsMMax*0.8)),
              # xaxt="n",
              # xlim = range(allzips$centile),
              col = '#00DD00',
              border = 'white')
      # names(s) <-c(0, "January")
      
    } else {
      
      vals1 <- c("No data", "Uncond. unstable", "0-50 mm/day", "50-100 mm/day", "100-200 mm/day", "200-400 mm/day",">400 mm/day", "Uncond. stable" )
      cols <- c("#FFFFFF","#ff0000","#ff8040","#ffff00","#00ff00","#80ffff","#0000ff", "#808080")
      # pal colorFactor(cols, na.color = "transparent", alpha = TRUE, domain = NULL )
      
      r <- subset(rS, 4)
      zz <- freq(r)
      counts <- as.data.frame(zz)
      
      iddf <- data.frame(value=c(0, 1, 2, 3, 4, 5, 6, 7))
      
      dfc <- join(iddf, counts, type = "full")
      dfc[c("value", "count")][is.na(dfc[c("value", "count")])] <- 0
      
      counts <- dfc
      
      
      counts["name"] <- vals1
      
      midpts <- barplot(counts$count, main="Slope stability classes",beside=TRUE, names.arg="", col=cols)
      text(counts$name,
           x = midpts,
           offset = -0.2,
           y = -25,
           cex = 0.85,
           srt = 60,
           adj= 1,
           xpd = TRUE,
           pos = 2 )
    }
    
  })
  
  
  #######################################################################
  ########################################################################
  ## output$dfslides <- renderTable({
  
  
  dfslides <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$csvFile)
    
    
    df <- read.csv(input$csvFile$datapath,
                   sep = ",",
                   quote = input$quote)
    
    df0 = data.frame(df$X, df$Y, df$slides)
    df = unique(df0)
    names(df)[1] = 'X'
    names(df)[2] = 'Y'
    names(df)[3] = 'slides'
    ## print(df)
    return(df)
    
    
  })
  
  
  ## output$table <- renderTable(dfslides())
  
  
  SlidesMap <- reactive({
    
    req(input$csvFile)
    
    rS <- shalstabMODEL()
    rCRV <- subset(rS, 3)
    projdem <- projection(rCRV)
    ## print(projdem)
    
    slides01 <- dfslides()
    coordinates(slides01) <- c("X", "Y")
    proj4string(slides01) <- projdem  
    ## print(slides01)
    
    llslides01 <- spTransform(slides01, CRS("+init=epsg:4238"))
    
    palfunc <- function (n, alpha = 1, begin = 0, end = 1, direction = 1)
    {
      colors <- RColorBrewer::brewer.pal(11, "RdBu")
      if (direction < 0) colors <- rev(colors)
      colorRampPalette(colors, alpha = alpha)(n)
    }
    
    
    
    ## cntr_crds <- c(mean(coordinates(llslides01)[, 1]),
    ##               mean(coordinates(llslides01)[, 2]))
    ## m@map %>% setView(cntr_crds[1], cntr_crds[2], zoom = 14)
    
    mapviewOptions(mapview.maxpixels =  600000)
    m1 <- mapview::mapview(rCRV, map.types = c("Esri.WorldShadedRelief", "OpenStreetMap.DE"), layer.name = "CR", col.regions = palfunc)
    m2 <- mapview::mapview(slides01, zcol = "slides",  
                           burst = TRUE, col.regions = c("red", "green"), 
                           legend = TRUE, layer.name = c("Absence", "Presence"))
    
    m <- m1 + m2
  })
  
  
  SlidesMV <- reactive({
    
    req(input$csvFile)
    
    rS <- shalstabMODEL()
    rCRV <- subset(rS, 3)
    projdem <- projection(rCRV)
    
    ##cat(paste("extent     :", round(rS@extent@xmin, 2), ",", round(rS@extent@xmax, 2), ",", round(rS@extent@ymin, 2), ",", 
    ##          round(rS@extent@ymax, 2), "(xmin, xmax, ymin, ymax)"))
    
    slides01 <- dfslides()
    coordinates(slides01) <- c("X", "Y")    
    proj4string(slides01) <- projdem  
    llslides01 <- spTransform(slides01, CRS("+init=epsg:4238"))
    
    
    # Build a spatial dataframe with extent data from dtm.temp
    # df <- data.frame(ID = 1:2, X = c(rS@extent@xmin, rS@extent@xmax),
    #                  Y = c(rS@extent@ymin, rS@extent@ymax))
    # coordinates(df) <- c("X", "Y")
    # crs_text <- crs(rS, asText=TRUE) # extracting crs from dtm.temp 
    # proj4string(df) <- CRS(crs_text) 
    # ext.lonlat <- spTransform(df, CRS("+proj=longlat +datum=WGS84"))
    # ext.lonlat
    
    xmin <- slides01@bbox[1,1]
    xmax <- slides01@bbox[1,2]
    ymin <- slides01@bbox[2,1]
    ymax <- slides01@bbox[2,2]
    
    if (xmax - xmin <= 5000) {
      zl = 13
    } else if ( xmax - xmin > 5000 & xmax - xmin <= 15000) {
      zl = 12
    } else if ( xmax - xmin > 15000 & xmax - xmin <= 30000) {
      zl = 11
    } else {
      zl = 10
    }
    
    cntr_crds <- c(mean(coordinates(llslides01)[, 1]),
                   mean(coordinates(llslides01)[, 2]))
    ##m@map %>% setView(cntr_crds[1], cntr_crds[2], zoom = 14)
    
    SlidesMap()@map %>% 
      setView(cntr_crds[1], cntr_crds[2], zoom = zl) # %>% 
    ## fitBounds(-123.8793, -123.5803, 44.4708, 44.64135)
  }) 
  
  
  output$SLplot <- renderLeaflet({
    SlidesMV()
  })
  
  ###############################
  # extract values with points
  ###############################
  
  
  SlidesCM <- reactive({
    ## output$table <- renderTable({
    
    ## extractPts <- reactive({
    req(input$csvFile)
    
    rS <- shalstabMODEL()
    print(rS)
    rCRV <- subset(rS, 3)
    projdem <- projection(rCRV)
    
    
    slides01 <- dfslides()
    coordinates(slides01) <- c("X", "Y")
    proj4string(slides01) <- projdem
    # X <- slides01$X
    # Y <- slides01$Y
    # slides <- as.matrix(as.factor(slides01$slides))
    print("(((((((((((((((((((((((((((((((((((((((((((((")
    print(slides01)
    
    xy <- cbind(slides01$X,slides01$Y)
    
    # xy <- coordinates(slides01)
    # print(slides01$X)
    val <- extract(rS, slides01)
    ## print(val)
    valtab <- data.frame(cbind(xy, val))
    ## Add the slides as factor
    dfsl01 <- data.frame(cbind(valtab, slides01$slides))
    colnames(dfsl01) <- c("X", "Y", "slope", "catch", "crv", "crc", "slides")
    
    threshold <- input$threshold
    
    # Filter the data and remove N/A
    slall <- dfsl01 %>%
      na.omit()
    print("*****************************************")
    
    
    slall$pred <- ifelse((slall$crv <= threshold), 1, 0)
    
    slall$slides <- as.numeric(as.character(slall$slides))
    slall$pres <- ifelse((slall$slides + slall$pred == 1), 0, 1)
    slall$abs <- as.factor(ifelse((slall$slides == 0 &  slall$pred == 0), 1, 0))
    
    return(slall)
  })
  
  ## slall1 <- slall[ which(slall$lsval <= threshold & slall$slides == 1),]
  ## slall0 <- slall[ which(slall$lsval <= threshold & slall$slides == 0),]
  
  ## slallP1 <- slall[ which(slall$lsval <= threshold & slall$pred == 1),]
  
  ##################################CONFUSION MATRIX ########################################################
  
  output$cm <- renderPlot({
    
    slall <-  SlidesCM()
    
    ## write.csv(slall, "test.csv")
    # calculate the confusion matrix
    cm <- confusionMatrix(data = as.factor(slall$pres), reference = as.factor(slall$slides))
    draw_confusion_matrix(cm)
    
    
  })
  
  
  ##################################### ROC ################################################################
  
  output$roctitle <- renderText({ 
    req(input$csvFile)
    ("ROC Curve") })
  
  
  output$cmtitle <- renderText({ 
    req(input$csvFile)
    ("Confusion Matrix") })
  
  
  
  output$roc <- renderPlot({
    
    specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
    
    slall <-  SlidesCM()
    threshold <- input$threshold
    slallroc <- slall[ which(slall$crv <= threshold),]
    
    pred_ROCR <- prediction(slallroc$crv, slallroc$slides, label.ordering = c(1, 0))
    auc_ROCR <- performance(pred_ROCR, measure = "auc")
    # auc_ROCR <- 1 - auc_ROCR@y.values[[1]]
    auc_ROCR <- auc_ROCR@y.values[[1]]
    auc_val <- paste("AUC = ", specify_decimal(auc_ROCR, 3)) 
    
    # roc_ROCR <- performance(pred_ROCR, measure = "fpr", x.measure = "tpr")
    roc_ROCR <- performance(pred_ROCR,"tpr", "fpr")
    plot(roc_ROCR, main = auc_val, colorize = T)
    abline(a = 0, b = 1)
    
    
  })
  
  #########################################################################
  # Generate Report
  #########################################################################
  
  output$downloadReport <- downloadHandler(
    
    filename = function() {
      paste0('report_', Sys.Date(), '.', switch(
        input$report_format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      print(wd)
      src <- "/srv/shiny-server/shalstabcv/index.Rmd"
      # src <- "/home/peterg/Peter/APPPPPPP/shalsatb/index.Rmd"
      ##src <- normalizePath('index.Rmd')
      owd <- setwd(tempdir(check=TRUE))
      on.exit(setwd(owd))
      file.copy(src, 'index.Rmd', overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      
      ls <- shalstabMODEL()
      slval <- SlidesCM()
      slcm <- SlidesCM()
      ms <- paste0(outfile)
      m <- SlidesMV()
      ## mapshot(m, file=ms, cliprect = "viewport", selfcontained = FALSE )
      
      
      params <- list(indens = input$density,incond = input$condact,
                     inthic = input$thickness, infric = input$frict,
                     incohe = input$cohesion, outshal = ls, invsl = slval,
                     ms = ms, plotSl = slcm, tresh = input$threshold,
                     cmt = cmtitle, roct = roctitle, of = outfile)
      
      library(rmarkdown)
      out <- rmarkdown::render(input = 'index.Rmd',
                               output_format = switch(
                                 input$report_format,
                                 PDF = pdf_document(),
                                 HTML = html_document(),
                                 Word = word_document()
                               ), params = params, 
                               envir = new.env(parent = globalenv())
                               
      )
      
      file.rename(out, file)
      
    })
  # Downloadable csv of selected dataset ----
  output$downloadGeoTIFF <- downloadHandler(
    
    filename = function() {
      paste("CRValue", ".tif", sep = "")
    },
    
    content = function(file) {
      ## r <- raster(system.file("external/test.grd", package="raster"))
      rS <- shalstabMODEL()
      r <- subset(rS, 3)
      res <- writeRaster(r, filename=file, format="GTiff", overwrite=TRUE)
      
      # Show the corresponding output filename
      print(res@file@name)
      
      # Rename it to the correct filename
      file.rename(res@file@name, file)
    }
  )
  
  # download handler
  output$downloadData <- downloadHandler(
    filename <- function() {
      paste("example", "zip", sep=".")
    },
    
    content <- function(file) {
      # file.copy("/home/peterg/shinyapp/data/example.zip", file)
      file.copy("data/example.zip", file)
    }
  )

######################################## KML ########################################  
  output$downloadDataGEP <- downloadHandler(
    filename = function() {
      paste("GEP_output", "zip", sep=".")
    },
    content = function(fname) {

      rS <- shalstabMODEL()
      r <- subset(rS, 3)
      names(r) <- "CRV"
      # r1 <- raster(r["CRV"])
      r1 <- r
      fs <- c()
      tmpdir <- tempdir()
      setwd(tempdir())
      mydir <- "/srv/shiny-server/shalstabcv/"
      kmlfile <- "CRV"
      kmllogo = "logo.png"
      logimg <- paste(mydir, kmllogo, sep="")
      #logimgtmp <- paste(tmpdir, "/", kmllogo, sep="")


      
      ############################# KML CRV ##########################################
      
      kml_open(paste(tmpdir, "/", kmlfile,  "_0.kml", sep=""), kml_visibility = TRUE, overwrite=TRUE)
      p <- projectRaster(r1, crs="+proj=longlat +datum=WGS84", method='ngb')
      KML(p,  col=rev(matlab.like(255)), file=paste(tmpdir, "/", kmlfile, "_0.kml", sep=""), overwrite=TRUE)
      kml_close(paste(tmpdir, "/", kmlfile,  "_0.kml", sep=""))
      
      ############################# KML LOGO ##########################################
      kml_open(paste(tmpdir,  "/", kmlfile, "_1.kml", sep=""), kml_visibility = TRUE, overwrite=TRUE)

      logo = "https://geogis.bgsu.edu/temp/R/logo.png"
      kml_screen(image.file = logo, position = "LL", sname = "SHALSTAB logo")
      #logoimg <- readPNG(logimg)
      #rasterImage(logoimg,1,1,6, 6)
      #dev.off()
      kml_close(paste(tmpdir,  "/", kmlfile,  "_1.kml", sep=""))
      
      fs <- c("CRV_0.kml", "CRV_0.kmz", "CRV_1.kml")
      
      
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )

  
  }


# Create Shiny app ----
shinyApp(ui, server)
