library(shiny)
library(move)
library(sp)
library(raster)
library(viridis)
library(mapview)
library(leaflet)
library(leaflegend)
library(shinycssloaders)
# webshot::install_phantomjs()

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Interactive Density Map"),
    fluidPage(
      fluidRow(class = "myRow1"), #we give a tag to the row to add a style below
      sidebarLayout(
        
        sidebarPanel(verticalLayout(
          selectInput(inputId = ns("var"), 
                      label = "Variable to rasterize", 
                      choices = list( "N. of GPS locations" = "n_locations", 
                                      "N. of individuals" = "n_individuals", 
                                      "N. of species" = "n_species", 
                                      "N. of Movebank studies" = "n_studies"),
                      selected = "n_locations"),
          
          sliderInput(inputId = ns("pxSize"), 
                      label = "Raster pixel size (degrees)", 
                      value = 0.05, min = 0.01, max = 5), # range in deg, from about 1 km to 500 km
          
          checkboxInput(inputId = ns("reverse"), 
                        label = "Reverse color palette", 
                        value = FALSE), #by default false
        ), width = 2),
        
        mainPanel(withSpinner(leafletOutput(ns("leafmap"), height="82vh"),type=6, size=2),
                  actionButton(ns('savePlot'), 'Save Plot'),
                  # downloadButton(ns('savePlot'), 'Save Plot')
                  width = 10)
      ), tags$head(tags$style(".myRow1{height:25px;background-color: white;}"))
    )
  )
}

shinyModule <- function(input, output, session, data) {
  current <- reactiveVal(data) 
  
  rmap <- reactive({
    SP <- SpatialPointsDataFrame(coords=coordinates(data), 
                                 data=as.data.frame(data), 
                                 proj4string=CRS("+proj=longlat +ellps=WGS84 +no_defs"))
    SP$rowNum <- 1:nrow(SP)
    rr <- raster(ext=extent(SP), resolution=input$pxSize, crs=CRS("+proj=longlat +ellps=WGS84 +no_defs"), vals=NULL)
    
    if(input$var=="n_locations"){
      SPr <- rasterize(SP, rr, field="rowNum", fun="count", update=TRUE)
      legendTitle <- "N. of GPS locations"
    }else if(input$var=="n_individuals"){
      SPr <- rasterize(SP, rr, field="individual.local.identifier", fun=function(x, ...){length(unique(na.omit(x)))}, update=TRUE)
      legendTitle <- "N. of individuals"
    }else if(input$var=="n_species"){
      SPr <- rasterize(SP, rr, field="individual.taxon.canonical.name", fun=function(x, ...){length(unique(na.omit(x)))}, update=TRUE)
      legendTitle <- "N. of species"
    } else if(input$var=="n_studies"){
      SPr <- rasterize(SP, rr, field="study.id", fun=function(x, ...){length(unique(na.omit(x)))}, update=TRUE)
      legendTitle <- "N. of Movebank studies"
    }
    bounds <- as.vector(bbox(extent(data)))
    SPr_l <- projectRasterForLeaflet(SPr, method = "ngb")
    
    if(max(values(SPr_l), na.rm=T) <= 7){
      myBins <- length(1:max(values(SPr_l), na.rm=T))
    }else{myBins <- 7}
    
    brewCol <- viridis(7)
    if(myBins == 7){
      rPal <- colorBin(brewCol, 1:max(values(SPr_l), na.rm=T), reverse = input$reverse,
                       na.color = "transparent", bins=myBins)
    }else{
      rPal <- colorFactor(brewCol[1:myBins], as.factor(1:max(values(SPr_l), na.rm=T)), reverse = input$reverse, 
                          na.color = "transparent")
    }
    
    outl <- leaflet() %>% 
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>% 
      addTiles() %>%
      addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addRasterImage(SPr_l, colors = rPal, opacity = 0.7, project = FALSE, group = "raster") %>%
      addScaleBar(position="bottomright",
                  options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
      addLayersControl(
        baseGroups = c("TopoMap","Aerial"),
        overlayGroups = "raster",
        options = layersControlOptions(collapsed = FALSE)) #%>%
    # addLegend(position="topright", opacity = 0.6,
    #              pal = rPal, values = values(SPr_l), title = legendTitle)
    
    if(input$var=="n_locations"){
      outl <- outl %>%
        addLegend(position="topright", opacity = 0.6, #bins = myBins, 
                  pal = rPal, values = 1:max(values(SPr_l), na.rm=T), title = legendTitle)
    }else{
      outl <- outl %>%
        addLegend(position="topright", opacity = 0.6, 
                  pal = rPal, values = as.factor(1:max(values(SPr_l), na.rm=T)), title = legendTitle)
    }
    
    outl   
  })
  
  
  output$leafmap <- renderLeaflet({
    rmap()  
  })  
  
  # ## save plot to moveapps output folder to be able to link it with API.
  # ## This would be the best option but in moveapps it does not work at the moment.
  # ## once shiny can save settings on moveapps, figure out how to save automatically, without hitting the save plot button
  # observeEvent("savePlot", {
  #   mymap <- rmap()
  #   mapshot( x = mymap
  #            , remove_controls = c("zoomControl","layersControl")
  #            , file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"DensityMap.png")
  #            , cliprect = "viewport"
  #            , selfcontained = FALSE)
  # })
  
  ### save map, takes some seconds ### here user can choose directory
  output$savePlot <- downloadHandler(
    filename = function() {
      paste("Leaflet_densityMap.png", sep="")
    },
    content = function(file) {
      leafmap <- rmap()
      mapshot( x = leafmap
               , remove_controls = "zoomControl"
               , file = file
               , cliprect = "viewport"
               , selfcontained = FALSE)
    }
  )
  return(reactive({ current() }))
}


