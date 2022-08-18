library(shiny)
library(move)
library(sp)
library(raster)
library(RColorBrewer)
library(mapview)
library(leaflet)


shinyModuleUserInterface <- function(id, label, entity = "n_locations", pxSize = 0.1) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Rasterize n. observations/individuals/species on leaflet map"),
    selectInput(inputId = ns("entity"), 
                label = "Choose which entity you want to rasterize", 
                choices = list( "N. of GPS locations" = "n_locations", 
                                "N. of individuals" = "n_individuals", 
                                "N. of species" = "n_species"), #, "N. of Movebank studies" = "n_studies"),
                selected = entity),
    sliderInput(inputId = ns("pxSize"), 
                label = "Choose the raster cell resolution in degrees", 
                value = pxSize, min = 0.01, max = 5), # range of about 1 km to 500 km
    leafletOutput(ns("leafmap"), height="70vh"),
    downloadButton(ns('savePlot'), 'Save Plot')
  )
}

# shinyModuleConfiguration <- function(id, input) { ## inclusion of this function is optional. To be used if one would like the user to set values of parameters before executing the shiny app. See user manual.
#   ns <- NS(id)
#   configuration <- list()
#   configuration
# }

shinyModule <- function(input, output, session, data, entity = "n_locations", pxSize = 0.1) {
  current <- reactiveVal(data) 
  
  rmap <- reactive({
    SP <- SpatialPointsDataFrame(coords=as.data.frame(data)[,c("location_long","location_lat")], 
                                 data=as.data.frame(data), 
                                 proj4string=CRS("+proj=longlat +ellps=WGS84 +no_defs"))
    SP$rowNum <- 1:nrow(SP)
    rr <- raster(ext=extent(SP), resolution=input$pxSize, crs =CRS("+proj=longlat +ellps=WGS84 +no_defs"), vals=NULL)
    
    if(input$entity=="n_locations"){
      SPr <- rasterize(SP, rr, field="rowNum", fun="count", update=TRUE) #why do we need update=T?
      legendTitle <- "N. of GPS locations"
    }else if(input$entity=="n_individuals"){
      SPr <- rasterize(SP, rr, field="local_identifier", fun=function(x, ...){length(unique(na.omit(x)))}, update=TRUE)
      legendTitle <- "N. of individuals"
    }else if(input$entity=="n_species"){
      SPr <- rasterize(SP, rr, field="taxon_canonical_name", fun=function(x, ...){length(unique(na.omit(x)))}, update=TRUE)
      legendTitle <- "N. of species"
    } #else if(input$entity=="n_studies"){
    #   SPr <- rasterize(SP, rr, field="study.name", fun=function(x, ...){length(unique(na.omit(x)))}, update=TRUE)
    #   legendTitle <- "N. of Movebank studies"
    # }
    bounds <- as.vector(bbox(extent(data)))
    SPr_l <- projectRasterForLeaflet(SPr, method = "ngb")
    brewCol <- brewer.pal(7, name = "YlGnBu")
    rPal <- colorNumeric(brewCol, values(SPr_l), na.color = "transparent", reverse = T)

    outl <- leaflet() %>% 
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>% 
      addTiles() %>%
      addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addRasterImage(SPr_l, colors = rPal, opacity = 0.7, project = FALSE, group = "raster") %>%
      addLegend(position="topright", opacity = 0.6, bins = 7,
                pal = rPal, values = values(SPr_l), title = legendTitle) %>%
      addScaleBar(position="bottomright",
                  options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
      addLayersControl(
        baseGroups = c("TopoMap","Aerial"),
        overlayGroups = "raster",
        options = layersControlOptions(collapsed = FALSE)
      )
    outl   
  })
  
  
  output$leafmap <- renderLeaflet({
    rmap()  
  })  
  
  ### save map, takes some seconds ###
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


