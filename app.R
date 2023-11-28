library(colorRamps)
library(ggplot2)
library(shiny)
library(leaflet)
library(readxl)
require(reshape2)
library(plyr)
library(tidyr)

###########
# UI      # 
###########

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  # testtags$style(type="text/css", "legend.info {font-size: 5px}"),
  leafletOutput("map", width = "100%", height = "95%"),
  textOutput("value"), 
  absolutePanel(top = 10, right = 10,
                selectInput(inputId = "color", 
                            label = "Chose a Value:",
                            choices = c("Phylogenetic Classification" = 'branch.legend', 
                                        "Referential Classification" = 'guthrie'))
  )
)

###########
# Dataset # 
###########
source("data.R")


################
# Costum Icons #
################

b.img <- list.files("icons_branch/", pattern = "\\.png$")
b.img <- sub('\\.png$', '', b.img)
b.img <- as.character(sort(as.numeric(b.img)))
b.img <- paste0("icons_branch/",b.img, ".png")

#li = list()
#for(i in 1:length(img)){
#  li[[paste0("code",i)]] = makeIcon(paste0("icons/", img[i], ".png"), iconWidth = 40, iconHeight = 10)
#}

b.li <- icons(iconUrl = b.img, iconWidth = 60, iconHeight = 21)


g.img <- list.files("icons_guthrie/", pattern = "\\.png$")
g.img <- sub('\\.png$', '', g.img)
g.img <- as.character(sort(as.numeric(g.img)))
g.img <- paste0("icons_guthrie/",g.img, ".png")

g.li <- icons(iconUrl = g.img, iconWidth = 60, iconHeight = 21)


###########
# Popup   # 
###########

popup <- paste0("<b>", "Variety: ","</b>", d$variety, "<br>",
                "<b>", "Branch: ","</b>", ifelse(!is.na(d$branch), paste0("KLC/", d$branch), paste0("non KLC West-Costal-Bantu")), "<br>",
                "<b>", "(Updated) Guthrie 1971/Maho 2009 code: ", "</b>", d$guthrieCode, "<br>",
                "<b>", "Coordinates: ", "</b>", paste0(abs(d$lat), "°S / "), d$long, "°E <br>", 
                "<b>", "Source: ", "</b>", d$source)

###########
# Server  # 
###########

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet(d, 
            options = leafletOptions(minZoom = 5, 
                                     maxZoom = 11)) %>% 
      addProviderTiles(providers$CartoDB.Positron, 
                       group = "CartoDB") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, 
                       group = "ESRI") %>%
      addTiles(group = "OpenStreetMap") %>% 
      fitBounds(~min(as.numeric(d$long)), 
                ~min(as.numeric(d$lat)), 
                ~max(as.numeric(d$long)), 
                ~max(as.numeric(d$lat))) %>% 
      addLayersControl(
        position = "bottomright",
        baseGroups = c("Toner Lite", "ESRI", "OpenStreetMap"),
        options = layersControlOptions(collapsed = T))
  })
  
  selectedData <- reactive({
    d[[input$color]]
  })

  observe({
    
    x <- eval(parse(text = paste0("d$", input$color)))
    pal <- colorFactor(rainbow(length(unique(x))), 
                       domain = x)
    
    
    if(input$color == "branch.legend"){
      leafletProxy("map", data = d) %>% 
        clearMarkers() %>% 
        clearControls() %>% 
        addMarkers(data = d,
                   ~as.numeric(d$long),
                   ~as.numeric(d$lat),
                   icon = b.li, 
                   popup = popup)# %>% 
        #addLegend(position = "bottomleft",
        #          title = "",
        #          pal = if(input$color ==  "branch"){colorFactor(append(rainbow(length(unique(d$branch))), "#808080"), domain = NULL)}else{pal},
        #          values = if(input$color ==  "branch"){~d$branch.legend}else{~x},
        #          opacity = 1)
    } else if(input$color == "guthrie"){
      leafletProxy("map", data = d) %>% 
        clearMarkers() %>% 
        clearControls() %>% 
        addMarkers(data = d,
                   ~as.numeric(d$long),
                   ~as.numeric(d$lat),
                   icon = g.li, 
                   popup = popup)# %>% 
      
      #leafletProxy("map", data = d) %>% 
      #  clearMarkers() %>% 
      #  clearControls() %>% 
      #  addCircleMarkers(data = d,
      #                   ~as.numeric(d$long),
      #                   ~as.numeric(d$lat),
      #                   label = paste(d$variety, " [", d$guthrieCode, "]", sep = ''),
      #                   color = ~pal(x),
      #                   #color = ~d$branch.col,
      #                   popup = popup,
      #                   fillOpacity = .5) %>% 
      #  addLegend(position = "bottomleft",
      #            title = "",
      #            pal = if(input$color ==  "branch"){colorFactor(append(rainbow(length(unique(d$branch.legend))), "#808080"), domain = NULL)}else{pal},
      #            values = if(input$color ==  "branch"){~d$branch.legend}else{~x},
      #            opacity = 1)
    }

  })
  
  output$value <- renderText({
    if(input$color == "branch.legend"){
      print("based on de Schryver et al. 2015, Bostoen & de Schryver 2018 and Pacchiarotti et al. 2019")
    }
    else if(input$color == "guthrie"){
      print("based on Guthrie 1971, Maho 2009 and Pacchiarotti et al. 2019")
    }
  })
  
}

shinyApp(ui, server)
