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
                            choices = c("Phylogenetic Classification" = 'branch', 
                                        "Referential Classification" = 'guthrie', 
                                        "Referential Classification (Labels)" = 'guthrie.label'))
  )
)

###########
# Dataset # 
###########

# Alter KLC Datensatz
# -------------------
klc <- as.data.frame(read_excel("Map PhD SB_KB.xlsx")) %>%
  dplyr::transmute(
    branch = .data[["KLC branch"]], 
    guthrieCode = .data[["GUTHRIE"]],
    variety = .data[["Variety"]],
    code = .data[["Code"]],
    long = .data[["Longitude"]],
    lat = .data[["Latitude"]],
    source = .data[["Source"]],
    place = .data[["Place"]]
  ) %>% dplyr::mutate(
    long = round(as.numeric(long), 2),
    lat = round(as.numeric(lat), 2)
  ) %>% dplyr::filter(!is.na(guthrieCode))

klc$guthrie <- paste(substr(klc$guthrieCode, 1, 1), 
                      10 * floor(as.numeric(substr(gsub("[^[:digit:]]", 
                                                        "", 
                                                        klc$guthrieCode), 
                                                   1, 2))/10), 
                      sep = '')

# remove prefixes
klc$variety <- gsub("Ki|Di|Yi|I|Ci", "", klc$variety)
klc$variety <- paste(toupper(substr(klc$variety, 1, 1)), substr(klc$variety, 2, nchar(klc$variety)), sep="")

# Neuer Datensatz von Sara
# ------------------------
sara <- as.data.frame(read_excel("geocoordinates 20181008_for website.xlsx")) %>%
  dplyr::transmute(
    guthrieCode = .data[["Guthrie (-inspired) Code"]],
    variety = .data[["Variety"]],
    long = .data[["longitude (geonames.org)"]],
    lat = .data[["latitude (geonames.org)"]],
    source = .data[["Sources"]]
  ) %>% dplyr::filter(!is.na(long))

# create colum with unique colors for individual variety
# cut everything after the 3rd position to get somewhat of a reasonable colormap
# see http://r.789695.n4.nabble.com/Ceiling-to-the-nearest-ten-tp844951p844952.html
sara$guthrie <- paste(substr(sara$guthrieCode, 1, 1), 
                      10 * floor(as.numeric(substr(gsub("[^[:digit:]]", "", sara$guthrieCode), 1, 2))/10), 
                      sep = '')

# für alle kleinen Buchstaben gsub("[^[:lower:]]", "", d$Guthrie...inspired..Code)
sara$guthrieCodeLang <- gsub("[^xyz]", 
                             "", 
                             sara$guthrieCode)

# Alle großen Buchstaben
sara$guthrieDialect <- substring(gsub("[^[:upper:]]", 
                                      "", 
                                      sara$guthrieCode), 2)

# Concatenate the two dataframes 
# ------------------------------
d <- rbind.fill(klc, sara)

# Labels for Legend for Phylogenetic Classification / Branch
# ----------------------------------------------------------
d$branch.legend <- paste0("KLC/", unique(d$branch))
d$branch.legend[d$branch.legend == "KLC/NA"] <- "WCB outside KLC"

################
# Costum Icons #
################

img <- list.files("icons/", pattern = "\\.png$")
img <- sub('\\.png$', '', img)
img <- as.character(sort(as.numeric(img)))
img <- paste0("icons/",img, ".png")

#li = list()
#for(i in 1:length(img)){
#  li[[paste0("code",i)]] = makeIcon(paste0("icons/", img[i], ".png"), iconWidth = 40, iconHeight = 10)
#}

li <- icons(iconUrl = img, iconWidth = 40, iconHeight = 10)

#li <- makeIcon("icons/1.png", iconWidth = 40, iconHeight = 10)

###########
# Popup   # 
###########

popup <- paste0("<b>", "Variety: ","</b>", d$variety, "<br>",
                "<b>", "Branch: ","</b>", ifelse(!is.na(d$branch), paste0("KLC/", d$branch), paste0("non KLC West-Costal-Bantu")), "<br>",
                "<b>", "(Updated) Guthrie 1971/Maho 2009 code: ", "</b>", d$guthrieCode, "<br>",
                "<b>", "Coordinates: ", "</b>", if(d$lat <= 0){paste0(abs(d$lat), "°S / ")} else {paste0(d$lat, "°N / ")}, d$long, "°E <br>", 
                "<b>", "Source: ", "</b>", d$source)

###########
# Server  # 
###########

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet(d, 
            options = leafletOptions(minZoom = 5, 
                                     maxZoom = 11)) %>% 
      addProviderTiles(providers$Stamen.TonerLite, 
                       group = "Toner Lite") %>%
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
    
    
    if(input$color == "guthrie.label"){
      leafletProxy("map", data = d) %>% 
        clearMarkers() %>% 
        clearControls() %>% 
        addMarkers(data = d,
                   ~as.numeric(d$long),
                   ~as.numeric(d$lat),
                   icon = li, 
                   popup = popup)# %>% 
        #addLegend(position = "bottomleft",
        #          title = "",
        #          pal = if(input$color ==  "branch"){colorFactor(append(rainbow(length(unique(d$branch))), "#808080"), domain = NULL)}else{pal},
        #          values = if(input$color ==  "branch"){~d$branch.legend}else{~x},
        #          opacity = 1)
    } else {
      leafletProxy("map", data = d) %>% 
        clearMarkers() %>% 
        clearControls() %>% 
        addCircleMarkers(data = d,
                         ~as.numeric(d$long),
                         ~as.numeric(d$lat),
                         label = paste(d$variety, " [", d$guthrieCode, "]", sep = ''),
                         color = ~pal(x),
                         popup = popup,
                         fillOpacity = .5) %>% 
        addLegend(position = "bottomleft",
                  title = "",
                  pal = if(input$color ==  "branch"){colorFactor(append(rainbow(length(unique(d$branch))), "#808080"), domain = NULL)}else{pal},
                  values = if(input$color ==  "branch"){~d$branch.legend}else{~x},
                  opacity = 1)
    }

  })
  
  output$value <- renderText({
    if(input$color == "branch"){
      print("based on de Schryver et al. 2015, Grollemund et al. 2015, Bostoen & de Schryver 2018")
    }
    else{
      print(" based on Guthrie 1971, Maho 2009 and further updates by Sara Pacchiarotti")
    }
  })
  
}

shinyApp(ui, server)
