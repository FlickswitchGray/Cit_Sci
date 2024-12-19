# Map which visualises WFD classifications by Waterbody

library(sf)
library(tidyverse)
library(leaflet)
library(shiny)

unzip("Converted_Whole_Sf_eng.zip")  
unzip("England_Classifications.zip")  

WFD <- read_sf("Converted_Whole_Sf_eng.shp")
Class <- read.csv("England_Classifications.csv")


# Internal version
#library(pins)

#Cit_Sci_Board <- board_connect()

#WFD <- pin_read(name = "harry.gray@environment-agency.gov.uk/WFD_England",
 #               board = Cit_Sci_Board)

#Class <- pin_read(name = "harry.gray@environment-agency.gov.uk/WFD_Classifications_England",
 #                 board = Cit_Sci_Board)

#merge <- inner_join(WFD, Class, by = c("WB_ID" = "Water.Body.ID"))
#merge$Year <- as.numeric(merge$Year)

merge <- inner_join(WFD, Class, by = c("WB_ID" = "Water.Body.ID"))
merge$Year <- as.numeric(merge$Year)

#rivers <- read_sf("C:/Users/hg000051/Downloads/oprvrs_essh_gb (1)/data/WatercourseLink.shp") |> st_transform(4326)

# App

# bootstrap page is dynamic so fits whatever size your screen is

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("seLect", "Year", min=min(merge$Year), max=max(merge$Year),
                            value = 2019, step = 1, sep = ""
                            
                ),
                selectInput("OP_Select", "Select WFD Operational Catchment", sort(unique(merge$OPCAT_NAME)),
                            selected = "Tyne Upper"
                ),
                selectInput("Class_Element", "Select Classification Item", sort(unique(merge$Classification.Item)),
                            selected = "Overall Water Body"
                )
  )
)

server <- function(input, output, session) {
  
  
  # Define WFD palette
  pal <- colorFactor(
    palette = c("#ADE8F4", "seagreen", "seagreen", "yellow", "yellow", "#b71105","orange", "red", "red"),
    levels = c("High", "Good", "Supports Good", "Moderate", "Moderate or less", "Bad", "Poor", "Fail", "Does Not Support Good"),
    na.color = "transparent"
  )
  
  # Conditional input select dropdowns
  
  observe({
    filtered_year <- merge %>% filter(Year == input$seLect)
    updateSelectInput(
      session, "Class_Element",
      choices = sort(unique(filtered_year$Classification.Item)),
      selected = "Overall Water Body"
    )
  })
  
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    merge %>% filter(OPCAT_NAME == input$OP_Select,
                     Classification.Item == input$Class_Element,
                     Year == as.numeric(input$seLect))
  })
  
  # Basemap set up: Use leaflet() here, and only include aspects of the map that
  # won't need to change dynamically (at least, not unless the
  # entire map is being torn down and recreated).
  output$map <- renderLeaflet({
    leaflet(merge) %>% addProviderTiles(providers$Stadia)
  })
  
  # Interactive changes to the map should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    
    dat <- filteredData()
    if (nrow(dat) == 0) return()  # Prevent errors if no data matches filters
    
    bbox <- st_bbox(dat)
    outline <- st_union(dat)
    
    leafletProxy("map", data = dat) %>%
      clearShapes() %>%
      flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]], options = list()) %>% 
      addPolygons(data=outline, color = "black",
                  fillColor = NA) %>%
      addPolygons(data=dat,
                  fillColor = ~pal(Status),
                  fillOpacity = 0.7,
                  color = "black",  weight = 0.5, 
                  dashArray = "1",
                  popup = paste0("<b>",dat$WB_NAME,"<b/>",", ",
                                 "<b>"," ",dat$Year," ", dat$Status,"</b>", " for",
                                 "<br><b>",dat$Classification.Item,"</b>"),
                  highlightOptions = highlightOptions(color = "white", weight = 4,
                                                      bringToFront = TRUE)) |> 
      clearControls() |> 
      addLegend(data=dat, pal= pal, values = ~Status, position = "bottomright") 
  })
  
  
}

shinyApp(ui, server)
