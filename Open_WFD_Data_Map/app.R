# Map which visualises WFD classifications by Waterbody

library(sf)
library(tidyverse)

  WFD <- read_sf("C:/Users/hg000051/OneDrive - Defra/Projects/04_Misc_Data/WFD Data/Whole England/Converted_sf_whole_eng.shp")
  Class <- read.csv("C:/Users/hg000051/Downloads/England_classifications (11).csv")
  merge <- inner_join(WFD, Class, by = c("WB_ID" = "Water.Body.ID"))

  merge$Year <- as.numeric(merge$Year)
  
  # Begin the app  
  
  ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  sliderInput("seLect", "Year", min=min(merge$Year), max=max(merge$Year),
                              value = 2019, step = 1, sep = ""
                              
                  ),
                  selectInput("OP_Select", "Select Operational Catchment", sort(unique(merge$OPCAT_NAME))
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
    
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
                merge %>% filter(OPCAT_NAME == input$OP_Select & Classification.Item == input$Class_Element
                                  & Year == as.numeric(input$seLect))
                                        })
    
    output$map <- renderLeaflet({
      # Basemap set up: Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      leaflet(merge) %>% addTiles()
    })
    
    # Interactive changes to the map should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
      
      bbox <- st_bbox(filteredData())
      outline <- st_union(filteredData())
      
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]], options = list()) %>% 
        addPolygons(data=outline, color = "black",
                    fillColor = NA) %>%
        addPolygons(data=filteredData(),
                    fillColor = ~pal(Status),
                    fillOpacity = 0.7,
                    color = "black",  weight = 0.5, 
                    dashArray = "1",
                    popup = paste0("I'M NOT HERE; THIS ISN'T HAPPENING"),
              highlightOptions = highlightOptions(color = "white", weight = 4,
                                                        bringToFront = TRUE)) |> 
        addMiniMap()
    })
    
    
  }
  
  shinyApp(ui, server)
