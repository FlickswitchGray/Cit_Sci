# WFD map for the people

library(sf)
library(tidyverse)

  WFD <- read_sf("C:/Users/hg000051/OneDrive - Defra/Projects/04_Misc_Data/WFD Data/Whole England/Converted_sf_whole_eng.shp")
  Class <- read.csv("C:/Users/hg000051/Downloads/England_classifications (11).csv")
  merge <- inner_join(WFD, Class, by = c("WB_ID" = "Water.Body.ID"))

  merge$Year <- as.numeric(merge$Year)
  
  
  merge <- merge |> filter(OPCAT_NAME=="Parrett")

  # Begin the app  
  
  ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  sliderInput("seLect", "Year", min=min(merge$Year), max=max(merge$Year),
                              value = "2022", step = 1, sep = ""
                              
                  ),
                  selectInput("OP_Select", "Select Operational Catchment", sort(unique(merge$OPCAT_NAME))
                  ),
                  selectInput("Class_Element", "Select Classification Item", sort(unique(merge$Classification.Item))
                  )
    )
  )
  
  server <- function(input, output, session) {
    
    
    # Define WFD palette
    pal <- colorFactor(
      palette = c("#ADE8F4", "seagreen", "seagreen", "yellow", "#b71105","orange", "red"),
      levels = c("High", "Good", "Supports Good", "Moderate", "Bad", "Poor", "Fail"),
      na.color = "transparent"
    )
    
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
                merge %>% filter(OPCAT_NAME == input$OP_Select & Classification.Item == input$Class_Element
                                  & Year == input$seLect)
                                        })
        

    output$map <- renderLeaflet({
      # Basemap set up: Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      leaflet(WFD) %>% addTiles(providers$Stadia) 
    })
    
    # Interactive changes to the map should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
      
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        addPolygons(fillColor = ~pal(Status), color = "black",
                    weight = 0.5, fillOpacity = 0.9,
              highlightOptions = highlightOptions(color = "white", weight = 4,
                                                        bringToFront = TRUE))
    })
    
    # Create dropdown based on dynamic value selected in above drop down
    
   # observe(updateSelectInput(session, "seLect", choices = merge$OPCAT_NAME))
    
    # Use a separate observer to recreate the legend as needed.
    observe({
      proxy <- leafletProxy("map", data = merge)
      
      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% clearControls()
      if (input$legend) {
        pal <- colorpal()
        proxy %>% addLegend(position = "bottomright",
                            pal = pal, values = merge$Status
        )
        
      }
    })
  }
  
  shinyApp(ui, server)