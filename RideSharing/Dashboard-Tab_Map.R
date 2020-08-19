
# Preparation
timeLocation <- rideSharing %>% 
  mutate(Day = weekdays(timeStamp),
         Date = date(timeStamp),
         Hour = hour(timeStamp)) %>% 
  group_by(Date, Hour) %>% 
  distinct(srcLong, srcLat, destLong, destLat) %>% 
  arrange(Date, Hour) %>% 
  ungroup() %>% 
  complete(Date, Hour) %>% 
  replace(. == 0, NA)

timeRange <- unique(timeLocation$Date)
hourRange <- unique(timeLocation$Hour)


# For dashboard

tab_Map_ui <- tabPanel(title = "Map",
                    # Start sidebar layout style with sidebar and main panel
                    sidebarLayout(
                      # Start design the sidebar panel
                      sidebarPanel(width = 4,
                                   fluidRow(column(width = 12,
                                                   HTML(
                                                     paste0("<h2>Ride Sharing Map</h2>
                        <p>This app visualize the order and destination location in a ride sharing 
                        application. Dataset used in this application can be found 
                        <a href='https://rpubs.com/aepoetry/beam_me_up'>here</a></p>
                        
                        <h4>Map Control</h4>
                        <p>Instruction(s):
                        <ul>
                          <li>Choose the departing or destination location to be shown on the map</li>
                          <li>Select observed date on the dateInput column</li>
                          <li>Use the slider to control the daytime observation</li>
                        </ul>
                        </p>
                        "
                                                     )))),
                                   fluidRow(
                                     # Select source or destination
                                     column(width = 7,
                                            tags$style(".fa-check-square {color = steelblue}"),
                                            tags$style(".fa-square-o {color = steelblue}"),
                                            radioGroupButtons(
                                              inputId = "showLoc",
                                              label = "Location to show",
                                              choices = c("Departing", "Destination"),
                                              direction = "horizontal",
                                              width = "400px",
                                              status = "primary", 
                                              checkIcon = list(
                                                yes = icon("check-square"),
                                                no = icon("square-o"))
                                            )),
                                     # Select date
                                     column(width = 5,
                                            dateInput(
                                              inputId = "dateSelected",
                                              label = "Choose date:", 
                                              value = min(timeRange),
                                              min = min(timeRange),
                                              max = max(timeRange)))
                                   ),
                                   # Select time of the day
                                   fluidRow(
                                     column(width = 12,
                                            sliderTextInput(
                                              inputId = "timeSelected",
                                              label = "Choose time:", 
                                              choices = hourRange,
                                              grid = TRUE
                                            )))
                      ),
                      
                      
                      # Start design the main panel
                      mainPanel(width = 8,
                                leafletOutput("orderridemap", height = 600)
                      )
                    ))


tab_Map_server <- function(input, output, session){
  output$orderridemap <- renderLeaflet({
    loc <- timeLocation %>% 
      filter(Date == input$dateSelected,
             Hour == input$timeSelected)
    
    if (input$showLoc == "Departing") {
      loc %>% 
        leaflet() %>% 
        addTiles() %>% 
        addMarkers(lng = ~srcLong,
                   lat = ~srcLat,
                   clusterOptions = markerClusterOptions())
    } else {
      loc %>% 
        leaflet() %>% 
        addTiles() %>% 
        addMarkers(lng = ~destLong,
                   lat = ~destLat,
                   clusterOptions = markerClusterOptions())
    }
  })
}