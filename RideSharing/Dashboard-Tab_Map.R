
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
                                                     paste0(
                        "<h2>Ride Sharing</h2>
                        <p align = 'justify'>This application explores and forecasts the demand of the ride sharing service 
                        in Turkey named <a href = 'https://scotty.app/'>Scotty</a>. Scotty is the fastest 
                        growing Turkish born technology start-up, serving in three verticals; ride-sharing, 
                        food delivery and courier delivery in Istanbul.
                        <h3>Customer Demand Location</h4>
                        <p>This part shows the customer location on the map. There are three features to 
                        control the map visualization, including:
                        </p>
                        <ul>
                          <li>Demand type, customer's request or destination location</li>
                          <li>Date, Date of the customer's demand</li>
                          <li>Hour, Hour of the day of the customer's demand</li>
                        </ul>
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
                                            ))),
                                    fluidRow(column(width = 12,
                                                    HTML(paste0(
                          "<p align= 'justify'>From this mapping, it can be seen that the customers request location is mostly
                          around the city and some outside the city or Istanbul. For the destination location,
                          it seems that the dataset shows the destination is to outside of Turkey region. 
                          </p>"           
                                                    ))))
                      ),
                      
                      
                      # Start design the main panel
                      mainPanel(width = 8,
                                leafletOutput("orderridemap", height = 650)
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
