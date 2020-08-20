# Forecast function

frcst <- function(forcastFeature = TRUE, 
                  fun,
                  timeFeature = TRUE, 
                  lambda = 0,
                  h = 10, 
                  freq = 24, 
                  season = 7, 
                  addDependentVar = 1){
  
  # transform data into timeseries forecasting format
  if (deparse(substitute(fun)) == "ets") {
    input <- ts(forcastFeature, start = min(timeFeature), frequency = freq) + addDependentVar
  } else if (deparse(substitute(fun)) %in% c("stlm", "tbats")) {
    input <- msts(forcastFeature, seasonal.periods = c(freq, freq * season)) + addDependentVar
  }
  
  # Function to add constant avoid Infinite log transformation value
  removeConstant <- function(forecastResult, constant = addDependentVar){
    forecastResult[["mean"]] <- forecastResult[["mean"]] - constant
    forecastResult[["upper"]] <- forecastResult[["upper"]] - constant
    forecastResult[["lower"]] <- forecastResult[["lower"]] - constant
    return(forecastResult)
  }
  
  # Forecasting
  if (deparse(substitute(fun)) == "tbats") {
    tbatsInput <- input %>%
      log() %>% 
      fun(use.box.cox = FALSE)
    
    tbatsInput$lambda <- 0
    
    forecasting <- tbatsInput %>% 
      forecast(h = h) %>% 
      removeConstant()
      
  } else {
    forecasting <- input %>%
      fun(lambda = lambda) %>%
      forecast(h = h) %>%
      removeConstant()
  }
  
  return(forecasting)
}


# Data for forecasting
rideOrder <- rideSharing %>%
  mutate(Date = date(timeStamp),
         Hour = hour(timeStamp)) %>% 
  arrange(riderID, Date, Hour) %>% 
  group_by(riderID, Date, Hour) %>%
  mutate(prevOrder = lag(orderStatus, default = NA),
         prev2Order = lag(orderStatus,n = 2, default = NA)) %>%  
  mutate(orderStatus = ifelse(orderStatus == "confirmed",
                                 orderStatus,
                                 ifelse(is.na(prevOrder),
                                        orderStatus,
                                        ifelse(orderStatus == prevOrder,
                                               NA,
                                               orderStatus)))) %>%
  mutate(orderStatus = ifelse(orderStatus == "confirmed",
                                 orderStatus,
                                 ifelse(is.na(prev2Order),
                                        orderStatus,
                                        ifelse(orderStatus == prev2Order,
                                               NA,
                                               orderStatus)))) %>%  
  filter(!is.na(orderStatus)) %>%
  ungroup() %>% 
  group_by(Date, Hour, orderStatus) %>% 
  summarise(Total = n()) %>% 
  ungroup() %>% 
  spread(key = orderStatus, value = Total) %>% 
  complete(Date, Hour) %>% 
  replace(is.na(.), 0) %>% 
  mutate(Week = week(Date),
         Day = wday(Date))

  

# For dashboard

tab_Forecast_ui <- tabPanel(title = "Forecasting",
            sidebarLayout(
              
              sidebarPanel(
                width = 4,
                
                # If tab explored data selected
                conditionalPanel(
                  condition = "input.tabselected == 1",
                  fluidRow(
                    column(width = 12,
                           HTML(paste0(
                            "
                            <h2>Preliminary Analysis</h2>
                            <p align='justify'>One of steps in conducting a forecasting is to perform a preliminary 
                            analysis. In this preliminary analysis, we will find a consistent pattern
                            (such a trend or seasonality) or outliers that requires a contextual 
                            knowledge to be explained. Findings in this preliminary analysis defines
                            the appropriate forecasting tool.</p>
                            "))
                           )),
                  
                  fluidRow(
                    column(width = 6,
                           tags$style(".fa-check-square {color = steelblue}"),
                           tags$style(".fa-square-o {color = steelblue}"),
                           radioGroupButtons(
                             inputId = "plotEDopt",
                             label = "Select profile",
                             choices = c("Week", "Day"),
                             direction = "horizontal",
                             width = "200px",
                             status = "primary", 
                             justified = T, 
                             checkIcon = list(
                               yes = icon("check-square"),
                               no = icon("square-o"))
                           ))
                  )
                  ),
                
                # If tab forecasting selected
                conditionalPanel(
                  condition = "input.tabselected == 2",
                  fluidRow(
                    column(width = 12,
                           HTML(paste0(
                             "<h2>Demand Forecasting</h2>",
                              "<p align='justify'>
                              This section shows a demo of a forecasting function to predict the 
                              ride sharing demand on the service provider. To test the accuracy, forecasting 
                              is conducted to redict the last (n) datapoints. These are the instruction to 
                              control the forecasting</p>
                              <ul>
                              <li>Choose the forecasting method</li>
                              <li>Select total datapoints to be predicted</li>
                              </ul>
                              ")))
                    ),
                  fluidRow(
                    column(width = 6,
                           # Select forcasting method
                           pickerInput(
                              inputId = "forecastMethod",
                              label = "Style : primary", 
                              choices = c("ets", "stlm", "tbats"),
                              options = list(
                              style = "btn-primary")
                              )),
                    column(width = 6,
                           # Space for feature
                           sliderTextInput(inputId = "dateRange",
                               label = "Choose date range:",
                               choices = timeRange,
                               selected = c(min(timeRange), max(timeRange))
                               ))
                    ),
                  fluidRow(
                    column(width = 12,
                           sliderInput(inputId = "tail",
                                label = "Set n tail datapoint", 
                                value = 1,
                                min = 1,
                                max = 200))
                    )
                  )), 
                          
              mainPanel(width = 8,
                        tabsetPanel(id = "tabselected",
                
                # Tab to explain the data 
                tabPanel(title = "Explored Data",
                         value = 1,
                         tags$h2("Exploration"),
                         fluidRow(column(width = 12,
                                         plotOutput(outputId = "plotED1", height = 300))
                                  )
                         ),
                
                # Tab for forecasting
                tabPanel(title = "Forecasting",
                         value = 2,
                         tags$h2("Exploration"),
                         fluidRow(column(width = 12))
                         ))
                ))
            )


tab_Forecast_server <- function(input, output, session){
  
  # Explored Data
  output$plotED1 <- renderPlot({
    
    if (input$plotEDopt == "Week") {
      rideOrder %>%
        group_by(Week, Day = wday(Date, label = F)) %>%
        summarise(Cancelled = sum(cancelled),
                  Confirmed = sum(confirmed),
                  Nodriver = sum(nodrivers)) %>% 
        gather(key, value, Cancelled, Confirmed, Nodriver) %>% 
        ggplot(aes(x = Day,
                   y = value,
                   group = key,
                   color = as.factor(key))) +
        geom_line() +
        geom_point(aes(x = Day,
                       y = value)) +
        facet_grid(.~Week) +
        theme_classic() +
        theme(panel.spacing.x = unit(0, "lines"),
              legend.position = c(0.06,0.8)) +
        labs(x = "Day of the week",
             y = "Total demand",
             title = "Seasonal profile",
             color = "Total demand")
      
    } else if (input$plotEDopt == "Day") {
      rideOrder %>% 
        group_by(Day = wday(Date, label = T),
                 Hour) %>%
        summarise(Cancelled = sum(cancelled),
                  Confirmed = sum(confirmed),
                  Nodriver = sum(nodrivers)) %>%   
        ggplot(aes(x = Hour,
                   y = Confirmed,
                   group = Day,
                   color = as.factor(Day))) +
        geom_line(size = 1) +
        theme_classic() +
        theme(legend.position = c(0.06,0.7)) +
        labs(x = "Hour of the day",
             y = "Total demand",
             title = "Daily profile",
             color = "Operational day") +
        scale_color_colorblind()
    }

  })
  
}
