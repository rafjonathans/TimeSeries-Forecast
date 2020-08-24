
# Data for forecasting
rideOrder <- rideSharing %>%
  # Add Date and Hour
  mutate(Date = date(timeStamp),
         Hour = hour(timeStamp)) %>% 
  arrange(riderID, Date, Hour) %>% 
  
  # Remove multiple cancel and no driver in orderStatus at the same Date and Hour
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
  
  # Summarize total order per each order status
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
                             width = "300px",
                             status = "primary", 
                             justified = T, 
                             checkIcon = list(
                               yes = icon("check-square"),
                               no = icon("square-o"))
                           ))
                  ),
                  fluidRow(column(width = 12,
                                  sliderTextInput(
                                    inputId = "EDWeek",
                                    label = "Choose week:", 
                                    choices = unique(rideOrder$Week),
                                    grid = TRUE
                                  )
                  )),
                  fluidRow(column(width = 12,
                                  sliderTextInput(
                                    inputId = "EDDay",
                                    label = "Choose day of the week:", 
                                    choices = unique(weekdays(rideOrder$Date)),
                                    grid = TRUE
                                  )
                  )),
                  fluidRow(column(width = 12,
                                  sliderTextInput(
                                    inputId = "EDHour",
                                    label = "Choose time of the day:", 
                                    choices = unique(rideOrder$Hour),
                                    grid = TRUE
                                  )
                  )),
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
                           pickerInput(
                             inputId = "forecastMethod",
                             label = "Style : primary", 
                             choices = c("ets", "stlm", "tbats"),
                             options = list(
                               style = "btn-primary")
                           ))
                    ),
                  fluidRow(
                    column(width = 12,
                           sliderTextInput(inputId = "tail",
                                label = "Set n tail datapoint", 
                                choices = c(5,10,25,50,100,150,200),
                                grid = TRUE))
                    )
                  )), 
                          
              mainPanel(width = 8,
                        tabsetPanel(id = "tabselected",
                
                # Tab to explain the data 
                tabPanel(title = "Explored Data",
                         value = 1,
                         uiOutput(outputId = "ED1_title"),
                         fluidRow(column(width = 3,
                                         uiOutput(outputId = "EDbox1")),
                                  column(width = 3,
                                         uiOutput(outputId = "EDbox4")),
                                  column(width = 3,
                                         uiOutput(outputId = "EDbox2")),
                                  column(width = 3,
                                         uiOutput(outputId = "EDbox3"))
                                  ),
                         fluidRow(column(width = 12,
                                         plotOutput(outputId = "plotED1", 
                                                    height = 300))
                                  ),
                         uiOutput(outputId = "ED1_exp")
                         ),
                
                # Tab for forecasting
                tabPanel(title = "Forecasting",
                         value = 2,
                         tags$h2("Exploration"),
                         fluidRow(column(width = 3,
                                         withSpinner(valueBoxOutput(outputId = "FCbox1", width = 12))),
                                  column(width = 3,
                                         withSpinner(valueBoxOutput(outputId = "FCbox2", width = 12))),
                                  column(width = 3,
                                         withSpinner(valueBoxOutput(outputId = "FCbox3", width = 12))),
                                  column(width = 3,
                                         withSpinner(valueBoxOutput(outputId = "FCbox4", width = 12)))
                         ),
                         fluidRow(column(width = 12,
                                         withSpinner(plotlyOutput(outputId = "plotForecast_1"))
                                         ))
                         )
                ))
              ))


tab_Forecast_server <- function(input, output, session){
  
  # Explored Data
  ## Conditional title content
  output$ED1_title <- renderUI({
    if (input$plotEDopt == "Week") {
      # If week data selected
      tags$h2("Week Seasonality")
      
      # If Day data selected
    } else if (input$plotEDopt == "Day") {
      tags$h2("Daily Demand Profile")
    }
  })
  
  
  ## Value Box 1
  output$EDbox1 <- renderUI({
    if (input$plotEDopt == "Week") {
      # If week data selected
      valueBoxOutput(outputId = "freebox1_1", width = 12)
      # If Day data selected
    } else if (input$plotEDopt == "Day") {
      valueBoxOutput(outputId = "freebox1_2", width = 12)
    }
  })
  
  output$freebox1_1 <- renderValueBox({
    valueBox(value = rideOrder %>% 
                        group_by(Week) %>%  
                        summarise(Total_Confirmed = sum(confirmed), 
                                  Total_Cancelled = sum(cancelled), 
                                  Total_Nodriver = sum(nodrivers)) %>%
                        ungroup() %>% 
                        filter(Week == input$EDWeek) %>%
                        mutate(percent = Total_Confirmed/(Total_Confirmed + Total_Cancelled + Total_Nodriver)) %>% 
                        select(percent) %>% as.numeric() %>% percent(), 
             subtitle = paste("Order confirmed on Week-", input$EDWeek), 
             icon = icon("dollar-sign"), 
             color = "green")
  })
  
  output$freebox1_2 <- renderValueBox({
    valueBox(value = rideOrder %>% 
                        group_by(Week,
                                 Day = weekdays(Date)) %>% 
                        summarise(Total_Confirmed = sum(confirmed), 
                                  Total_Cancelled = sum(cancelled), 
                                  Total_Nodriver = sum(nodrivers)) %>%
                        ungroup() %>% 
                        filter(Week == input$EDWeek, 
                               Day == input$EDDay) %>% 
                        select(Total_Confirmed) %>% as.numeric(), 
             subtitle = paste("Confirmed on", input$EDDay), 
             icon = icon("dollar-sign"), 
             color = "green")
  })
  
  
  ## Value Box 2
  output$EDbox2 <- renderUI({
    if (input$plotEDopt == "Week") {
      # If week data selected
      valueBoxOutput(outputId = "freebox2_1", width = 12)
      # If Day data selected
    } else if (input$plotEDopt == "Day") {
      valueBoxOutput(outputId = "freebox2_2", width = 12)
    }
  })
  
  output$freebox2_1 <- renderValueBox({
    valueBox(value = rideOrder %>% 
               group_by(Week,
                        Day = weekdays(Date)) %>%  
               summarise(Total_Confirmed = sum(confirmed), 
                         Total_Cancelled = sum(cancelled), 
                         Total_Nodriver = sum(nodrivers)) %>%
               ungroup() %>% 
               filter(Week == input$EDWeek,
                      Day == input$EDDay) %>%
               select(Total_Cancelled) %>% as.numeric(), 
             subtitle = paste("Cancelled on Week-", input$EDWeek), 
             icon = icon("fire"), 
             color = "red")
  })
  
  output$freebox2_2 <- renderValueBox({
    valueBox(value = rideOrder %>% 
               group_by(Week,
                        Day = weekdays(Date),
                        Hour) %>% 
               summarise(Total_Confirmed = sum(confirmed), 
                         Total_Cancelled = sum(cancelled), 
                         Total_Nodriver = sum(nodrivers)) %>%
               ungroup() %>% 
               filter(Week == input$EDWeek,
                      Day == input$EDDay,
                      Hour == input$EDHour) %>% 
               select(Total_Cancelled) %>% as.numeric(), 
             subtitle = paste("Cancelled on at Hour-", input$EDHour), 
             icon = icon("fire"), 
             color = "red")
  })
  
  
  ## Value Box 3
  output$EDbox3 <- renderUI({
    if (input$plotEDopt == "Week") {
      # If week data selected
      valueBoxOutput(outputId = "freebox3_1", width = 12)
      # If Day data selected
    } else if (input$plotEDopt == "Day") {
      valueBoxOutput(outputId = "freebox3_2", width = 12)
    }
  })
  
  output$freebox3_1 <- renderValueBox({
    valueBox(value = rideOrder %>% 
               group_by(Week,
                        Day = weekdays(Date)) %>%  
               summarise(Total_Confirmed = sum(confirmed), 
                         Total_Cancelled = sum(cancelled), 
                         Total_Nodriver = sum(nodrivers)) %>%
               ungroup() %>% 
               filter(Week == input$EDWeek,
                      Day == input$EDDay) %>%
               select(Total_Nodriver) %>% as.numeric(), 
             subtitle = paste("NoDriver on Week-", input$EDWeek), 
             icon = icon("exclamation"), 
             color = "yellow")
  })
  
  output$freebox3_2 <- renderValueBox({
    valueBox(value = rideOrder %>% 
               group_by(Week,
                        Day = weekdays(Date),
                        Hour) %>% 
               summarise(Total_Confirmed = sum(confirmed), 
                         Total_Cancelled = sum(cancelled), 
                         Total_Nodriver = sum(nodrivers)) %>%
               ungroup() %>% 
               filter(Week == input$EDWeek,
                      Day == input$EDDay,
                      Hour == input$EDHour) %>%  
               select(Total_Nodriver) %>% as.numeric(), 
             subtitle = paste("Total NoDriver at Hour-", input$EDHour), 
             icon = icon("exclamation"), 
             color = "yellow")
  })
  
  
  ## Value Box 4
  output$EDbox4 <- renderUI({
    if (input$plotEDopt == "Week") {
      # If week data selected
      valueBoxOutput(outputId = "freebox4_1", width = 12)
      # If Day data selected
    } else if (input$plotEDopt == "Day") {
      valueBoxOutput(outputId = "freebox4_2", width = 12)
    }
  })
  
  output$freebox4_1 <- renderValueBox({
    valueBox(value = rideOrder %>% 
               group_by(Week,
                        Day = weekdays(Date)) %>%  
               summarise(Total_Confirmed = sum(confirmed), 
                         Total_Cancelled = sum(cancelled), 
                         Total_Nodriver = sum(nodrivers)) %>%
               ungroup() %>% 
               filter(Week == input$EDWeek,
                      Day == input$EDDay) %>%
               select(Total_Confirmed) %>% as.numeric(), 
             subtitle = paste("Confirmed on", input$EDDay), icon = icon("tachometer-alt"), color = "blue")
  })
  
  output$freebox4_2 <- renderValueBox({
    valueBox(value = rideOrder %>% 
               group_by(Week,
                        Day = weekdays(Date),
                        Hour) %>% 
               summarise(Total_Confirmed = sum(confirmed), 
                         Total_Cancelled = sum(cancelled), 
                         Total_Nodriver = sum(nodrivers)) %>%
               ungroup() %>% 
               filter(Week == input$EDWeek,
                      Day == input$EDDay,
                      Hour == input$EDHour) %>% 
               select(Total_Confirmed) %>% as.numeric(), 
             subtitle = paste("Confirmed at Hour-", input$EDHour), icon = icon("tachometer-alt"), color = "blue")
  })
  
  
  ## Conditional explanation content
  output$ED1_exp <- renderUI({
    if (input$plotEDopt == "Week") {
      # If week data selected
      fluidRow(
        column(width = 12,
               HTML(paste0(
                        "
                        <br></br>
                        <h4>Explanation: </h4>
                        <p align = 'justify'>
                        As can be seen in the graph above, there is an obvious pattern on the demand profile
                        in daily basis. 
                        </p>
                        "
                      )))
      )
      
      # If Day data selected
    } else if (input$plotEDopt == "Day") {
      fluidRow(
        column(width = 12,
               HTML(paste0(
                 "
                        <br></br>
                        <h4>Explanation: </h4>
                        <p align = 'justify'>
                        In hourly basis, there are more demand on before and after office. There is a slight
                        increase in mid day as it is assumed people are moving for lunch or travel.
                        </p>
                        "
               )))
      )
    }
  })
  
  
  ## plotED1
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
  
  
  # Forecast 
  ## Make time series data
  tsdata <- reactive({
    if (input$forecastMethod == "ets") {
      ts(rideOrder$confirmed,
         start = min(rideOrder$Date),
         frequency = 24) + 1
    } else if (input$forecastMethod %in% c("stlm", "tbats")) {
      msts(rideOrder$confirmed,
           seasonal.periods = c(24, 24*7)) + 1
    }
  })
  
  ## Split data
  ts_initial <- reactive({
    head(tsdata(), length(tsdata()) - input$tail)
  })
  
  ts_last <- reactive({
    tail(tsdata(), input$tail)
  })
  
  ## Forecast input
  forecast_input <- reactive({
    if (input$forecastMethod == "ets") {
      (ts_initial() + 1) %>%
        ets(lambda = 0)
    } else if(input$forecastMethod == "stlm") {
      (ts_initial() + 1) %>%
        stlm(lambda = 0)
    } else if(input$forecastMethod == "tbats") {
      temp <- (ts_initial() + 1) %>%
        log() %>%
        tbats(use.box.cox = FALSE)
      
      temp$lambda <- 0
      return(temp)
    }
    
  })
  
  ## Forecast output
  forecast_out <- reactive({
    forecast(forecast_input(), input$tail)
  })
    
  ## Forecast accuracy
  forecast_acc <- reactive({
    tibble(Actual = as.vector(ts_last()),
           Predicted = as.vector(forecast_out()$mean)) %>%
      filter(Actual != 0,
             Predicted != 0) %$%
      accuracy(Predicted, Actual)
    
  })
  
  
  # Valuebox
  output$FCbox1 <- renderValueBox({
    valueBox(value = (forecast_acc()[5]/100) %>% percent(),
             icon = icon("percent"),
             color = "green",
             subtitle = "MAPE")
  })
  
  output$FCbox2 <- renderValueBox({
    valueBox(value = forecast_acc()[3] %>% round(1),
             icon = icon("chart-line"),
             color = "yellow",
             subtitle = "MAE")
  })
  
  output$FCbox3 <- renderValueBox({
    valueBox(value = forecast_acc()[2] %>% round(1),
             icon = icon("wolf-pack-battalion"),
             color = "red",
             subtitle = "RMSE")
  })
  
  output$FCbox4 <- renderValueBox({
    valueBox(value = forecast_acc()[4] %>% round(1),
             icon = icon("tachometer-alt"),
             color = "blue",
             subtitle = "MPE")
  })


  ## plotForecast_1
  output$plotForecast_1 <- renderPlotly({
    ggplotly(
      autoplot(ts_initial()) +
        autolayer(ts_last(), series = "Actual") +
        autolayer((forecast_out()$mean)-1, series = "Forecast") +
        theme_classic() +
        labs(x = "",
             y = "Total Demand")
    )
  })

  
}
