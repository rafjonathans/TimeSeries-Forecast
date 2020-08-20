library(tidyverse)
library(lubridate)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(forecast)
library(magrittr)
library(ggthemes)
library(plotly)


# Prepare data
rideSharing <- read_csv(choose.files())

source("Dashboard-Tab_Map.R")
source("Dashboard-Tab_Forecast.R")

ui <- navbarPage(title = "Ride Sharing",
                 tab_Map_ui,
                 tab_Forecast_ui
                 )

server <- function(input, output, session) {
  tab_Map_server(input, output, session)
  tab_Forecast_server(input, output, session)
}

shinyApp(ui, server)



