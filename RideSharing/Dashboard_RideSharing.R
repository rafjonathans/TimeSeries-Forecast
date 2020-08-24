library(tidyverse)
library(lubridate)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(forecast)
library(magrittr)
library(ggthemes)
library(plotly)
library(scales)
library(shinydashboard)
library(shinycssloaders)


# Prepare data
rideSharing <- read_csv("ridesharing.csv")

source("Dashboard-Tab_Map.R")
source("Dashboard-Tab_Forecast.R")

ui <- tagList(
  useShinydashboard(),
  navbarPage(title = "Ride Sharing", 
             theme = shinytheme("darkly"),
             tab_Map_ui,
             tab_Forecast_ui
  )
)
  

server <- function(input, output, session) {
  tab_Map_server(input, output, session)
  tab_Forecast_server(input, output, session)

}

shinyApp(ui, server)
