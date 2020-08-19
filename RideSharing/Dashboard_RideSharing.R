library(tidyverse)
library(lubridate)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(forecast)
library(magrittr)
library(ggthemes)


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




# 
# 
# x <- seq(0.01, .99, length.out = 100)
# df <- data.frame(
#   x = rep(x, 2),
#   y = c(qlogis(x), 2 * qlogis(x)),
#   group = rep(c("a","b"),
#               each = 100)
# )
# p <- ggplot(df, aes(x=x, y=y, group=group))
# p + geom_line(aes(colour = group), linetype = 2)
# head(df)
# 
# 
# rideOrder %>% 
#   group_by(Week, Day = wday(Date, label = F)) %>% 
#   summarise(Total = sum(confirmed)) %>% 
#   ggplot(aes(x = Day,
#              y = Total,
#              group = Week,
#              colour = as.factor(Week))) +
#   geom_line(aes(linetype = as.factor(Week)),
#             size = 1) +
#   theme_classic() +
#   scale_color_economist()
#   
# 
# rideOrder %>% 
#   group_by(Week, Day = wday(Date, label = F)) %>% 
#   summarise(Total = sum(confirmed)) %>% 
#   ggplot(aes(x = Day,
#              y = Total,
#              fill = "steelblue")) +
#   geom_line() +
#   facet_grid(.~Week) +
#   theme_classic() +
#   theme(panel.spacing.x = unit(0, "lines"))
# 
# rideOrder %>% 
#   group_by(Date) %>% 
#   summarise(Total = sum(confirmed)) %>% 
#   ggplot(., aes(x = Date,
#              y = Total)) + 
#   geom_line() +
#   theme_classic() +
#   geom_vline()
