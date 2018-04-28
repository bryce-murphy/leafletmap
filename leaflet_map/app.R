#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(RCurl)

path <- "https://raw.githubusercontent.com/bryce-murphy/leafletmap/master/500cities.csv"

cities <- 
  read_csv(path) %>%
  rename(City = PlaceName) %>%
  select(-ends_with("95CI")) %>%
  separate(Geolocation, into = c("lat", "lon"), sep = ", ") %>%
  unite(city_state, c(City, StateAbbr), sep = ", ", remove = F) %>%
  rename_all(
    funs(
      str_replace(., "_CrudePrev", ""))) %>%
  gather("measure", "value", 8:35) %>%
  arrange(StateAbbr, City)

cities$lat <- gsub("[()]", "", cities$lat)
cities$lon <- gsub("[()]", "", cities$lon)
cities$lat <- as.numeric(cities$lat)
cities$lon <- as.numeric(cities$lon)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Mapping Health in 500 Cities"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "city",
        label = "Select a City:",
        choices = unique(cities$city_state),
        selected = "Grand Rapids, MI"
      ),
      selectInput(
        inputId = "risk",
        label = "Select a Risk Factor",
        choices = c("Access" = "ACCESS2",
                    "Arthritis" = "ARTHRITIS",
                    "Binge Drinking" = "BINGE",
                    "High Blood Pressure" = "BPHIGH",
                    "Medium Blood Pressure" = "BPMED",
                    "Cancer (except skin)" = "CANCER",
                    "Current Asthma" = "CASTHMA",
                    "Coronary Heart Disease" = "CHD",
                    "Annual Checkup" = "CHECKUP",
                    "Cholesterol Screening" = "CHOLSCREEN",
                    "Colorectal Cancer Screening" = "COLON_SCREEN",
                    "COPD",
                    "Core Preventive Services for Men" = "COREM",
                    "Core Preventive Services for Women" = "COREW",
                    "Current Smoking" = "CSMOKING",
                    "Dental Visit" = "DENTAL",
                    "Diabetes" = "DIABETES",
                    "High Cholesterol" = "HIGHCHOL", 
                    "Chronic Kidney Disease" = "KIDNEY",
                    "Physical Activity" = "LPA",
                    "Mammography" = "MAMMOUSE",
                    "Mental Health" = "MHLTH",
                    "Obesity" = "OBESITY",
                    "Pap Smear Test" = "PAPTEST",
                    "Physical Health" = "PHLTH",
                    "Sleep" = "SLEEP",
                    "Stroke" = "STROKE",
                    "Teeth Loss" = "TEETHLOST"),
        selected = "OBESITY"),
      radioButtons(
        inputId = "color",
        label = "Select a Color Palette",
        choices = c("Red-Yellow-Green" = "RdYlGn",
                    "Red-Yellow-Blue" = "RdYlBu",
                    "Yellow - Red" = "YlOrRd", 
                    "Yellow-Green-Blue" = "YlGnBu",
                    "Purple-Orange" = "PuOr",
                    "Purple-Green" = "PRGn",
                    "Brown-Blue-Green" = "BrBG",
                    "Spectral"),
        selected = "RdYlGn")
      ),
    
    mainPanel(
      leafletOutput("USA")
    )
  )
  
  
  
)

# DEFINE SERVER LOGIC ---------------------------------------------------------------

server <- function(input, output) {
  
  output$USA <- renderLeaflet({
    
    cities_leaf <-
      cities %>%
      filter(city_state == input$city,
             measure == input$risk)
    
    pal <- colorNumeric(
      palette = input$color,
      domain = cities_leaf$value)
    
    leaflet(cities_leaf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(popup = ~as.character(value),
                 radius = ~sqrt(Population2010) * 10, 
                 fillColor = ~pal(value),
                 stroke = F,
                 opacity = 1,
                 weight = 1) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = ~value,
                title = "Risk Factor",
                opacity = 1)
    
    
    
    
  })
  
}




shinyApp(ui = ui, server = server)