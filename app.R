#########################################################
# Shiny App by Addy Smith 
# Oct. 26, 2023
#
# Show the number of fatal car accidents for each state
# based on user selected weather type
# Referenced code from https://library.virginia.edu/data/articles/data-scientist-as-cartographer-an-introduction-to-making-interactive-maps-in-r-with-leaflet
#
# Deployed at https://addy-smith.shinyapps.io/accident_map/
# Source Code at GitHub https://github.com/addysmith00/Fatal_Traffic-_Accidents.git
##########################################################

library(tidyverse)
library(shiny)
library(leaflet)
library(magrittr)
library(sf)
library(htmltools)
library(htmlwidgets)
library(stringi)
library(RColorBrewer)


# data pre processing steps
accident <- read.csv('accident.csv')
weather <- read.csv('weather.csv')

accident2 <- accident %>%
  select(STATENAME, ST_CASE, PEDS, PERNOTMVIT, VE_TOTAL, VE_FORMS, PERSONS, PERMVIT,
         COUNTYNAME, MONTHNAME, DAY_WEEKNAME, HOURNAME, TWAY_ID, ROUTENAME, RUR_URBNAME,
         HARM_EVNAME, MAN_COLLNAME) %>%
  left_join(weather, by = "ST_CASE") %>%
  select(STATENAME.x, ST_CASE, PEDS, PERNOTMVIT, VE_TOTAL, VE_FORMS, PERSONS, PERMVIT,
         COUNTYNAME, MONTHNAME, DAY_WEEKNAME, HOURNAME, TWAY_ID, ROUTENAME, RUR_URBNAME,
         HARM_EVNAME, MAN_COLLNAME, WEATHERNAME) %>%
  rename(STATENAME = STATENAME.x) 

grouped <- accident2 %>%
  group_by(STATENAME, WEATHERNAME) %>%
  summarise(TotalAccidents = n())

population <- read.csv('population.csv')

population <- population %>%
  select(NAME, POPESTIMATE2020) %>%
  rename(STATENAME = NAME)

population <- population[c(15:65),]


new_grouped <- grouped %>%
  left_join(population, by = "STATENAME") %>%
  mutate(accidents_per_100k = (TotalAccidents/POPESTIMATE2020) * 100000)


# upload Census Bureau shape file data; found in above link
states <- read_sf('cb_2019_us_state_5m/cb_2019_us_state_5m.shp')

states1 <- states %>%
  rename(STATENAME = NAME)

states2 <- merge(states1, new_grouped, by = 'STATENAME', all.x = F)


# Define UI for application that produces colored leaflet map
ui <- fluidPage(
  
  # App title
  titlePanel("U.S. Fatal Traffic Accidents (2021)"),
  
  # Sidebar with drop down for weather type and buttons to change color
  sidebarLayout(
    sidebarPanel(
      selectInput("weather_input",
                  "Select Weather Condition:",
                  choices = unique(new_grouped$WEATHERNAME)),
      radioButtons("palette_color",
                   "Select Palette Color:",
                   choices = c("Oranges", "Reds", "Greens"),
                   selected = "Oranges")
    ),
    
    # Display a colored leaflet map
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic required to create leaflet map
server <- function(input, output) {
  
  # Update data based on weather type chosen by user
  filtered_data <- reactive({
    
    filter_data <- states2 %>% filter(WEATHERNAME == input$weather_input)
    return(filter_data)
  })
  
  # Update color palette based on color chosen by user
  selected_palette <- reactive({
    selected_palette <- colorNumeric(input$palette_color, domain = NULL)
    return(selected_palette)
  })
  
  # Render map
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels)  %>% 
      setView(lng = -96.25, lat = 39.50, zoom = 4) %>% 
      addPolygons(data = filtered_data(),
                  
                  # state border color
                  color = 'salmon',
                  
                  # weight of the state borders
                  weight = .75,
                  
                  # values >1 simplify the polygons' lines for less detail but faster loading
                  smoothFactor = 1,
                  
                  # set opacity of polygons
                  fillOpacity = .60,
                  
                  # specify that the each state should be colored per selected_palette()
                  fillColor = ~selected_palette()(filtered_data()$accidents_per_100k)) %>%
      addLegend(pal = selected_palette(), values = filtered_data()$accidents_per_100k,
                title = '<small>Fatal Traffic Accidents<br>(per 100k)</small>',
                position = 'bottomright',
                opacity = 0.25)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

