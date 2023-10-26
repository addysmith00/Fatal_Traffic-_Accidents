library(tidyverse)
library(leaflet)
library(magrittr)
library(sf)
library(htmltools)
library(htmlwidgets)
library(stringi)
library(RColorBrewer)

# pre data steps
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


states <- read_sf('cb_2019_us_state_5m/cb_2019_us_state_5m.shp')


states <- states %>%
  rename(STATENAME = NAME)
states <- merge(states, new_grouped, by = 'STATENAME', all.x = F)

states <- subset(states, WEATHERNAME == "Cloudy")

#paletteNum <- colorNumeric('YlOrRd', domain = states$centskWh)



m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels)  %>% 
  setView(lng = -96.25, lat = 39.50, zoom = 4) %>% 
  addPolygons(data = states,
              
              # state border stroke color
              color = 'salmon',
              
              # soften the weight of the state borders
              weight = .75,
              
              # values >1 simplify the polygons' lines for less detail but faster loading
              smoothFactor = .3,
              
              # set opacity of polygons
              fillOpacity = .60,
              
              # specify that the each state should be colored per paletteNum()
              fillColor = states$accidents_per_100k) %>%
  addLegend(pal = paletteNum, values = states$accidents_per_100k,
            title = '<small>Fatal Traffic Accidents<br>(per 100k)</small>',
            position = 'bottomleft',
            opacity = 0.5)
m

