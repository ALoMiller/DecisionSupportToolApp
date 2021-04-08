library(rgdal)
library(geojsonsf)
library(geojsonio)
library(rmapshaper)
library(spdplyr)

StatAreas <- rgdal::readOGR(dsn = shapefilePath, layer = 'StatAreas_DST_V3_SmoothCoast')
head(StatAreas@data, 10)
areas <- c('464', '465', '467', '511', '512', '513', '514',
           '515', '521', '522', '525', '526', '533', '534',
           '537', '538', '539', '541', '542', '552', '561',
           '562', '611', '612', '613', '614', '615', '616',
           '621', '622', '623', '625', '626', '627', '631',
           '632', '635', '636', '700', '701', '702', '706',
           '707', '708', '709', '712', '713', '714', '715',
           '717', '718', '719', '722', '723', '724', '727',
           '728', '729', '732', '733', '736', '737', '740',
           '741')

StatAreasDST <- StatAreas %>% 
  filter(Id %in% areas)
# county_id <- county %>% rename(county_id = GEOID)
# county_select <- county %>% select(STATEFP, COUNTYFP, GEOID, NAME)
StatAreasDST_json <- geojson_json(StatAreasDST)
StatAreasDST_json_simplified <- ms_simplify(StatAreasDST_json)


shapefilePath <- "/net/shiny1/amiller/DST/InputShapefiles"

StatAreas <-  sf::st_read(dsn = shapefilePath, layer = 'StatAreas_DST_V3_SmoothCoast',quiet = T)
StatAreas <- raster::subset(StatAreas,Id %in% areas)
coords <- as.data.frame(sf::st_coordinates(sf::st_centroid(StatAreas)))
colnames(coords) <- c("Long","Lat")

StatAreas_J <- sf_geojson(StatAreas)

LMAs <-  sf::st_read(dsn = shapefilePath, layer = 'Lobster_Management_Areas',quiet = T)
LMA_J <- sf_geojson(LMAs)
leaflet() %>%
  setView(lng = -68.73742, lat = 42.31386, zoom = 6) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addScaleBar(position = 'bottomright', options = scaleBarOptions(maxWidth = 250)) %>%
  
  addPolygons(group = "StatAreas" ,data = StatAreas_J , stroke = TRUE, color = '#5a5a5a',
              opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3) 
pal <- colorNumeric("viridis", NULL)

leaflet(LMA_J) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(log10(pop)),
              label = ~paste0(county, ": ", formatC(pop, big.mark = ","))) %>%
  
