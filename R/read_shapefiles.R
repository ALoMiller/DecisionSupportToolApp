## Stat Areas
shapefilePath <- "/net/shiny1/amiller/DST/InputShapefiles"

message("Reading in shapefiles ...")

#Maine
# MaineA <-  sf::st_read(dsn = here::here("TempShapefiles"), layer = 'Maine_Zone_A',quiet = T)
# MaineB <-  sf::st_read(dsn = here::here("TempShapefiles"), layer = 'Maine_Zone_B',quiet = T)
# MaineC <-  sf::st_read(dsn = here::here("TempShapefiles"), layer = 'Maine_Zone_C',quiet = T)
# MaineD <-  sf::st_read(dsn = here::here("TempShapefiles"), layer = 'Maine_Zone_D',quiet = T)
# MaineE <-  sf::st_read(dsn = here::here("TempShapefiles"), layer = 'Maine_Zone_E',quiet = T)
# MaineF <-  sf::st_read(dsn = here::here("TempShapefiles"), layer = 'Maine_Zone_F',quiet = T)
MaineG <-  sf::st_read(dsn = "/net/shiny1/amiller/DST/TempShapefiles", layer = 'Maine_Zone_G',quiet = T)

# LMAs <-  sf::st_read(dsn = shapefilePath, layer = 'Lobster_Management_Areas',quiet = T)

# OffshoreA <-  rgdal::readOGR(dsn = shapefilePath, layer = 'AtlOffshoreLAFigure2_',verbose = F)
# MassExpansion <-  rgdal::readOGR(dsn = shapefilePath, layer = 'Mass_Bay_Expansion_Only',verbose = F)
# CCBay <-  rgdal::readOGR(dsn = shapefilePath, layer = 'Proposed_Cape_Cod_Bay_Area',verbose = F)
# SouthShoreA <-  sf::st_read(dsn = shapefilePath, layer = 'SouthShoreA',quiet = T)
# SouthShoreB <-  sf::st_read(dsn = shapefilePath, layer = 'SouthShoreB',quiet = T)
# SouthShoreC <-  sf::st_read(dsn = shapefilePath, layer = 'SouthShoreC',quiet = T)
zero2three <- sf::st_read(dsn = paste0(shapefilePath,'/CoastalShapefiles'), layer = 'DST_CoastTo3nmi',quiet = T)
#zero2three <- geojsonio::geojson_write(zero2three)
three2twelve <- sf::st_read(dsn = paste0(shapefilePath,'/CoastalShapefiles'), layer = '3nmi_to_12nmi',quiet = T)
twelve2EEZ <- sf::st_read(dsn = paste0(shapefilePath,'/CoastalShapefiles'), layer = '12nmi_to_EEZ_Boundary',quiet = T)
#EastCoastLines <-  rgdal::readOGR(dsn = shapefilePath, layer = 'EastCoastLines',verbose = F)
# GB <-  rgdal::readOGR(dsn = shapefilePath, layer = 'GB_100M_to_600M',verbose = F)
# GOM <-  rgdal::readOGR(dsn = shapefilePath, layer = 'GOM_100M_to_EEZ',verbose = F)
# GSC_Gillnet <-  rgdal::readOGR(dsn = shapefilePath, layer = 'Great_South_Channel_Restricted_Gillnet_Area',verbose = F)
# GSC_Trap <-  rgdal::readOGR(dsn = shapefilePath, layer = 'Great_South_Channel_Restricted_Trap-Pot_Area',verbose = F)
# GSC_Sliver <-  rgdal::readOGR(dsn = shapefilePath, layer = 'Great_South_Channel_Sliver_Restricted_Area',verbose = F)
# LCMAs <-  rgdal::readOGR(dsn = shapefilePath, layer = 'LCMAs',verbose = F)
# MASS_RA <-  rgdal::readOGR(dsn = shapefilePath, layer = 'Massachusetts_Restricted_Area',verbose = F)
# MASS_RANE <-  rgdal::readOGR(dsn = shapefilePath, layer = 'Massachusetts_Restricted_Area_North_Expansion',verbose = F)
# NEA_NR <-  rgdal::readOGR(dsn = shapefilePath, layer = 'NEA_Nantucket_Rectangle',verbose = F)
# NEA_WGOM <-  rgdal::readOGR(dsn = shapefilePath, layer = 'NEA_WGOM_Area',verbose = F)
#SA_DT <-  rgdal::readOGR(dsn = shapefilePath, layer = 'StatAreas_DecisionTool',verbose = F)
#SA_537 <-  rgdal::readOGR(dsn = shapefilePath, layer = 'Statistical_Area_537',verbose = F)
#TinyWedge <-  rgdal::readOGR(dsn = shapefilePath, layer = 'TinyWedge_LMA1',verbose = F)
StatAreas <-  sf::st_read(dsn = shapefilePath, layer = 'StatAreas_DST_V3_SmoothCoast',quiet = T)
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
StatAreas <- raster::subset(StatAreas,Id %in% areas)
coords <- as.data.frame(sf::st_coordinates(sf::st_centroid(StatAreas)))
colnames(coords) <- c("Long","Lat")

#s512 <-  rgdal::readOGR(dsn = shapefilePath, layer = 's512',verbose = F)

message("Done")


