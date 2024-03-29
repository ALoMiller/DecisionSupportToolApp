shapefilePath <- here::here("InputShapefiles")

MaineDMR <-  sf::st_read(dsn = shapefilePath, layer = 'SmoothedMaineDMR',quiet = T)
LMAs <-  sf::st_read(dsn = shapefilePath, layer = 'Lobster_Management_Areas',quiet = T)
zero2three <- sf::st_read(dsn = paste0(shapefilePath,'/CoastalShapefiles'), layer = 'DST_CoastTo3nmi',quiet = T)
three2twelve <- sf::st_read(dsn = paste0(shapefilePath,'/CoastalShapefiles'), layer = '3nmi_to_12nmi',quiet = T)
twelve2EEZ <- sf::st_read(dsn = paste0(shapefilePath,'/CoastalShapefiles'), layer = '12nmi_to_EEZ_Boundary',quiet = T)
GSC_Gillnet <-  sf::st_read(dsn = shapefilePath, layer = 'Great_South_Channel_Restricted_Gillnet_Area',quiet=T)
GSC_Trap <-  sf::st_read(dsn = shapefilePath, layer = 'Great_South_Channel_Restricted_Trap-Pot_Area',quiet=T)
GSC_Sliver <-  sf::st_read(dsn = shapefilePath, layer = 'Great_South_Channel_Sliver_Restricted_Area',quiet=T)
MASS_RA <-  sf::st_read(dsn = shapefilePath, layer = 'Massachusetts_Restricted_Area',quiet = T)
MASS_RANE <-  sf::st_read(dsn = shapefilePath, layer = 'Massachusetts_Restricted_Area_North_Expansion',quiet=T)
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

sf::st_transform(LMAs,sf::st_crs(sf::st_crs(MaineDMR)))
LMAs$shape <- 'LMA'
MaineDMR$shape <- 'MEDMR'
two <- sf::st_union(MaineDMR,LMAs)
