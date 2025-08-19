# Prepare spatial data for PalavaStyle package

# Load the spatial data
nrwGemLine <- sf::read_sf("C:/Users/lwlmaster/Nextcloud/R/Palava/data/gpackage/nrwGemLine.gpkg")
nrwGemPoints <- sf::read_sf("C:/Users/lwlmaster/Nextcloud/R/Palava/data/gpackage/nrwGemPoints.gpkg")
nrwGemPolygons <- sf::read_sf("C:/Users/lwlmaster/Nextcloud/R/Palava/data/gpackage/nrwGemPolygon.gpkg")
nrwShape <- sf::read_sf("C:/Users/lwlmaster/Nextcloud/R/Palava/data/gpackage/nrwShape.gpkg")
nrwVoronoi <- sf::read_sf("C:/Users/lwlmaster/Nextcloud/R/Palava/data/gpackage/nrwVoronoi.gpkg")

# Save to package data directory
usethis::use_data(nrwGemLine, overwrite = TRUE)
usethis::use_data(nrwGemPoints, overwrite = TRUE)
usethis::use_data(nrwGemPolygons, overwrite = TRUE)
usethis::use_data(nrwShape, overwrite = TRUE)
usethis::use_data(nrwVoronoi, overwrite = TRUE)