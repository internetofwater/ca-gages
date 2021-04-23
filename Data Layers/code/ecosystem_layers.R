library(sf)
library(esri2sf)
library(dplyr)
library(gdalUtilities)

ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

# first download ecosystem geodatabase from hydroshare to ../data/Ecosystem/SB19_Ecosystem_Layers.gdb
# https://www.hydroshare.org/resource/ef6cb3b8cd7341719db8a226ac558e84/

layers <- st_layers("../data/Ecosystem/SB19_Ecosystem_Layers.gdb")$name

biodiversity <- sf::read_sf("../data/Ecosystem/SB19_Ecosystem_Layers.gdb",layer=layers[1])
freshwater <- sf::read_sf("../data/Ecosystem/SB19_Ecosystem_Layers.gdb",layer=layers[2])
wetlands <- sf::read_sf("../data/Ecosystem/SB19_Ecosystem_Layers.gdb",layer=layers[3])
vegetation <- sf::read_sf("../data/Ecosystem/SB19_Ecosystem_Layers.gdb",layer=layers[4])

biodiversity <- ensure_multipolygons(biodiversity)
freshwater <- ensure_multipolygons(freshwater)
wetlands <- ensure_multipolygons(wetlands)
vegetation <- ensure_multipolygons(vegetation)

wetlands2 <- ensure_multipolygons(wetlands)
wl2 <- st_join(wetlands2,biodiv)

list <- NA
for(i in 1:length(layers)){
  list[i] <- sf::read_sf("../data/Ecosystem/SB19_Ecosystem_Layers.gdb",layer=layers[i])
}
x <- sf::read_sf("../data/Ecosystem/SB19_Ecosystem_Layers.gdb",layer=layers[1])