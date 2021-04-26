library(nhdplusTools)
library(sf)
library(tidyverse)
library(mapview)

g <- read_sf("https://geoconnex.us/ca-gage-assessment/ca_gages/items")
start <- read_sf("https://geoconnex.us/ca-gage-assessment/gages/ANH")

start_nldi <- list("featuresource" = "huc12pp", featureID = start$huc12)

upstream_main <- navigate_nldi(nldi_feature = start_nldi,
                                mode = "UM",
                                distance_km = 100)$UM %>% st_geometry()


upstream_tribs <- navigate_nldi(nldi_feature = start_nldi,
                   mode = "UT",
                   distance_km = 100)$UT %>% st_geometry()

ca_gages <- read_sf("https://labs.waterdata.usgs.gov/api/nldi/linked-data/huc12pp/180400030907/navigation/UT/ca_gages?distance=100")
x <- do.call(rbind,lapply(ca_gages$uri, read_sf))

wqp <- read_sf("https://labs.waterdata.usgs.gov/api/nldi/linked-data/huc12pp/180400030907/navigation/UT/wqp?distance=100")
rights <-  read_sf("https://labs.waterdata.usgs.gov/api/nldi/linked-data/huc12pp/180400030907/navigation/UT/wade?distance=100")


mapview(upstream_main, color="darkblue",lwd=4) +
  mapview(upstream_tribs) +
  mapview(x, zcol="datasource", layer.name="SB19 Gages by Source") + 
  mapview(wqp, layer.name="Water Quality Portal",cex=2,col.regions="green") +
  mapview(rights, layer.name="Water Right Points of Diversion",cex=2,col.regions="white")
  
