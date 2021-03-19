library(sf)
library(esri2sf)

g <- esri2sf("https://gispublic.waterboards.ca.gov/portalserver/rest/services/Hosted/StreamGages_20210114/FeatureServer/0")


g$comid <- paste0("https://geoconnex.us/nhdplusv2/comid/",g$comid_medres)
g$hu08 <- paste0("https://geoconnex.us/ref/hu08/",g$huc8)
g$hu10 <- paste0("https://geoconnex.us/ref/hu10/",g$huc10)
g$hu12 <- paste0("https://geoconnex.us/nhdplusv2/huc12/",g$huc12)

g$provider <- NA
g$provider[which(g$datasource=="HADS")] <- "https://hads.ncep.noaa.gov"
g$provider[which(g$datasource=="NWIS")] <- "https://waterdata.usgs.gov"
g$provider[which(g$datasource=="CDEC")] <- "https://cdec.water.ca.gov"

g$weblink[which(g$datasource=="NWIS")] <- paste0("https://geoconnex.us/usgs/monitoring-location/",g$siteid[which(g$datasource=="NWIS")])

g$id <- paste0("https://sb19.linked-data.internetofwater.dev/collections/ca_gages/items/",g$siteid)

st_write(g,"../data/ca_gages.gpkg")
