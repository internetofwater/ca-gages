library(sf)
library(esri2sf)
library(dplyr)

target_url <- "https://sb19.linked-data.internetofwater.dev/collections/ca_gages/items/"

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

g$uri <- paste0("https://geoconnex.us/ca-gage-assessment/gages/",g$siteid)

vaa <- nhdplusTools::get_vaa()

g$comid_medres <- as.numeric(g$comid_medres)
g <- dplyr::left_join(g, dplyr::select(vaa, comid, frommeas), by = c("comid_medres"="comid"))

cd <- sf::read_sf("https://info.geoconnex.us/collections/gages/items?limit=10000000")
cd <- select(cd,provider_id:nhdpv2_COMID)
cd <- st_drop_geometry(cd)

g2 <- left_join(g,cd,by=c("siteid"="provider_id"))
g2$frommeas[which(g2$rchcd_medres==g2$nhdpv2_REACHCODE & g2$comid_medres == g2$nhdpv2_COMID)] <- g2$nhdpv2_REACH_measure[which(g2$rchcd_medres==g2$nhdpv2_REACHCODE & g2$comid_medres == g2$nhdpv2_COMID)] 

st_write(g,"../data/ca_gages.gpkg")
st_write(g,"../../Linked Data Server/data/ca_gages.gpkg",overwrite=TRUE, append=FALSE)

pids <- g %>% select(uri, sitename, siteid) %>% st_drop_geometry()

pids$id <- pids$uri
pids$target <- paste0(target_url,pids$siteid)
pids$creator <- "kyle.onda@duke.edu"
pids$description <- paste0("California Streamgage Network Assessment Catalog, site named",pids$sitename)
pids <- select(pids,id,target,creator,description)

write_csv(pids,"../../Linked Data Server/data/ca_gages_pids.csv",overwrite=FALSE, append=FALSE)
