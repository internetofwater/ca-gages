###############################################################################################################################3###########
#
#     CODE TO ACCESS AND SAVE STATIC MAP LAYERS FOR CA STREAM GAGE DASHBOARD
#
###############################################################################################################################3###########

#create a project holding data and code in same folder
#Load Libaries ------------------------------------
## First specify the packages of interest
packages = c("nhdplusTools",
             "sf", "rgdal",  "rmapshaper","geojsonio", #spatial data packages
             "jsonlite", "purrr", "httr", # api packages
             "tidyverse", "lubridate" #basic data manipulation
             )

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

#set working directory
swd_data = "C:\\Users\\lap19\\Documents\\Internet of Water\\Pilots\\CA\\data\\"

###############################################################################################################################3###########
#
#     SAVE HUC 6, 8, and 12 geojsons
#
###############################################################################################################################3###########
# READ IN HUC6, HUC8, and HUC12 layers using Geoconnex
#Grab state
ca_feature <- sf::read_sf("https://info.geoconnex.us/collections/states/items?f=json&properties=STATEFP&STATEFP=06")
hu08_url <- paste0("https://info.geoconnex.us/collections/hu08/items?bbox=", paste(sf::st_bbox(ca_feature), collapse = ","), "&limit=300")
hu08 <- sf::read_sf(hu08_url)
mapview::mapview(hu08)

#which intersect the state
ca.huc8 <- st_join(ca_feature, hu08) 
ca.huc8 <- hu08 %>% filter(uri %in% ca.huc8$uri.y)
mapview::mapview(ca.huc8)
#save HUC8
geojson_write(ca.huc8, file="data/ca_huc8.geojson")
rm(hu08_url, hu08)


#####--------------------------------------------------
#HUC6
hu06_url <- paste0("https://info.geoconnex.us/collections/hu06/items?bbox=", paste(sf::st_bbox(ca_feature), collapse = ","), "&limit=300")
hu06 <- sf::read_sf(hu06_url)
mapview::mapview(hu06)

#which intersect the state
ca.huc6 <- st_join(ca_feature, hu06) 
ca.huc6 <- hu06 %>% filter(uri %in% ca.huc6$uri.y)
mapview::mapview(ca.huc6)
#save HUC6
geojson_write(ca.huc6, file="data/ca_huc6.geojson")
rm(hu06_url, hu06)


#####--------------------------------------------------
#HUC10
hu10_url <- paste0("https://info.geoconnex.us/collections/hu10/items?bbox=", paste(sf::st_bbox(ca_feature), collapse = ","), "&limit=3000")
hu10 <- sf::read_sf(hu10_url)
mapview::mapview(hu10)

#which intersect the state
ca.huc10 <- st_join(ca_feature, hu10) 
ca.huc10 <- hu10 %>% filter(uri %in% ca.huc10$uri.y)
mapview::mapview(ca.huc10)
#save HUC10
geojson_write(ca.huc10, file="data/ca_huc10.geojson")
rm(hu10_url, hu10)



#####--------------------------------------------------
#HUC12 is not on geoconnex and must be obtained elsewhere
#https://rdrr.io/cran/nhdplusTools/man/get_huc12.html
#grab huc 12s within this area of interest... none of these libraries are working for some reason
ca.huc12 <- get_huc12(AOI = ca.huc8); #takes a long time to do 
mapview::mapview(ca.huc12)

#save HUC12
geojson_write(ca.huc12, file="data/ca_huc12_full.geojson")

#file is huge - simplify to 50%... can see where overlap when zoom really far in... make a mapbox tile instead
ca.huc12.simple <- ca.huc12 %>% ms_simplify(keep = 0.8, keep_shapes=TRUE)
mapview::mapview(ca.huc12.simple)
geojson_write(ca.huc12.simple, file="data/ca_huc12.geojson")



###############################################################################################################################3###########
#
#     SAVE WATER BODIES AND NHDPLUS DATA
#
###############################################################################################################################3###########
####------------------------------------------------------------------------------------------------
#invalid geometries are breaking code
area1 = ca.huc8[1,]; mapview::mapview(area)
water_bodies1 <- get_waterbodies(AOI = area)

#pull major lakes and reservoirs from here: https://data.cnra.ca.gov/dataset/national-hydrography-dataset-nhd
download.file("https://data.cnra.ca.gov/dataset/511528b2-f7d3-4d86-8902-cc9befeeeed5/resource/ec84510b-2cd3-41c1-9f3e-849209582a72/download/nhd_majorlakesandreservoirs.zip", 
              destfile="C://Users//lap19//Downloads//nhd_majorlakesandreservoirs.zip")
# Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.
water_bodies <- readOGR("C://Users//lap19//Downloads//nhd_majorlakesandreservoirs//MajorLakesAndReservoirs", "MajorLakesAndReservoirs")
water_bodies <- spTransform(water_bodies, CRS("+init=epsg:4326")) %>% st_as_sf()
mapview::mapview(water_bodies)
geojson_write(water_bodies, file="data/water_bodies.geojson")


####------------------------------------------------------------------------------------------------
#Perhaps we pull in on the fly the NHDplus data of interest
area = ca.huc8[51,]; mapview::mapview(area)
#flowlines <- get_nhdplus(area, realization = "flowline", streamorder = 1); #returns that stream order
flowlines <- get_nhdplus(area, realization = "flowline"); #returns stream order greater than ore equal to this value
mapview::mapview(flowlines)

#Let's create a static flowline for zoomed out. Then once a huc8 12 is called we can zoom in
flowline.main <- get_nhdplus(ca.huc8, realization = "flowline", streamorder = 4); #returns that stream order and higher (bigger streams)
mapview::mapview(flowline.main)

#simplify
table(flowline.main$ftype, useNA="ifany"); 
table(flowline.main$wbareatype, useNA = "ifany")
flowline.main <- flowline.main %>% select(id, comid, gnis_id, gnis_name, lengthkm, reachcode,wbareacomi, ftype, streamleve, streamorde, wbareatype, lakefract)
mapview::mapview(flowline.main)
#save flowlines
geojson_write(flowline.main, file="data/main_flowlines.geojson")


#save out all flowlines even though will only show when zoom in - if in tiles it should load faster
flowline.all <- get_nhdplus(ca.huc8[1,], realization = "flowline"); 
for (i in 2:dim(flowline.all)[1]){
  zt <- get_nhdplus(ca.huc8[i,], realization = "flowline"); 
  flowline.all <- rbind(flowline.all, zt)
  
  print(ca.huc8[i,]$NAME)
}
mapview::mapview(flowline.all)
flowline.all <-  flowline.all %>% select(id, comid, gnis_id, gnis_name, lengthkm, reachcode,wbareacomi, ftype, streamleve, streamorde, wbareatype, lakefract)
#save flowlines
geojson_write(flowline.all, file="data/all_flowlines.geojson")


###############################################################################################################################3###########
#
#     SAVE OUT STREAM GAGE DATA
#
###############################################################################################################################3###########
#pull in linked nldi
g <- sf::read_sf("https://sb19.linked-data.internetofwater.dev/collections/ca_gages/items?limit=5000")
#get site id
g$site_no <- gsub("https://sb19.linked-data.internetofwater.dev/collections/ca_gages/items/","",g$id)
g <- g %>% mutate(flow_status = ifelse(flow_status == "",NA, flow_status))


#remove duplicates with USGS
#g <- g %>% filter(datasource != "CDEC" | datasource == "CDEC" & operator != "USGS") 
table(g$datasource, g$flow_status, useNA="ifany")
table(g$datasource, g$sitestatus, useNA="ifany")
table(g$datasource, g$operator)
g <- g %>% select(-fid, -streamgages_objectid, -gnisid_medres, -x_orig, -y_orig, -totdasqkm, -reach_measure, -provider)
geojson_write(g, file="data/stream_gages.geojson")



###############################################################################################################################3###########
#
#     READ IN VAL DATA FROM GOOGLE DRIVE: https://drive.google.com/drive/folders/1SNWE6zYFDFp8uM0JT6gIca9glrbfdBfL
#
###############################################################################################################################3###########
#manual download and unzip
fgdb_flood <- "C:\\Users\\lap19\\Downloads\\2_Flood\\2_Flood\\Flood_EXPORT.gdb"

# List all feature classes in a file geodatabase
fc_list <- ogrListLayers(fgdb_flood)
print(fc_list)

# Read the feature class
firo_gage <- sf::st_read(fgdb_flood, layer = "FIRO_gage_upgrades")
mapview::mapview(firo_gage)
head(firo_gage) %>% as.data.frame()

firo_huc10 <- sf::st_read(fgdb_flood, layer = "FIRO_HUC10"); #The huc 10 and huc12 does not match gage locations
mapview::mapview(firo_huc10)
head(firo_huc10) %>% as.data.frame()


#ca api link https://gispublic.waterboards.ca.gov/portal/home/item.html?id=32dfb85bd2744487affe6e3475190093
g2 <- sf::read_sf("https://gispublic.waterboards.ca.gov/portalserver/rest/services/Hosted/StreamGages_20210114/FeatureServer"); #also did not work
#downloaded
g2 <- sf::st_read("C:\\Users\\lap19\\Downloads", layer="ca_streamgage2"); #it is a little different
table(g2$datasource, g2$flow_statu, useNA="ifany")
table(g2$floodmgmt); #same








###############################################################################################################################3###########
#
#     CREATE MAPBOX TILESETs OF HUC 12 and NHDLI DATA
#
###############################################################################################################################3###########
#create a tippecanoe script



