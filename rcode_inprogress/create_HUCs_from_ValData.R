###############################################################################################################################3###########
#
#     READ IN VAL DATA FROM GOOGLE DRIVE: https://drive.google.com/drive/folders/1SNWE6zYFDFp8uM0JT6gIca9glrbfdBfL
#
###############################################################################################################################3###########
rm(list = ls())
.rs.restartR()

#Load Libaries ------------------------------------
## First specify the packages of interest
packages = c("nhdplusTools",
             "sf", "rgdal",  "rmapshaper","geojsonio","leaflet", #spatial data packages
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
rm(packages, package.check)


#New SF package creates problems
sf::sf_use_s2(FALSE)

#set working directory
#swd_data = "C:\\Users\\lap19\\Documents\\Internet of Water\\Pilots\\CA\\data\\"
huc10 <- read_sf("data/ca_huc10.geojson")
huc12 <- read_sf("data/ca_huc12.geojson"); #using the full HUC 12 file did not change results notably


###############################################################################################################################3###########
#
#     GAGE GAP DATA
#
###############################################################################################################################3###########
#manual download and unzip and read in shapefile
gage_gap <- sf::st_read("C:\\Users\\lap19\\Downloads\\1_GageGap", layer="Streams_NHD5kplus_GGII")
gage_gap <- gage_gap %>% st_as_sf() %>% st_transform(4326) %>% select(COMID, GGIIStatus, StrmOrder, SU_Name, Gaged, geometry) %>% st_zm()
#link to HUC12 and simplify columns

gage_huc <- st_intersection(huc12, gage_gap) 
  zt <- gage_huc %>% filter(huc12=="150301040404")# %>% mutate(length = as.numeric(st_length(zt$geometry)))
  yt <- huc12 %>% filter(huc12 %in% zt$huc12)
#some snipping of streams along the edges of HUC12
  
mapview::mapview(yt, col.regions = "gray", alpha.regions = 0.4, cex=3, layer.name="HUC12") + mapview::mapview(zt, col.regions = "blue", alpha.regions = 0.6, cex=4, layer.name = "streams")
#simplify columns
gage_huc <- gage_huc %>% select(COMID, GGIIStatus, StrmOrder, SU_Name, Gaged, geometry, huc12)

#write geojson
geojson_write(gage_huc, file="data/gage_gap.geojson")
#---------------------------------------------------------------------------------------------------------------------------------------------

gage_huc <- read_sf("data/gage_gap.geojson")

#START BUILDING HUC 10 and HUC 12 STATUS
#what is the gage status in a huc 10 and 12 by stream order
gage_huc10 <- gage_huc
st_geometry(gage_huc10) <- NULL
gage_huc10 <- gage_huc10 %>% mutate(huc10 = substr(huc12,1,10)) %>% select(huc10, StrmOrder, GGIIStatus) %>% group_by(huc10, StrmOrder, GGIIStatus) %>% mutate(n=n()) %>% distinct()

gage_huc10 <- gage_huc10 %>% spread(key=GGIIStatus, value = n)  
#lets group stream orders and calculate
gage_huc10 <- gage_huc10 %>% mutate(stream_size = ifelse(StrmOrder <=3, "headwaters", ifelse(StrmOrder >=4 & StrmOrder <= 6, "streams", "rivers"))) %>% 
  group_by(huc10, stream_size) %>% summarize(poor_gage = sum(`Poorly Gaged`, na.rm=TRUE), some_gage = sum(`Almost Well Gaged`, na.rm=TRUE), well_gage = sum(`Well Gaged`, na.rm=TRUE), .groups="drop")
#lets calculate percent well gaged, gather, and spread by stream_size
gh10 <- gage_huc10 %>% mutate(per_well_gage = round(well_gage/(poor_gage + some_gage + well_gage)*100,2)) %>% select(huc10, stream_size, per_well_gage) %>% spread(stream_size, per_well_gage)
gh10.some <- gage_huc10 %>% mutate(per_some_gage = round(some_gage/(poor_gage + some_gage + well_gage)*100,2)) %>% select(huc10, stream_size, per_some_gage) %>% spread(stream_size, per_some_gage)
gh10.poor <- gage_huc10 %>% mutate(per_poor_gage = round(poor_gage/(poor_gage + some_gage + well_gage)*100,2)) %>% select(huc10, stream_size, per_poor_gage) %>% spread(stream_size, per_poor_gage)

#note that if zero streams that size were present. #if NA no streams that size present in huc10
colnames(gh10) <- c("huc10", "headwater_well", "river_well", "stream_well")
colnames(gh10.some) <- c("huc10", "headwater_some", "river_some", "stream_some")
colnames(gh10.poor) <- c("huc10", "headwater_poor", "river_poor", "stream_poor")

#merge together
gh10 <- merge(gh10, gh10.some, by="huc10")
gh10 <- merge(gh10, gh10.poor, by="huc10")

#let's merge with dataframe
ca.huc10 <- huc10 %>% select(id, NAME, geometry) %>% rename(huc10 = id, name=NAME)
ca.huc10 <- merge(ca.huc10, gh10, by.x="huc10", by.y="huc10", all.x=TRUE)

#map well gaged headwaters
zt <- ca.huc10 %>% filter(river_well > 75)
yt <- ca.huc10 %>% filter(stream_well > 75)
xt <- ca.huc10 %>% filter(headwater_well > 75)

leaflet() %>%
  addProviderTiles("Stamen.TonerLite", group="Toner by Stamen") %>% 
  addPolygons(data = zt, group = "rivers",  color ="black", weight = 0, fillOpacity= 0.5, fillColor = "blue", label = ~paste0(river_well) ) %>% 
  addPolygons(data = yt, group = "streams", color ="black", weight = 0, fillOpacity= 0.5, fillColor = "purple", label = ~paste0(stream_well) ) %>% 
  addPolygons(data = xt, group = "headwaters", color ="black", weight = 0,  fillOpacity= 0.5, fillColor = "green", label = ~paste0(headwater_well) ) %>% 
  addLegend("topright", 
            colors = c("green", "purple", "blue"),  labels= c("headwaters", "streams", "rivers"),
            title = "> 75% classified as well gaged", opacity = 1)

zt <- ca.huc10 %>% filter(river_poor > 75)
yt <- ca.huc10 %>% filter(stream_poor > 75)
xt <- ca.huc10 %>% filter(headwater_poor > 75)
leaflet() %>%
  addProviderTiles("Stamen.TonerLite", group="Toner by Stamen") %>% 
  addPolygons(data = xt, group = "headwaters", color ="black", weight = 0, fillOpacity= 0.5, fillColor = "orange", label = ~paste0(headwater_poor) ) %>% 
  addPolygons(data = yt, group = "streams", color ="black", weight = 0, fillOpacity= 0.5, fillColor = "red", label = ~paste0(stream_poor) ) %>% 
  addLegend("topright", 
            colors = c("orange", "red", "darkred"), labels= c("headwaters", "streams", "rivers"),
            title = "> 75% classified as poorly gaged",  opacity = 1)
rm(gh10, gh10.poor, gh10.some, gage_huc10)

### REPEAT FOR HUC 12 ----------------------------------------------------------------------------------------------------------------------
gage_huc12 <- gage_huc
st_geometry(gage_huc12) <- NULL
gage_huc12 <- gage_huc12 %>% select(huc12, StrmOrder, GGIIStatus) %>% group_by(huc12, StrmOrder, GGIIStatus) %>% mutate(n=n()) %>% distinct()

gage_huc12 <- gage_huc12 %>% spread(key=GGIIStatus, value = n)  
#lets group stream orders and calculate
gage_huc12 <- gage_huc12 %>% mutate(stream_size = ifelse(StrmOrder <=3, "headwaters", ifelse(StrmOrder >=4 & StrmOrder <= 6, "streams", "rivers"))) %>% 
  group_by(huc12, stream_size) %>% summarize(poor_gage = sum(`Poorly Gaged`, na.rm=TRUE), some_gage = sum(`Almost Well Gaged`, na.rm=TRUE), well_gage = sum(`Well Gaged`, na.rm=TRUE), .groups="drop")
#lets calculate percent well gaged, gather, and spread by stream_size
gh12 <- gage_huc12 %>% mutate(per_well_gage = round(well_gage/(poor_gage + some_gage + well_gage)*100,2)) %>% select(huc12, stream_size, per_well_gage) %>% spread(stream_size, per_well_gage)
gh12.some <- gage_huc12 %>% mutate(per_some_gage = round(some_gage/(poor_gage + some_gage + well_gage)*100,2)) %>% select(huc12, stream_size, per_some_gage) %>% spread(stream_size, per_some_gage)
gh12.poor <- gage_huc12 %>% mutate(per_poor_gage = round(poor_gage/(poor_gage + some_gage + well_gage)*100,2)) %>% select(huc12, stream_size, per_poor_gage) %>% spread(stream_size, per_poor_gage)

#note that if zero streams that size were present. #if NA no streams that size present in huc10
colnames(gh12) <- c("huc12", "headwater_well", "river_well", "stream_well")
colnames(gh12.some) <- c("huc12", "headwater_some", "river_some", "stream_some")
colnames(gh12.poor) <- c("huc12", "headwater_poor", "river_poor", "stream_poor")

#merge together
gh12 <- merge(gh12, gh12.some, by="huc12")
gh12 <- merge(gh12, gh12.poor, by="huc12")

#let's merge with dataframe
ca.huc12 <- huc12 %>% select(huc12, name, geometry)
ca.huc12 <- merge(ca.huc12, gh12, by.x="huc12", by.y="huc12", all.x=TRUE)

#map well gaged headwaters
zt <- ca.huc12 %>% filter(river_well > 75)
yt <- ca.huc12 %>% filter(stream_well > 75)
xt <- ca.huc12 %>% filter(headwater_well > 75)

leaflet() %>%
  addProviderTiles("Stamen.TonerLite", group="Toner by Stamen") %>% 
  addPolygons(data = zt, group = "rivers",  color ="black", weight = 0, fillOpacity= 0.5, fillColor = "blue", label = ~paste0(river_well) ) %>% 
  addPolygons(data = yt, group = "streams", color ="black", weight = 0, fillOpacity= 0.5, fillColor = "purple", label = ~paste0(stream_well) ) %>% 
  addPolygons(data = xt, group = "headwaters", color ="black", weight = 0,  fillOpacity= 0.5, fillColor = "green", label = ~paste0(headwater_well) ) %>% 
  addLegend("topright", 
            colors = c("green", "purple", "blue"),  labels= c("headwaters", "streams", "rivers"),
            title = "> 75% classified as well gaged", opacity = 1)

zt <- ca.huc10 %>% filter(river_poor > 75)
yt <- ca.huc10 %>% filter(stream_poor > 75)
xt <- ca.huc10 %>% filter(headwater_poor > 75)
leaflet() %>%
  addProviderTiles("Stamen.TonerLite", group="Toner by Stamen") %>% 
  addPolygons(data = xt, group = "headwaters", color ="black", weight = 0, fillOpacity= 0.5, fillColor = "orange", label = ~paste0(headwater_poor) ) %>% 
  addPolygons(data = yt, group = "streams", color ="black", weight = 0, fillOpacity= 0.5, fillColor = "red", label = ~paste0(stream_poor) ) %>% 
  addLegend("topright", 
            colors = c("orange", "red", "darkred"), labels= c("headwaters", "streams", "rivers"),
            title = "> 75% classified as poorly gaged",  opacity = 1)
rm(gh12, gh12.poor, gh12.some, gage_huc12)
############################################################################################################################################################################






###############################################################################################################################3###########
#
#     FLOOD DATA
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

firo_huc12 <- sf::st_read(fgdb_flood, layer = "FIRO_HUC12"); #The huc 10 and huc12 does not match gage locations
mapview::mapview(firo_huc12)
head(firo_huc12) %>% as.data.frame()

#lets add FIRO to huc10 and huc 12
ca.huc10 <- ca.huc10 %>% mutate(firo = ifelse(huc10 %in% firo_huc10$huc10, "yes", "no")); table(ca.huc10$firo, useNA="ifany")
ca.huc12 <- ca.huc12 %>% mutate(firo = ifelse(huc12 %in% firo_huc12$huc12, "yes", "no")); table(ca.huc12$firo, useNA="ifany")

rm(firo_huc10, firo_huc12)




###############################################################################################################################3###########
#
#     ECOSYSTEM DATA
#
###############################################################################################################################3###########
ace <- sf::st_read("C:\\Users\\lap19\\Downloads\\2_Ecosystem\\ACE_aquatic_biodiversity_rank\\ACE_ds2768_aquatic_biodiversity_summary", layer="ACE_ds2768_aquatic_biodiversity_summary")
#am assuming the fields of interest are BioAqRanks, NtVAqRanks, and RarAqRanks
ace <- ace %>% st_as_sf() %>% st_transform(4326) %>% select(HUC12, BioAqRankS, NtvAqRankS, RarAqRankS, geometry) %>% st_zm() %>% rename(huc12=HUC12) %>% mutate(huc10 = substr(huc12, 1,10))
st_geometry(ace) <- NULL

ace10 <- ace %>% group_by(huc10) %>% summarize(BioAqRankS = round(mean(BioAqRankS, na.rm=TRUE),2), NtvAqRankS = round(mean(NtvAqRankS, na.rm=TRUE),2), 
                                               RarAqRankS = round(mean(RarAqRankS, na.rm=TRUE),2), .groups="drop")

ca.huc10 <- merge(ca.huc10, ace10, by.x="huc10", by.y="huc10", all.x=TRUE)
ca.huc12 <- merge(ca.huc12, ace[,c("huc12", "BioAqRankS", "NtvAqRankS", "RarAqRankS")], by.x="huc12", by.y="huc12", all.x=TRUE)

zt <- ca.huc12 %>% filter(BioAqRankS >= 4); yt <- ca.huc12 %>% filter(NtvAqRankS >= 4); xt <- ca.huc12 %>% filter(RarAqRankS >= 4)
leaflet() %>%
  addProviderTiles("Stamen.TonerLite", group="Toner by Stamen") %>% 
  addPolygons(data = zt, group = "Bio",  color ="black", weight = 0, fillOpacity= 0.5, fillColor = "blue") %>% 
  addPolygons(data = yt, group = "Native", color ="black", weight = 0, fillOpacity= 0.5, fillColor = "purple" ) %>% 
  addPolygons(data = xt, group = "Rare", color ="black", weight = 0,  fillOpacity= 0.5, fillColor = "green" ) %>% 
  addLegend("topright", 
            colors = c("blue", "purple", "green"),  labels= c("Bio", "Native", "Rare"),
            title = "Aquatic Biodiversity >= 4", opacity = 1)

#add to hucs
ca.huc10 <- ca.huc10 %>% rename(bio_rank = BioAqRankS, native_rank = NtvAqRankS, rare_rank = RarAqRankS)
ca.huc12 <- ca.huc12 %>% rename(bio_rank = BioAqRankS, native_rank = NtvAqRankS, rare_rank = RarAqRankS)

ca.huc12 <- ca.huc12 %>% mutate(colcat = ifelse(bio_rank>=4, "#3680cd", ifelse(bio_rank < 4 & bio_rank >=3 , "#36bdcd", 
                                         ifelse(bio_rank < 3 & bio_rank >= 2, "#cd8536", ifelse(bio_rank < 2 & bio_rank >= 1,"#ea3119", ifelse(bio_rank < 1, "#71261c", "white"))))))
ca.huc12$colcat <- ifelse(is.na(ca.huc12$bio_rank) ==TRUE, "white", ca.huc12$colcat)  ;     table(ca.huc12$colcat, useNA="ifany")
leaflet() %>%
  addProviderTiles("Stamen.TonerLite", group="Toner by Stamen") %>% 
  addPolygons(data = ca.huc12, group = "Bio",  color = "black", weight = 0, fillOpacity= 0.5, fillColor = ~colcat) %>% 
  addLegend("topright", 
            colors=c("#3680cd", "#36bdcd", "#cd8536", "#ea3119", "#71261c", "white"), labels = c(">=4", "3 to 3.9", "2 to 2.9", "1 to 1.9", "<1", "unknown"),
            title="Aquatic Biodiversity Index", opacity=1)
  
ca.huc12 <- ca.huc12 %>% select(-colcat)


rm(ace, ace10)
#------------------------------------------------------------------------------------------------------------------------------------
#CALIFORNIA WATER BLUE PRINT DATA

bprint <- sf::st_read("C:\\Users\\lap19\\Downloads\\2_Ecosystem\\CA_FWBlueprint_v1", layer="CA_FWBlueprint_v1")
#assume want the condition index, threat index and conservation strategy
st_geometry(bprint) <- NULL

bprint <- bprint %>% select(HUC_12, cond_index, threat_ind, cons_strat) %>% rename(huc12 = HUC_12)
bp10 <- bprint %>% mutate(huc10 = substr(huc12,1,10)) %>% group_by(huc10) %>% #assume median
  summarize(cond_index = round(median(cond_index, na.rm=TRUE),2), threat_index = round(median(threat_ind, na.rm=TRUE),2))

#how to assess conservation strategy percent huc 12s in restore, secure, monitor and mitigate buckets... or must take the max value
bp.strat = bprint %>% mutate(huc10 = substr(huc12,1,10))
table(bp.strat$huc10, bp.strat$cons_strat)
#bp.strat <- bp.strat %>% mutate(word1 = str_trim(word(cons_strat, 1, sep="\\&")), word2 = str_trim(word(cons_strat, 2, sep="\\&"))) %>% select(huc10, huc12, word1, word2)
bp.strat <- bp.strat %>% group_by(huc10, cons_strat) %>% mutate(n=n()) %>% ungroup() %>% select(huc10, cons_strat, n) %>% distinct()
bp.strat <- bp.strat %>% group_by(huc10) %>% filter(n == max(n)) %>% ungroup()
#if tied prioritize mitigation and then securing
bp.strat <- bp.strat %>% group_by(huc10) %>% mutate(n_hucs = n()) %>% ungroup() %>% mutate(keep = "keep")
bp.strat <- bp.strat %>% mutate(word2 = str_trim(word(cons_strat, 2, sep="\\&"))) %>% mutate(keep = ifelse(n_hucs >= 2 & word2 != "Mitigate", "drop", keep))
bp.strat <- bp.strat %>% mutate(word1 = str_trim(word(cons_strat, 1, sep="\\&"))) %>% mutate(keep = ifelse(n_hucs >=2 & word2 == "Monitor" & word1 == "Secure", "keep", keep))
bp.strat <- bp.strat %>% filter(keep == "keep")

bp.strat <- bp.strat %>% group_by(huc10) %>% mutate(n_hucs = n()) %>% ungroup() %>% mutate(keep = "keep")
bp.strat <- bp.strat %>% mutate(keep = ifelse(n_hucs >= 2 & word2 != "Mitigate", "drop", keep))

bp.strat <- bp.strat %>% filter(keep == "keep")

bp.strat <- bp.strat %>% group_by(huc10) %>% mutate(n_hucs = n()) %>% ungroup() %>% mutate(keep = "keep")
bp.strat <- bp.strat %>% mutate(keep = ifelse(n_hucs >= 2 & word1 != "Secure", "drop", keep))
bp.strat <- bp.strat %>% filter(keep == "keep")


#combine to bp10
bp10 <- merge(bp10, bp.strat[,c("huc10", "cons_strat")], by="huc10", all.x=TRUE)
bprint <- bprint %>% rename(threat_index = threat_ind) %>% mutate(cond_index = round(cond_index, 2), threat_index = round(threat_index,2))

#add to hucs
ca.huc10 <- merge(ca.huc10, bp10, by.x="huc10", by.y="huc10", all.x=TRUE)
ca.huc12 <- merge(ca.huc12, bprint, by.x="huc12", by.y="huc12", all.x=TRUE)

#plot strategies
ca.huc12 <- ca.huc12 %>% mutate(colcat = ifelse(cons_strat=="Secure & Monitor", "#3680cd", ifelse(cons_strat == "Restore & Monitor" , "#36bdcd", 
                                         ifelse(cons_strat=="Secure & Mitigate", "#cd8536", ifelse(cons_strat == "Restore & Mitigate","#ea3119", "white")))))
ca.huc12$colcat <- ifelse(is.na(ca.huc12$cons_strat) ==TRUE, "white", ca.huc12$colcat)  ;     table(ca.huc12$colcat, useNA="ifany")
leaflet() %>%
  addProviderTiles("Stamen.TonerLite", group="Toner by Stamen") %>% 
  addPolygons(data = ca.huc12, group = "Bio",  color = "black", weight = 0, fillOpacity= 0.5, fillColor = ~colcat) %>% 
  addLegend("topright", 
            colors=c("#3680cd", "#36bdcd", "#cd8536", "#ea3119", "white"), labels = c("Secure & Monitor", "Restore & Monitor", "Secure & Mitigate", "Restore & Mitigate", "unknown"),
            title="Conservation Strategy", opacity=1)
ca.huc12 <- ca.huc12 %>% select(-colcat)

rm(bprint, bp10, bp.strat)


#------------------------------------------------------------------------------------------------------------------------------------
#Groundwater
#manual download and unzip
fgdb_gw <- "C:\\Users\\lap19\\Downloads\\2_Ecosystem\\i02_NCCAG_v2_0\\i02_NaturalCommunitiesCommonlyAssociatedwithGroundwater_v2_0.gdb"

# List all feature classes in a file geodatabase
fc_list <- ogrListLayers(fgdb_gw)
print(fc_list)

# Read the feature class - can do but takes forever... faster to do in ArCMAP
#wetlands <- sf::st_read(fgdb_gw, layer = "i02_NCCAG_Wetlands_2_0")
 #wl <- wetlands %>% st_as_sf() %>% st_transform(4326) %>% st_zm()
 #as.data.frame(table(st_geometry_type(wl)))
#to fix the unsupported geometry type - multisurface
 #wl2 = st_cast(wl, "MULTIPOLYGON")
 #wl2 = wl2 %>% ms_simplify(keep=0.5, keep_shapes=TRUE)
#fix corrupt shapefiles for block group... takes awhile can try to skip
 #if (FALSE %in% st_is_valid(wl2)) {wl2 <- suppressWarnings(st_buffer(wl2[!is.na(st_is_valid(wl2)),], 0.0)); print("fixed")}
 #wl2 <- wl2 %>% group_by(WETLAND_NAME, ORIGINAL_CODE, SOURCE_CODE) %>% summarize(n=n(), .groups="drop")
 #mapview::mapview(wl2)


#wetlands <- sf::st_read("C:\\Users\\lap19\\Downloads\\2_Ecosystem\\i02_NCCAG_v2_0", layer = "wetlands_dissolve")
#  as.data.frame(table(st_geometry_type(wetlands)))
#breaks r - tried ms_simplify website: https://mapshaper.org/
#wetlands <- wetlands %>% st_as_sf() %>% st_transform(4326) %>% ms_simplify(keep=0.5, keep_shapes=TRUE)

#write geojson
#geojson_write(wetlands, file="results/wetlands.geojson")


#These files are huge - probably better to say % wetland in HUC10 and in HUC12
#rm(wetlands, wl)




#write geojson
geojson_write(ca.huc10, file="results/huc10_data.geojson")
geojson_write(ca.huc12, file="results/huc12_data.geojson")

#read in if need to
ca.huc10 <- read_sf("results/huc10_data.geojson")
ca.huc12 <- read_sf("results/huc12_data.geojson")


###############################################################################################################################3###########
#
#     STREAM GAGE DATA
#
###############################################################################################################################3###########
#read in stream gages
g <- read_sf("data/stream_gages.geojson")
g <- g %>% select(-totdasqmi, -ref_gagesii, -hu08, -hu10, -hu12, -uri, -id)
firo_gage <- firo_gage %>% rename(site_id = Gage_ID, operator = Operator) %>% mutate(flow_status = "Active", floodmgmt = "Y")
#I think these gages should be classified as flood management rather than "NA"
g <- g %>% mutate(firo = ifelse(site_id %in% firo_gage$site_id, "Y", "N"), floodmgmt = ifelse(site_id %in% firo_gage$site_id, "Y", floodmgmt))
#5 FIRO gages operated by the Corps around Lake Medicino are currently not added because no site_id and not in stream gage network but it covers locations with gages indicated
zt <- g %>% filter(firo=="Y");

mapview::mapview(firo_huc10, col.region="gray", alpha.region = 0.5, layer.name="FIRO HUCS") + mapview::mapview(firo_gage, col.region="orange", layer.name="FIRO Gages") + mapview::mapview(zt, col.region="blue", layer.name="Stream Gages")

#clean up metadata
######################################################################################################################################################################
#
#   METADATA OF INTEREST
#
######################################################################################################################################################################
#Table by data source
table(g$datasource, useNA="ifany");
g <- g %>% mutate(datasource = ifelse(is.na(datasource), "Unknown", datasource))

#Table of operators and active
g <- g %>% mutate(operator2 = ifelse(substr(operator,1,2)=="CA" | grepl("DWR",operator), "California", operator)) %>% 
  mutate(operator2 = ifelse(grepl("Irrigation", operator) | grepl(" ID ", operator), "Irrigation Districts", operator2)) %>% 
  mutate(operator2 = ifelse(grepl("Pacific Gas", operator) | grepl("Power", operator), "Energy Utilities", operator2)) %>% 
  mutate(operator2 = ifelse(substr(operator,1,3)=="US " | grepl("Reclamation", operator), "Federal (non-USGS)", operator2)) %>% 
  mutate(operator2 = ifelse(grepl("Conservation", operator) | grepl("Consv", operator), "Conservation Districts", operator2)) %>% 
  mutate(operator2 = ifelse(grepl("County", operator) | grepl("City", operator), "City or County", operator2)) %>% 
  mutate(operator2 = ifelse(grepl("Utility", operator) | grepl("Authority", operator) | grepl("Water", operator2), "Water Utilities", operator2)) %>% 
  mutate(operator2 = ifelse(operator=="OTHER", "Other", operator2))
table(g$operator2, useNA="ifany")
g <- g %>% mutate(operator2 = ifelse(is.na(operator), "Other", operator2))

table(g$operator2, g$sitestatus) %>% as.data.frame() %>% spread(Var2, Freq) %>% rename(Operator = Var1)
df <- g %>% filter(sitestatus == "Active")
table(df$operator2, df$streamtype) %>% as.data.frame() %>% spread(Var2, Freq) %>% rename(Operator = Var1)
#table(df$operator2, df$huc8) %>% as.data.frame() %>% spread(Var2, Freq) %>% rename(Operator = Var1)
#write g
geojson_write(g, file="results/stream_gages.geojson")


#What does this look like by HUC?
#Info by type of data collected ########################################################################################################################################################
#Table of purpose...not stage status has a lot of "NA"... therefore use site status... can add more hucs or other variables
df.stage <- st_drop_geometry(df) %>% group_by(sitestatus, huc10, stage_yn) %>% count() %>% mutate(stage_yn = paste0("stage_",stage_yn)) %>% rename(variable = stage_yn) # %>% spread(stage_yn, n)
df.flow <- st_drop_geometry(df) %>% group_by(sitestatus, huc10, flow_yn) %>% count() %>% mutate(flow_yn = paste0("flow_",flow_yn)) %>% rename(variable = flow_yn) #%>% spread(flow_yn, n)
df.wq <- st_drop_geometry(df) %>% group_by(sitestatus, huc10, watqual_yn) %>% count() %>% mutate(watqual_yn = paste0("watqual_",watqual_yn)) %>% rename(variable = watqual_yn)
df.temp <- st_drop_geometry(df) %>% group_by(sitestatus, huc10, temp_yn) %>% count() %>% mutate(temp_yn = paste0("temp_",temp_yn)) %>% rename(variable = temp_yn)

df.data <- rbind(df.stage, df.flow, df.wq, df.temp) #%>% spread(variable, n)
df.data[is.na(df.data)]<-0;


filterBy = "stage"; #options: stage, flow, watqual, temp
df.huc10 <- df.data %>% filter(grepl(filterBy, variable)) %>% mutate(variable = substr(variable, nchar(variable), nchar(variable))) %>% spread(variable, n)

#only plotting active sites here that monitor stages
huc10.stage <- merge(ca.huc10, df.huc10 %>% filter(sitestatus=="Active"), by.x="huc10", by.y="huc10", all.x=TRUE)
huc10.stage[is.na(huc10.stage)]<-0
#pal <- colorNumeric(c("white", "navy"), 0:60)
#binpal <- colorBin("Blues", huc8.stage[paste0(filterBy,"_Y")], c(0,1,5,10,25,50,100), pretty = FALSE)
if(filterBy == "stage" | filterBy == "flow"){
  binpal <- colorBin("Blues", huc10.stage$Y, c(0,1,5,10,15,100), pretty = FALSE)
}

if(filterBy == "watqual"){
  binpal <- colorBin("Purples", huc10.stage$Y, c(0,1,5,10,15,100), pretty = FALSE)
}

if(filterBy == "temp"){
  binpal <- colorBin("Reds", huc10.stage$Y, c(0,1,5,10,15,100), pretty = FALSE)
}

leaflet() %>% addProviderTiles("Stamen.TonerLite") %>% 
  addPolygons(data = huc10.stage,
              fillOpacity= 0.8, fillColor = ~binpal(Y),
              stroke=FALSE,
              label = ~paste0(Y, " active stream sites measuring ", filterBy)
  ) %>% 
  addLegend("topright", 
             colors=binpal(c(0,1,5,10,15,20)), labels = c("0", "1", "2-5", "6-10", "11-15", ">16"),
             title=paste0("Gages measuring ", filterBy), opacity=1)


#Table of HUC and purpose########################################################################################################################################################################
purp.list <- c("floodmgmt", "ecosysmgmt", "grdwtrmgmt", "pubsafety", "wtrsupply")
filterPurp = "floodmgmt";  #OPTIONS: floodmgmt, ecosysmgmt, grdwtrmgmt, pubsafety, wtrsupply

df.purp <- df %>% select(site_id, sitename, datasource, sitestatus, huc8, all_of(purp.list), geometry) %>% mutate(floodmgmt = ifelse(floodmgmt =="Y", 1, 0), ecosysmgmt = ifelse(ecosysmgmt=="Y",1,0),
                                                                                                                  grdwtrmgmt = ifelse(grdwtrmgmt=="Y",1,0), pubsafety=ifelse(pubsafety=="Y",1,0), wtrsupply = ifelse(wtrsupply=="Y",1,0))
df.purp[is.na(df.purp)] = 0
df.purp <- df.purp %>% mutate(total = floodmgmt+ecosysmgmt+grdwtrmgmt+pubsafety+wtrsupply)
df.purp <- df.purp %>% filter(sitestatus=="Active")

leaflet() %>%
  addProviderTiles("Stamen.TonerLite") %>% 
  addCircleMarkers(data = df.purp,
                   radius = ~df.purp$total*2,
                   #radius = 4,
                   stroke = TRUE, color ="black", weight = 1,
                   fillOpacity= 0.7, fillColor = "gray",
                   label = ~paste0("Site: ", site_id, ", Total purposes: ", df.purp$total)
  )



colorList = c("purple", "green", "goldenrod", "red", "blue")
purp.sel = 5
purpCol = purp.list[purp.sel]

leaflet() %>%
  addProviderTiles("Stamen.TonerLite") %>% 
  addCircleMarkers(data = df.purp %>% filter(get(purpCol)==1),
                   radius = 4,
                   stroke = TRUE, color ="black", weight = 1,
                   fillOpacity= 0.7, fillColor = colorList[purp.sel],
                   label = ~paste0("Site: ", site_id)
  ) %>% 
  addLegend("topright", colors = c(colorList[purp.sel]), labels= c(purp.list[purp.sel]), title = purp.list[purp.sel], opacity = 1)


df.purp2 <- df.purp %>% st_drop_geometry() %>% select(-site_id, -sitename, -datasource) %>% group_by(sitestatus, huc8) %>% gather(key="purpose", value="n", -c(sitestatus, huc8)) %>% 
  ungroup() %>% group_by(sitestatus, huc8, purpose) %>% summarize(n = sum(n, na.rm=TRUE), .groups="drop")
df.purp2 <- df.purp2 %>% spread(purpose, n)
#how many have zero
dim(subset(df.purp2, total==0))
dim(subset(df.purp, total==0))


############################################################################################################################################################################################################
#
#  ONCE SELECT A SITE... WHAT TYPE OF METADATA?
#
############################################################################################################################################################################################################
#list parameters at a site
#https://gispublic.waterboards.ca.gov/portal/home/item.html?id=32dfb85bd2744487affe6e3475190093

site.id <- "09429490"; #usgs example
site.id ="ANH";  #BBD; ANH; #BPB   #cdec example... no stream gauge


df.sel <- df %>% filter(site_id == site.id)
print(paste0(df.sel$sitename, " is ", tolower(df.sel$sitestatus), " and is located on a ", df.sel$streamtype))
#make a table that of water purpose that shows how many days and years of record have been collected
#add row beneath that shows if collecting real type data
df.table <- as.data.frame(matrix(nrow=1, ncol=6));   colnames(df.table) <- c("Site", "Flood", "Ecosystem", "Groundwater", "Public Safety", "Water Supply")
df.table$Site <- df.sel$site_id;# df.table$Flood = df.table$Ecosystem = df.table$Groundwater = df.table$`Public Safety` = df.table$`Water Supply` = "No/Unknown"
#flood management
df.table <- df.table %>% mutate(Flood = ifelse(df.sel$floodmgmt=="Y", "Yes", "No"), Ecosystem = ifelse(df.sel$ecosysmgmt=="Y", "Yes", "No"), Groundwater = ifelse(df.sel$grdwtrmgmt=="Y", "Yes", "No"),
                                `Public Safety` = ifelse(df.sel$pubsafety=="Y", "Yes", "No"), `Water Supply` = ifelse(df.sel$wtrsupply=="Y", "Yes", "No"))
df.table[is.na(df.table)] = "Unknown"
df.table
#-----------------------------------------------------------------------------------------------

sel.huc <- "1802011102"
df.sel.huc <- df %>% filter(huc10 == sel.huc)
df.table <- as.data.frame(matrix(nrow=dim(df.sel.huc)[1], ncol=6));   colnames(df.table) <- c("Site", "Flood", "Ecosystem", "Groundwater", "Public Safety", "Water Supply")
df.table$Site <- df.sel.huc$site_id;
df.table <- df.sel.huc %>% select(site_id, floodmgmt, ecosysmgmt, grdwtrmgmt, pubsafety, wtrsupply)
st_geometry(df.table) <- NULL

df.table[,c(2:6)] <- ifelse(df.table[,c(2:6)]=="Y", "Yes", "No")
df.table[is.na(df.table)] = "Unknown"

df.table <- df.table %>% mutate(Flood = ifelse(is.na(floodmgmt), "No", floodmgmt), Ecosystem = ifelse(is.na(ecosysmgmt), "No", ecosysmgmt), Groundwater = ifelse(is.na(grdwtrmgmt), "No", grdwtrmgmt),
                                Safety = ifelse(is.na(pubsafety), "No", pubsafety), Supply = ifelse(is.na(wtrsupply), "No", wtrsupply)) %>% select(site_id, Flood, Ecosystem, Groundwater, Safety, Supply)
df.table



#what data does this collect?
df.table2 <- as.data.frame(matrix(nrow=4, ncol=5));   colnames(df.table2) <- c("Attribute", "Stage", "Flow", "Water Quality", "Temperature")
df.table2$Attribute <- c("Collected (Yes/No)", "Days of Data", "Years of Data", "Real Time (Yes/No)");

#Stage
if(df.sel$stage_yn == "Y"){
  df.table2$Stage[1] = "Yes"; df.table2$Stage[2] = df.sel$stage_por;   df.table2$Stage[3] = round(as.numeric(df.sel$stage_por)/365.25,0);    df.table2$Stage[4] = ifelse(df.sel$stage_realtime=="Y", "Yes", "No")
} else {
  df.table2$Stage[c(1,4)] = "No"; df.table2$Stage[2:3] = 0;
}

if(df.sel$flow_yn == "Y"){
  df.table2$Flow[1] = "Yes"; df.table2$Flow[2] = df.sel$flow_por;   df.table2$Flow[3] = round(as.numeric(df.sel$flow_por)/365.25,0);    df.table2$Flow[4] = ifelse(df.sel$flow_realtime=="Y", "Yes", "No")
} else {
  df.table2$Flow[c(1,4)] = "No"; df.table2$Flow[2:3] = 0;
}

if(df.sel$watqual_yn == "Y"){
  df.table2$`Water Quality`[1] = "Yes"; df.table2$`Water Quality`[2] = df.sel$watqual_por;   df.table2$`Water Quality`[3] = round(as.numeric(df.sel$watqual_por)/365.25,0);    df.table2$`Water Quality`[4] = ifelse(df.sel$watqual_realtime=="Y", "Yes", "No")
} else {
  df.table2$`Water Quality`[c(1,4)] = "No"; df.table2$`Water Quality`[2:3] = 0;
}

if(df.sel$temp_yn == "Y"){
  df.table2$Temperature[1] = "Yes"; df.table2$Temperature[2] = df.sel$temp_por;   df.table2$Temperature[3] = round(as.numeric(df.sel$temp_por)/365.25,0);    df.table2$Temperature[4] = ifelse(df.sel$temp_realtime=="Y", "Yes", "No")
} else {
  df.table2$Temperature[c(1,4)] = "No"; df.table2$Temperature[2:3] = 0;
}
df.table2



#read in metadata created in explore_gage_metadata.R
metadata <- read.csv("data/cdec_metadata.csv") %>% select(-X)
table(metadata$sensor)



############################################################################################################################################################################################################
#
#  USE NLDI TO LINK UPSTRTEAM AND DOWNSTREAM
#
############################################################################################################################################################################################################
#Examples
#https://geoconnex.internetofwater.dev/demo/
#https://usgs-r.github.io/nhdplusTools/articles/nhdplusTools.html

#what sources are in the NLDI?
dataRetrieval::get_nldi_sources()$source
#> [1] "comid"      "ca_gages"   "gfv11_pois" "huc12pp"    "nwissite"  
#> [6] "ref_gage"   "wade"       "WQP"

#Downstream
#Lets say select site ANH
site.id ="BBD";  #BBD; ANH; #BPB   #cdec example... no stream gauge
sel.site <- df %>% filter(site_id == site.id)

#get geometry
geom_site <- sel.site$geometry[[1]]
nldi_point<-nhdplusTools::discover_nhdplus_id(sel.site)
# nldiURL<-"DM = "https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite/USGS-08279500/navigate/DM?distance=100"

nldi_query <- URLencode(paste0('https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/position?f=json&coords=POINT(',geom_site[1],' ',geom_site[2],')'))
start <- sf::read_sf(nldi_query)
mainstem <- sf::read_sf(paste0(start$navigation,"/DM/flowlines?distance=1000")) #?distance=250"))
mainstem <- mainstem %>% mutate(direction = "mainstem") %>% group_by(direction) %>% summarize()

mapview::mapview(mainstem, col.regions="blue") + mapview::mapview(sel.site, col.regions="red")

#what hucs intersect the rivers... and then what gages?
nldi_feature <- list(featureSource = "comid",  featureID = start$identifier)
cdec_down <- nhdplusTools::navigate_nldi(nldi_feature, "DM", "ca_gages", distance_km = 1000)
mapview::mapview(mainstem, hcl.colors="blue") + mapview::mapview(cdec_down, col.regions="red") + mapview::mapview(sel.site, col.regions="black")

#Now for upstream
nldi_feature <- list(featureSource = "comid", featureID = start_point)
get_nldi_feature(nldi_feature)

flowline_nldi <- navigate_nldi(nldi_feature, 
                               mode = "upstreamTributaries", 
                               distance_km = 1000)
up_main <- navigate_nldi(nldi_feature, 
                         mode = "UM", #upstream_main
                         distance_km = 1000)

#up_main <- nhdplusTools::navigate_nldi(flowline_nldi, "UM", "ca_gages", distance_km = 1000)
flowline_ndldi
mapview::mapview(mainstem, hcl.colors="blue") + mapview::mapview(sel.site, col.regions="black") + mapview::mapview(cdec, col.regions="red") + mapview::mapview(flowline_nldi)# + mapview::mapview(up_main)

#get upstream gages
cdec_up <- nhdplusTools::navigate_nldi(nldi_feature, "UT", "ca_gages", distance_km = 1000)

#mapview::mapview(mainstem, hcl.colors="blue") + mapview::mapview(sel.site, col.regions="black") + mapview::mapview(cdec, col.regions="red") + mapview::mapview(flowline_nldi) + mapview::mapview(cdec_up) # + mapview::mapview(up_main)

#or what if someone selects a HUC?
sel.huc <- "1804000306"
#find center
huc.sf <- read_sf(paste0("https://info.geoconnex.us/collections/hu10/items/",sel.huc, "?f=json"))
start_point <- st_centroid(huc.sf)
mapview::mapview(huc.sf) + mapview::mapview(start_point, col.regions="red")

start_comid <- discover_nhdplus_id(start_point)
#find connected flowlines
huc.flowline.down <- navigate_nldi(list(featureSource = "comid", featureID = start_comid), mode = "DM", distance_km = 1000)

huc.flowline.up <- navigate_nldi(list(featureSource = "comid", featureID = start_comid), mode = "upstreamTributaries", distance_km = 1000)
#huc.flowline.up <- navigate_nldi(list(featureSource = "comid", featureID = huc.flowline.down$nhdplus_comid[1]), mode = "upstreamTributaries", distance_km = 1000)
mapview::mapview(huc.sf) + mapview::mapview(start_point, col.regions="red") + mapview::mapview(huc.flowline.up) + mapview::mapview(huc.flowline.down)

#Now we can intersect the flowlines with the huc6 and then pull any gages or.....
#get upstream gages
geom_site <- start_point$geometry[[1]]
nldi_query <- URLencode(paste0('https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/position?f=json&coords=POINT(',geom_site[1],' ',geom_site[2],')'))
start <- sf::read_sf(nldi_query)

nldi_feature <- list(featureSource = "comid",  featureID = start$identifier)
huc.gages.up <- nhdplusTools::navigate_nldi(nldi_feature, "UT", "ca_gages", distance_km = 200)
huc.gages.down <- nhdplusTools::navigate_nldi(nldi_feature, "DM", "ca_gages", distance_km = 1000)
mapview::mapview(huc.sf) + mapview::mapview(start_point, col.regions="red") + mapview::mapview(huc.gages.up) + mapview::mapview(huc.flowline.down) + mapview::mapview(huc.gages.down, col.regions="black")

all.flowline <- rbind(huc.flowline.down, huc.flowline.up)
huc.int <- st_intersection(all.flowline, ca.huc8)
unique.huc <- unique(huc.temp$id)
huc.int <- ca.huc8 %>% filter(id %in% unique.huc)
mapview::mapview(huc.int) + mapview::mapview(huc.sf, col.regions="red")






