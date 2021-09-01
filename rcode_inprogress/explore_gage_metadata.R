library(rstudioapi); #used to set working directory
library(sf); library(leaflet); #used to work with different types of spatial data
library(rmapshaper); library(geojsonio)
library(dataRetrieval); #used to access data from USGS and importWaterML2
#library(CDECRetrieve);
library(sharpshootR); #ca data retrieval package
library(jsonlite); library(rvest); library(purrr); library(httr); #used with apis, jsons, etc
library(tidyverse); library(lubridate); 
library(plotly);  library(stringr);  #used to work with tables, dates, etc

#for getting data
library(nhdplusTools)


######################################################################################################################################################################
#
#   READ IN CA GAGES DATA
#
######################################################################################################################################################################
swd_data <- "C:\\Users\\lap19\\Documents\\GitHub\\CA-Gages\\Data Layers\\data\\"

#identify duplicate sites
#pcode = "00060"
#ca.usgs <- readNWISdata(stateCd="CA", parameterCd = pcode, service = "site", seriesCatalogOutput=TRUE);
#table(ca.usgs$agency_cd)... all listed as USGS

#pull in linked nldi
g <- sf::read_sf("https://sb19.linked-data.internetofwater.dev/collections/ca_gages/items?limit=4000")
#get site id
g$site_no <- gsub("https://sb19.linked-data.internetofwater.dev/collections/ca_gages/items/","",g$id)

#remove duplicates with USGS
g <- g %>% filter(datasource != "CDEC" | datasource == "CDEC" & operator != "USGS")
table(g$datasource, g$flow_status, useNA="ifany")
table(g$datasource, g$sitestatus, useNA="ifany")
table(g$datasource, g$operator)

nwis <- g %>% filter(datasource=="NWIS") %>% filter(flow_status=="Active" | is.na(flow_status)==TRUE)
cdec <- g %>% filter(datasource != "NWIS") %>% filter(flow_status != "Inactive")
table(cdec$operator)



######################################################################################################################################################################
#
#   METADATA OF INTEREST
#
######################################################################################################################################################################
#Table by data source
table(g$datasource);

#Table of operators and active
df <- g %>% mutate(operator2 = ifelse(substr(operator,1,2)=="CA" | grepl("DWR",operator), "California", operator)) %>% 
  mutate(operator2 = ifelse(grepl("Irrigation", operator) | grepl(" ID ", operator), "Irrigation Districts", operator2)) %>% 
  mutate(operator2 = ifelse(grepl("Pacific Gas", operator) | grepl("Power", operator), "Energy Utilities", operator2)) %>% 
  mutate(operator2 = ifelse(substr(operator,1,3)=="US " | grepl("Reclamation", operator), "Federal", operator2)) %>% 
  mutate(operator2 = ifelse(grepl("Conservation", operator) | grepl("Consv", operator), "Conservation Districts", operator2)) %>% 
  mutate(operator2 = ifelse(grepl("County", operator) | grepl("City", operator), "City or County", operator2)) %>% 
  mutate(operator2 = ifelse(grepl("Utility", operator) | grepl("Authority", operator) | grepl("Water", operator2), "Water Utilities", operator2)) %>% 
  mutate(operator2 = ifelse(operator=="OTHER", "Other", operator2))
table(df$operator2)

#
table(df$operator2, df$sitestatus) %>% as.data.frame() %>% spread(Var2, Freq) %>% rename(Operator = Var1)
table(df$operator2, df$streamtype) %>% as.data.frame() %>% spread(Var2, Freq) %>% rename(Operator = Var1)
#table(df$operator2, df$huc8) %>% as.data.frame() %>% spread(Var2, Freq) %>% rename(Operator = Var1)

#pull all huc8's for plotting------------------------------------------------------------------------------------------------------------------
#unique.huc8 <- unique(df$hu08)
#ca.huc8 <- sf::read_sf(unique.huc8[1])
#for (i in 2:length(unique.huc8)){
#  ca.huc8 <- rbind(ca.huc8, sf::read_sf(unique.huc8[i]))
#}

#pulls most of them so use bounding box by state
ca_feature <- sf::read_sf("https://info.geoconnex.us/collections/states/items?f=json&properties=STATEFP&STATEFP=06")
hu08_url <- paste0("https://info.geoconnex.us/collections/hu08/items?bbox=", paste(sf::st_bbox(ca_feature), collapse = ","), "&limit=200")
hu08 <- sf::read_sf(hu08_url)
mapview::mapview(hu08)

#which intersect the state
ca.huc8 <- st_intersection(ca_feature, hu08) 
ca.huc8 <- hu08 %>% filter(uri %in% ca.huc8$uri.1)
  mapview::mapview(ca.huc8)
######################################----------------------------------------------------------------------------------------------------------

  

#Info by type of data collected ########################################################################################################################################################
#Table of purpose...not stage status has a lot of "NA"... therefore use site status... can add more hucs or other variables
df.stage <- st_drop_geometry(df) %>% group_by(sitestatus, huc8, stage_yn) %>% count() %>% mutate(stage_yn = paste0("stage_",stage_yn)) %>% rename(variable = stage_yn) # %>% spread(stage_yn, n)
df.flow <- st_drop_geometry(df) %>% group_by(sitestatus, huc8, flow_yn) %>% count() %>% mutate(flow_yn = paste0("flow_",flow_yn)) %>% rename(variable = flow_yn) #%>% spread(flow_yn, n)
df.wq <- st_drop_geometry(df) %>% group_by(sitestatus, huc8, watqual_yn) %>% count() %>% mutate(watqual_yn = paste0("watqual_",watqual_yn)) %>% rename(variable = watqual_yn)
df.temp <- st_drop_geometry(df) %>% group_by(sitestatus, huc8, temp_yn) %>% count() %>% mutate(temp_yn = paste0("temp_",temp_yn)) %>% rename(variable = temp_yn)

df.data <- rbind(df.stage, df.flow, df.wq, df.temp) #%>% spread(variable, n)
df.data[is.na(df.data)]<-0;


filterBy = "temp"; #options: stage, flow, watqual, temp
df.huc8 <- df.data %>% filter(sitestatus=="Active") %>% filter(grepl(filterBy, variable)) %>% mutate(variable = substr(variable, nchar(variable), nchar(variable))) %>% spread(variable, n)
#only plotting active sites here that monitor stages
huc8.stage <- merge(ca.huc8, df.huc8 %>% filter(sitestatus=="Active"), by.x="id", by.y="huc8", all.x=TRUE)
huc8.stage[is.na(huc8.stage)]<-0
#pal <- colorNumeric(c("white", "navy"), 0:60)
#binpal <- colorBin("Blues", huc8.stage[paste0(filterBy,"_Y")], c(0,1,5,10,25,50,100), pretty = FALSE)
if(filterBy == "stage" | filterBy == "flow"){
  binpal <- colorBin("Blues", huc8.stage$Y, c(0,1,5,10,25,50,100), pretty = FALSE)
}

if(filterBy == "watqual"){
  binpal <- colorBin("Purples", huc8.stage$Y, c(0,1,5,10,25,50,100), pretty = FALSE)
}

if(filterBy == "temp"){
  binpal <- colorBin("Reds", huc8.stage$Y, c(0,1,5,10,25,50,100), pretty = FALSE)
}

leaflet() %>% addProviderTiles("Stamen.TonerLite") %>% 
  addPolygons(data = huc8.stage,
              #fillOpacity= 0.7, fillColor = ~pal(Y),
              fillOpacity= 0.8, fillColor = ~binpal(Y),
              stroke=FALSE,
              label = ~paste0(Y, " active stream sites measuring ", filterBy)
  )




#Table of HUC and purpose########################################################################################################################################################################
purp.list <- c("floodmgmt", "ecosysmgmt", "grdwtrmgmt", "pubsafety", "wtrsupply")
filterPurp = "floodmgmt";  #OPTIONS: floodmgmt, ecosysmgmt, grdwtrmgmt, pubsafety, wtrsupply

df.purp <- df %>% select(site_no, sitename, datasource, sitestatus, huc8, all_of(purp.list), geometry) %>% mutate(floodmgmt = ifelse(floodmgmt =="Y", 1, 0), ecosysmgmt = ifelse(ecosysmgmt=="Y",1,0),
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
                   label = ~paste0("Site: ", site_no, ", Total purposes: ", df.purp$total)
  ) %>% 
  addLegend("topright", 
            colors = c("white"), #c("darkred","red","orange","green","aquamarine","blue","navy"),
            labels= c(""), #c("Min Flow", "1-10%", "11-25%", "26-74%", "75-89%", "90-99%", "Max Flow"),
            title = "Total Purposes",
            opacity = 1)



colorList = c("purple", "green", "goldenrod", "red", "blue")
purp.sel = 5
purpCol = purp.list[purp.sel]

leaflet() %>%
  addProviderTiles("Stamen.TonerLite") %>% 
  addCircleMarkers(data = df.purp %>% filter(get(purpCol)==1),
                   radius = 4,
                   stroke = TRUE, color ="black", weight = 1,
                   fillOpacity= 0.7, fillColor = colorList[purp.sel],
                   label = ~paste0("Site: ", site_no)
  ) %>% 
  addLegend("topright", colors = c(colorList[purp.sel]), labels= c(purp.list[purp.sel]), title = purp.list[purp.sel], opacity = 1)


df.purp2 <- df.purp %>% st_drop_geometry() %>% select(-site_no, -sitename, -datasource) %>% group_by(sitestatus, huc8) %>% gather(key="purpose", value="n", -c(sitestatus, huc8)) %>% 
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


df.sel <- df %>% filter(site_no == site.id)
print(paste0(df.sel$sitename, " is ", tolower(df.sel$sitestatus), " and is located on a ", df.sel$streamtype))
#make a table that of water purpose that shows how many days and years of record have been collected
#add row beneath that shows if collecting real type data
df.table <- as.data.frame(matrix(nrow=1, ncol=6));   colnames(df.table) <- c("Site", "Flood", "Ecosystem", "Groundwater", "Public Safety", "Water Supply")
df.table$Site <- df.sel$site_no;# df.table$Flood = df.table$Ecosystem = df.table$Groundwater = df.table$`Public Safety` = df.table$`Water Supply` = "No/Unknown"
#flood management
df.table <- df.table %>% mutate(Flood = ifelse(df.sel$floodmgmt=="Y", "Yes", "No"), Ecosystem = ifelse(df.sel$ecosysmgmt=="Y", "Yes", "No"), Groundwater = ifelse(df.sel$grdwtrmgmt=="Y", "Yes", "No"),
                                `Public Safety` = ifelse(df.sel$pubsafety=="Y", "Yes", "No"), `Water Supply` = ifelse(df.sel$wtrsupply=="Y", "Yes", "No"))
df.table[is.na(df.table)] = "Unknown"
df.table

#what data does this collect?
df.table2 <- as.data.frame(matrix(nrow=4, ncol=5));   colnames(df.table2) <- c("Attribute", "Stage", "Flow", "Water Quality", "Temperature")
df.table2$Attribute <- c("Collected (Yes/No)", "Days of Data", "Years of Data", "Real Time (Yes/No)");

#Stage
if(df.sel$stage_yn == "Y"){
  df.table2$Stage[1] = "Yes"; df.table2$Stage[2] = df.sel$stage_por;   df.table2$Stage[3] = round(df.sel$stage_por/365.25,0);    df.table2$Stage[4] = ifelse(df.sel$stage_realtime=="Y", "Yes", "No")
} else {
  df.table2$Stage[c(1,4)] = "No"; df.table2$Stage[2:3] = 0;
}

if(df.sel$flow_yn == "Y"){
  df.table2$Flow[1] = "Yes"; df.table2$Flow[2] = df.sel$flow_por;   df.table2$Flow[3] = round(df.sel$flow_por/365.25,0);    df.table2$Flow[4] = ifelse(df.sel$flow_realtime=="Y", "Yes", "No")
} else {
  df.table2$Flow[c(1,4)] = "No"; df.table2$Flow[2:3] = 0;
}

if(df.sel$watqual_yn == "Y"){
  df.table2$`Water Quality`[1] = "Yes"; df.table2$`Water Quality`[2] = df.sel$watqual_por;   df.table2$`Water Quality`[3] = round(df.sel$watqual_por/365.25,0);    df.table2$`Water Quality`[4] = ifelse(df.sel$watqual_realtime=="Y", "Yes", "No")
} else {
  df.table2$`Water Quality`[c(1,4)] = "No"; df.table2$`Water Quality`[2:3] = 0;
}

if(df.sel$temp_yn == "Y"){
  df.table2$Temperature[1] = "Yes"; df.table2$Temperature[2] = df.sel$temp_por;   df.table2$Temperature[3] = round(df.sel$temp_por/365.25,0);    df.table2$Temperature[4] = ifelse(df.sel$temp_realtime=="Y", "Yes", "No")
} else {
  df.table2$Temperature[c(1,4)] = "No"; df.table2$Temperature[2:3] = 0;
}
df.table2



#pull data and try to recreate State climate office plot by variable
#metadata: https://gispublic.waterboards.ca.gov/portal/home/item.html?id=32dfb85bd2744487affe6e3475190093
endDate = as.Date(Sys.time(), format ="Y-m-d")
base_url <- "https://cdec.water.ca.gov/dynamicapp/req/JSONDataServlet?Stations="

#Might use this for metadata instead...
CDEC_StationInfo(site.id)#Has to be length of string 1
cdec_stations <- df %>% filter(operator2 != "USGS")
unique.cdec <- unique(cdec_stations$id)
cdec_metadata <- as.data.frame(matrix(nrow=0, ncol=8));

for (i in 1:length(unique.cdec)){
  zt <- CDEC_StationInfo(unique.cdec[i])
  
  if (is.null(zt)){
    zt.sensor <- as.data.frame(matrix(nrow=1, ncol=8));
    colnames(zt.sensor) <- colnames(cdec_metadata)
    zt.sensor$id = unique.cdec[i]
  }
  
  if (is.null(zt)==FALSE){
    zt.site <- zt$site.meta
      colnames(zt.site) <- c("id", "river_basin", "hydrologic_area", "lat", "operator", "elevation", "county", "nearby_city", "longitute", "maintenance", "name")
      zt.site <- zt.site %>% select(id, name, river_basin, hydrologic_area, county, nearby_city)
    
      
    if(zt$sensor.meta$sensor_details  %>% str_detect("'")==FALSE){  
      zt.sensor <- zt$sensor.meta
    }
      
    if(zt$sensor.meta$sensor_details  %>% str_detect("'")==TRUE){
      zt.sensor <- zt$comments
      colnames(zt.sensor)<- colnames(zt$sensor.meta[1:6])
    }
      
      zt.sensor <- zt.sensor %>% mutate(unit = sub('.*\\,', '', sensor_details), dur_code = gsub("[()]", "", interval), sensor = gsub("[()]", "", sensor_name), 
                                    start_date = as.Date(substr(period_of_record, 1, 10), format="%m/%d/%Y"), end_date = as.Date(substr(period_of_record, 15,25), format="%m/%d/%Y")) %>% 
                mutate(id = zt.site$id, name = zt.site$name) %>% 
                dplyr::select(id, name, sensor, unit, dur_code, collection_method, start_date, end_date)
  }#end if zt exists
  
  #rbind data
  cdec_metadata = rbind(cdec_metadata, zt.sensor)
  print(i)  
}
#if NA set to present
bk.up <- cdec_metadata

#fix errors
CDEC_StationInfo("SR3") #bad start date
CDEC_StationInfo("TUR") #bad end date
CDEC_StationInfo("GVO") #bad end date
CDEC_StationInfo("CVL") #bad data

#cdec_metadata 
#using if_else keeps date in format
cdec_metadata <- cdec_metadata %>% mutate(end_date = if_else(is.na(end_date), as.Date(Sys.time(), format="%Y-%m-%d"), as.Date(end_date, format="%Y-%m-%d")), 
    start_date = if_else(start_date=="0016-02-11", as.Date("2016-02-11", format="%m/%d/%Y"), start_date), end_date = if_else(end_date=="0110-07-12", as.Date("2004-07-12", format="%Y-%m-%d"), end_date),
    end_date = if_else(end_date=="1818-07-20", as.Date("2018-07-20", format="%Y-%m-%d"), end_date)) %>% mutate(unit = trimws(unit, "both"))

table(zt2$dur_code)
table(zt2$unit)
table(zt2$sensor)
table(zt2$collection_method)

write.csv(cdec_metadata, "C:\\Users\\lap19\\Documents\\Internet of Water\\Pilots\\CA\\data\\cdec_metadata.csv")


#will need to do for usgs
parm_cd_file = parameterCdFile
site.id <- "09429490"; #usgs example
data_usgs <- whatNWISdata(siteNumber = site.id) %>% filter(count_nu>100 & end_date > 2010-01-01 & medium_grp_cd == "wat" & is.na(parm_cd)==FALSE)
data_usgs <- merge(data_usgs, parm_cd_file[,c("parameter_cd","srsname", "parameter_units")], by.x="parm_cd", by.y="parameter_cd", all.x=TRUE)



dur_code_url <- "&dur_code=D"
date_url <- paste0("&Start=", "2020-01-01", "&End=", endDate)
full_url = paste0(base_url, site.id, date_url) #, dur_code_url)

api.data <- GET(full_url)
df.site <- jsonlite::fromJSON(content(api.data, 'text'), simplifyVector = TRUE, flatten=TRUE)
df.site <- df.site %>% mutate(value = ifelse(value == -9999, NA, value))

#x <- jsonlite::fromJSON("https://cdec.water.ca.gov/dynamicapp/req/JSONDataServlet?Stations=ANH&Start=2021-05-01&End=2021-06-01")
#x <- CDECquery(id="ANH", sensor="1", interval = "E", start="2021-03-30", end="2021-05-30")

#They have different formats based on durCode
#durCode D = daily data
#durCode E = event data... the data will need to be summarized into a daily value. It provides the stationId, durCode, SENSOR_NUM, sensorType, date, obsDate, value, dataFlag, and units
#durCode 






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
sel.site <- df %>% filter(id == site.id)

#get geometry
geom_site <- sel.site$geometry[[1]]
nldi_point<-nhdplusTools::discover_nhdplus_id(sel.site)
# nldiURL<-"DM = "https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite/USGS-08279500/navigate/DM?distance=100"

nldi_query <- URLencode(paste0('https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/position?f=json&coords=POINT(',geom_site[1],' ',geom_site[2],')'))
start <- sf::read_sf(nldi_query)
mainstem <- sf::read_sf(paste0(start$navigation,"/DM/flowlines?distance=1000")) #?distance=250"))

mapview::mapview(mainstem, hcl.colors="blue") + mapview::mapview(sel.site, col.regions="red")

#what hucs intersect the rivers... and then what gages?
nldi_feature <- list(featureSource = "comid",  featureID = start$identifier)
cdec_down <- nhdplusTools::navigate_nldi(nldi_feature, "DM", "ca_gages", distance_km = 1000)
mapview::mapview(mainstem, hcl.colors="blue") + mapview::mapview(sel.site, col.regions="black") + mapview::mapview(cdec, col.regions="red")

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


