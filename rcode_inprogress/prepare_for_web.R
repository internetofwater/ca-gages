###############################################################################################################################3###########
#
#     READ IN FULL DATASETS AND PUT IN SIMPLEST FORMAT TO RENDER QUICKLY
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


#LOAD IN HUCS and CLIP TO THOSE INTERSECTING THE STATE RATHER THAN FULL EXTENSION
#SIMPLIFY HUCS TO 65%
ca_feature <- sf::read_sf("https://info.geoconnex.us/collections/states/items?f=json&properties=STATEFP&STATEFP=06")
huc6 <- sf::read_sf("data/ca_huc6.geojson") %>% ms_simplify(keep=0.65, keep_shapes=TRUE)
huc6 <- huc6 %>% select(id, uri, NAME, geometry)
geojson_write(huc6, file = "www_data/huc6.geojson") 

#huc8
huc8 <- sf::read_sf("data/ca_huc8.geojson") %>% ms_simplify(keep=0.65, keep_shapes=TRUE)
temp <- st_join(ca_feature, huc8) 
huc8 <- huc8 %>% filter(uri %in% temp$uri.y)
huc8 <- huc8 %>% select(id, uri, NAME, geometry)
geojson_write(huc8, file = "www_data/huc8.geojson") 
rm(temp)


#huc10
huc10 <- sf::read_sf("results/huc10_data.geojson") %>% ms_simplify(keep=0.65, keep_shapes=TRUE)
huc10 <- huc10 %>% filter(substr(huc10,1,8) %in% huc8$id)
geojson_write(huc10, file = "www_data/huc10.geojson") 

#huc12
huc12 <- sf::read_sf("results/huc12_data.geojson") 
huc12 <- huc12 %>% filter(substr(huc12,1,8) %in% huc8$id) %>% ms_simplify(keep=0.45, keep_shapes=TRUE)
huc12 <- huc12 %>% mutate(cond_index = round(as.numeric(as.character(cond_index)),2), threat_index = round(as.numeric(as.character(threat_index)),2),
                          headwater_well = round(as.numeric(as.character(headwater_well)),2), river_well = round(as.numeric(as.character(river_well)),2),
                          stream_well = round(as.numeric(as.character(stream_well)),2), headwater_some = round(as.numeric(as.character(headwater_some)),2),
                          river_some = round(as.numeric(as.character(river_some)),2), stream_some = round(as.numeric(as.character(stream_some)),2),
                          headwater_poor = round(as.numeric(as.character(headwater_poor)),2), river_poor = round(as.numeric(as.character(river_poor)),2),
                          stream_poor = round(as.numeric(as.character(stream_poor)),2)
                          )
geojson_write(huc12, file = "www_data/huc12.geojson") 

#rivers
#rivers <- sf::read_sf("data/main_flowlines.geojson") %>% ms_simplify(keep=0.5, keep_shapes = TRUE)
#geojson_write(rivers, file = "data/www_data/main_flowlines.geojson") 

#waterbodies 
lakes <- sf::read_sf("data/water_bodies.geojson") %>% ms_simplify(keep=0.25, keep_shapes=TRUE)
geojson_write(lakes, file = "www_data/lakes.geojson")

#gages
g <- sf::read_sf("results/stream_gages.geojson")
geojson_write(g, file = "www_data/stream_gages.geojson") 

#gage_gap
gap <- sf::read_sf("data/gage_gap.geojson")
gap <- gap %>% ms_simplify(keep=0.45, keep_shapes = TRUE)
gap <- gap %>% st_make_valid()

#some are missing geometries - remove to avoid errors later
gap1 <- gap %>% filter(st_geometry_type(gap) != "GEOMETRYCOLLECTION") 

#merge together to make smaller and rename
gap1 <- gap1 %>% group_by(huc12, SU_Name, StrmOrder, GGIIStatus, Gaged) %>% summarize(n_segments = n(), .groups="drop")
  as.data.frame(table(st_geometry_type(gap1)))
  
gap2 <- gap1 %>% filter(GGIIStatus == "Well Gaged")
#gap1 <- gap %>% filter(StrmOrder > 3)
mapview::mapview(gap2)                 

geojson_write(gap1, file = "www_data/gap_flowlines.geojson") 



library(mapboxer)
basemaps
Sys.setenv(MAPBOX_API_TOKEN = "pk.eyJ1IjoiaW50ZXJuZXRvZndhdGVyIiwiYSI6ImNrdHZuZXRrazJjMnMycHAzdW91bnAyNjgifQ.KeB-tlVDJgW7H_NGdCghNg")

map <- mapboxer(
    #style = basemaps$Mapbox$streets_v11,
    style = basemaps$Mapbox$light_v10,
    center = c(-119, 37.5),
    zoom = 5,
    minZoom = 3
  ) %>% 
  add_navigation_control(
    pos = "top-left",
    # Option passed to the 'NavigationControl'
    showCompass = FALSE
  ) #%>%
  #add_text_control(
  #  pos = "top-right",
  #  text = "mapboxer"
  #)

#load in map from tilset

huc12_source <- as_mapbox_source(url= 'mapbox://internetofwater.d71se1mk');

#create a source
huc12_source <- huc12 %>% as_mapbox_source()
map  %>% 
  add_fill_layer(
    source = huc12_source,
    #fill_color = c("get", "fillColor"),
    fill_color = "darkgray",
    fill_opacity = 0.6,
    popup = "HUC12: {{name}} ") #,
    #filter = list(">", c("get", "river_well"), 80))

leaflet() %>%
  #load tiles
  addProviderTiles("Stamen.TonerLite", group="Toner by Stamen") %>% 
  
  #add mapPanes to render layers in specific order
  addMapPane("points", zIndex = 400) %>% 
  addMapPane("lines", zIndex = 300) %>% 
  addMapPane("polygons", zIndex = 200) %>% 
  
  #load layers
  addGeoJSON(huc12, group = "HUC 12",
              color ="black", weight = 2,
              fillOpacity= 0.5, fillColor = "gray", options = pathOptions(pane = "polygons") 
  )




