library(mapview)
library(urltools)
library(nhdplusTools)
library(sf)

#<script src="https://gist.github.com/ksonda/4f2d1b8384a0688625051dee26f457bd.js"></script>


convert <- function(x) {
  for(n in names(x)) {
    if(is.character(x[[n]])) {
      test <- grepl("^http", x[[n]])
      x[[n]][test] <- paste0('<a href="', 
                             x[[n]][test], 
                             '" target="_blank">', 
                             x[[n]][test], 
                             '</a>')
    }
  }
  x
}

nldi_mapview <- function(x,y, endpoint="ca_gages", method=c("DM","UM","UT","DD")){
  point <- sf::st_sfc(st_point(c(x,y)),crs=4326)
  nldi_point<-nhdplusTools::discover_nhdplus_id(point)
  
  url_features <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",
                         nldi_point,
                         "/navigation/",
                         method,
                         "/",
                         endpoint,
                         "?f=json&distance=6000")
  
  url_flowlines<- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",
                         nldi_point,
                         "/navigation/",
                         method,
                         "/flowlines?f=json&distance=6000")
  
  features <- sf::read_sf(url_features)
  features$provider <- urltools::domain(features$uri)
  features <- convert(features)
  flowlines <- sf::st_as_sf(sf::st_geometry(sf::read_sf(url_flowlines)))
  x<-mapview::mapview(features,zcol="provider") + mapview::mapview(flowlines)
  return(x)
}

x <- -119.158
y <- 36.140227
nldi_mapview(x=x,y=y,method="UT",endpoint = "ref_gage")
