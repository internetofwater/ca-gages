library(sf)
library(esri2sf)

g <- esri2sf("https://gispublic.waterboards.ca.gov/portalserver/rest/services/Hosted/StreamGages_20210114/FeatureServer/0")
st_write(g,"../data/ca_gages.geojson")