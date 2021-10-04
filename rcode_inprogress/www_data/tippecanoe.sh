tippecanoe -z8 -o huc12.mbtiles --read-parallel --generate-ids --force -L huc12:"huc12.geojson" 
tippecanoe -z8 -o gap_flowlines.mbtiles --read-parallel --generate-ids --force -L gap_flowlines:"gap_flowlines.geojson" 

#at -z8 it is pretty good - but if zoom in a lot you can see gaps. 