tippecanoe -z10 -o ca_huc12.mbtiles --read-parallel --generate-ids --force -L ca_huc12:"ca_huc12_full.geojson" 
tippecanoe -z9 -o ca_hucs.mbtiles --read-parallel --generate-ids --force -L ca_huc6:"ca_huc6.geojson" -L ca_huc8:"ca_huc8"
tippecanoe -z10 -o ca_flowlines.mbtiles --read-parallel --generate-ids --force -L ca_flowlines:"all_flowlines.geojson" 