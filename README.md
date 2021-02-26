# CA-Gages
This is a stub, Email content below:



Dan Schultz and team are:
-	A. Creating a map and associated documentation that includes metadata on gage provenance, location, parameter, and period of record, mostly for gages within USGS NWIS or CDEC, but may include others
-	B. Writing a (python? can probably do R as well) script (or using existing one) to identify gage network gaps
-	C. Identifying geospatial layers and developing procedures (process or script TBD) to use a combination of the layers as an input (A.) and output a table of basins (HUC8?, 12? NHD Catchment? Basin scale TBD) with prioritization indicators by management category (ecosystem, water supply, water quality, flood).



Lauren Patterson will:
-	D. Create a data visualization that allows users to
o	View, filter, and sort gage locations, streams, and basins by prioritization indicators created  by (C.)
o	POTENTIALLY – suggest locations for new gages (along with parameters?), triggering a re-run of (B.) and changing the visualization accordingly
	Note: Both Lauren and Kyle are most proficient as R developers for data processing tasks. If the script (B.) will be in python, the most reasonable way for this to work would be if it underpinned an API that a geojson file could be passed to.
•	Alternatively, Lauren and Kyle could maintain and document an R version, and package the data viz with an R-based API that does the above. 

Kyle Onda will:
*	Coordinate with Dan and Matt Correa to explore options for creating a neutral platform that hosts up-to-date versions of gage metadata from all public sources in human- and machine-readable formats.
--	Eg. Imagine pages like this  that could include links to the relevant CDEC, WDL, and any other websites that might hold versions of data coming from each gage. These pages would have embedded geojson forms, so it can serve as the up-to-date backend of (D.) or any other tool relying on gage location metadata. The links could also include pre-canned API calls to allow users to retrieve data directly from relevant USGS, CDEC, or WDL services. See here for a very coarse proof-of concept example which is automatically updated based on the data system in the previous two links.

*	What is the source of up-to-dateness? Should it be based solely on (A.), with the human-curated metadata, or should there be live connections to USGS and CDEC? Live connections if possible
-- What about CA DWR Water Data Library? 
--	What about other local/ private gages as they become available? We should have a batch of these (approx. 180) that could be used as test sites soon (I believe that the telemetered data is pushed to a variety of different website platforms)



### Sources of data (live preferred)

#### CA DWR Continuous monitoring stations
https://gis.water.ca.gov/arcgis/rest/services/Geoscientific/i08_Stations_Monitoring_Continuous_Hydstra_Period/MapServer/0/query?where=objectid+%3D+objectid&outfields=*&orderByFields=OBJECTID+ASC&f=geojson

#### CA DWR Contrinuous monitoring station tables
https://gis.water.ca.gov/arcgis/rest/services/Geoscientific/i08_Stations_Monitoring_Continuous_Hydstra_Period/MapServer/1/query?where=objectid+%3D+objectid&outfields=*&orderByFields=OBJECTID+ASC&f=json

#### CDEC Stations
https://services.gis.ca.gov/arcgis/rest/services/AtmosphereClimate/CDEC_Stations/MapServer/1/query?where=objectid+%3D+objectid&outfields=*&orderByFields=OBJECTID+ASC&f=geojson



#### USGS (all NWIS sites)
https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite?f=json


## Other?



