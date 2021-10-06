//##############################################################################################
//                   HOVER, CLICK FUNCTIONS ON MAP
//                   Function to interact with map hovers and clicks
//##############################################################################################

/*-------------------------------------------------------------------------------------------------------
  ////////////    CREATE HOVER OVER MAP FUNCTIONS                                             ///////////
--------------------------------------------------------------------------------------------------------*/
// HOVER OVER HUC 12
map.on("mousemove", "huc12", function (e) {
    map.getCanvas().style.cursor = "pointer";
    //check if feature exist
    if (e.features.length > 0) {
        document.getElementById("map_hover_box").innerHTML =
            "<p><strong>HUC 12 Information<br><br></strong>" +
             e.features[0].properties.name + " (" +  e.features[0].properties.huc12 +")<br>"+
             "Headwaters Well Gaged: " + Math.round(e.features[0].properties.headwater_well) + "%<br>" + "Streams Well Gaged: " +
             e.features[0].properties.stream_well + "%<br>" + "Rivers Well Gaged: " +
             e.features[0].properties.river_well + 
            "%</p>";
   } // end if hover over map
}); //end map.on

map.on("mouseleave", "huc12", function () {
    document.getElementById("map_hover_box").innerHTML =
        "<p><strong>Hover over a huc12<strong></p>";
    map.getCanvas().style.cursor = ""; //resent point
});


// STREAM GAUGE HOVER ----------#############################################################################
map.on("mousemove", "streamgauge-layer", function (e) {
    // Change the cursor style as a UI indicator.
    map.getCanvas().style.cursor = "pointer";
    var coordinates = e.features[0].geometry.coordinates.slice();
    var description =
        e.features[0].properties.site_id +": " + e.features[0].properties.sitename  + ")";

    // Ensure that if the map is zoomed out such that multiple copies of the feature are visible, the popup appears over the copy being pointed to.
    while (Math.abs(e.lngLat.lng - coordinates[0]) > 180) {
        coordinates[0] += e.lngLat.lng > coordinates[0] ? 360 : -360;
    }
    // Populate the popup and set its coordinates based on the feature found.
    popup.setLngLat(coordinates).setHTML(description).addTo(map);
}); //end map on for stream gauges

map.on("mouseleave", "streamgauge-layer", function () {
    map.getCanvas().style.cursor = "";
    popup.remove();
});




//########################################################################################################
/*-------------------------------------------------------------------------------------------------------
  ////////////    CREATE CLICK ON MAP FUNCTIONS                                             ///////////
--------------------------------------------------------------------------------------------------------*/
//########################################################################################################
// // set up if utilities-layer is clicked
// map.on("click", "utilities-layer", function (e) {
//     //since multiple overlapping layers, need to set up so that utiliteis don't get clicked when clicking on point
//     var f = map.queryRenderedFeatures(e.point, {
//         layers: [
//             "utilities-layer",
//             "streamgauge-layer",
//             "reservoirs",
//             "groundwater",
//             "precipitation",
//         ],
//     });
//     //console.log(f);   console.log(f.length);
//     if (f.length > 1) {
//         return;
//     }

//     myUtilityID = e.features[0].properties.ncpwsid;
//     myUtility = e.features[0].properties.utility_name;
//     //console.log(utilityID + ": " + myUtility);
//     //set dropdown list to whatever is selected
//     document.getElementById('setSystem').value = myUtilityID;

//     //filter water supply watersheds?? Not sure how to do
//     map.setFilter("water_supply", ["in", "drawFile", myUtilityID]);
//     map.setFilter("water_supply_name", ["in", "drawFile", myUtilityID]);

//     //run functions
//     myUtilityInfo(myUtility);
//     createCurrentSummary(myUtility);
//     createDemandInfo(myUtilityID, checkedDemand);
//     return myUtilityID;
// });

// map.on("click", "streamgauge-layer", function (e) {
//     document.getElementById("switchStatsDiv").style.display = "block";

//     var streamGaugeName = e.features[0].properties.name;
//     streamID = e.features[0].properties.site;
//     recentDate = e.features[0].properties.julian;
//     //var urlLink = e.features[0].properties.url_link; //console.log(e.features);
//     var urlLink =
//         "https://waterdata.usgs.gov/monitoring-location/" +
//         streamID +
//         "/#parameterCode=00060";

//     //highlight in map
//     if (typeof map.getLayer("streamgauge-selected") !== "undefined") {
//         map.removeLayer("streamgauge-selected");
//     }
//     map.addLayer({
//         id: "streamgauge-selected",
//         type: "circle",
//         source: "streamgauges",
//         filter: ["in", "site", streamID],
//         paint: {
//             "circle-radius": 20,
//             "circle-color": "yellow",
//             "circle-opacity": 0.5,
//         }, //end paint
//     }); //end add layer

//     document.getElementById("selectDataName").innerHTML =
//         "<h4>" +
//         streamID +
//         ": <a href=" +
//         urlLink +
//         " target='_blank'>" +
//         streamGaugeName +
//         "</a><h4>";
//     fileName = "data/streamflow/stream_stats.csv";

//     createDailyStatistics(streamID, streamPlotType);
// });
