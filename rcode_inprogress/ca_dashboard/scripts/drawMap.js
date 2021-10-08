//##############################################################################################
//                             DRAW MAP
//                   created by Lauren Patterson
//##############################################################################################
function drawMap(){
  //###########################################################################
  //                            INITIAL MAP LOAD
  //##############################################################################################
      //have to add sources on load---------------------------------------------
      map.on('load', function () {
        //Add tiles holding boundary layers
        map.addSource('boundary-layers', {
          type: 'vector',
          url: 'mapbox://internetofwater.60by4eh2'
        });

        // Add vector tile source for huc12
        map.addSource('huc12-layer', {
          type: 'vector',
          url: 'mapbox://internetofwater.d71se1mk'
        });

        //Add vector tile source for streams
        map.addSource('gap-layer', {
          type: 'vector',
          url: 'mapbox://internetofwater.07nyozq7'
        });

        //Add vector tiles for gages
        map.addSource('gage-layer', {
          type: 'vector',
          url: 'mapbox://internetofwater.3976lo15'
        });


        // Add layers from vector source -------------------------------------------
        map.addLayer({
          'id': 'huc12',
          'type': 'fill',
          'source': 'huc12-layer',
          'source-layer': 'huc12',
          //'minzoom': zoomThreshold,
          'paint': {
            'fill-color': 'rgba(0,0,0,0)',
            'fill-outline-color': 'black',
            'fill-opacity': 1,
          }, //end paint
          'layout': {'visibility': 'visible'},
        }); // end huc12 map layer

        
      

  //#########################################################################################
  //         ADD BOUNDARIES DATA
  //#########################################################################################
    //ADD LAYERS TO TURN ON AND OFF - SET TOGGLES OUTSIDE OF MAP ON TOP IF CAN
    // ORDER MATTERS... THE FIRST ONE GOES UNDER OTHERS

    //COUNTY + LABELS----------------------------------------------
    map.addLayer({
      'id': 'county',
      'type': 'line',
      'source': 'boundary-layers',
      'source-layer': 'county',
      'layout': { 'visibility': 'none' }, //how you turn off visibility on load
      'paint': {'line-color': 'darkgray', 'line-width': 4}
    });
   map.addLayer({
    id: "county_name",
    type: "symbol",
    source: 'boundary-layers',
    'source-layer': 'county_pts',
    layout: {
        "visibility": 'none',
        "text-field": "{NAME}",
        'symbol-placement': "point"
    },
    paint: {
        "text-color": "black",
        "text-halo-color": "#fff",
        "text-halo-width": 4,
        "text-halo-blur": 0,
    }
    });

    //COUNTY + LABELS----------------------------------------------
    map.addLayer({
      'id': 'urban',
      'type': 'fill',
      'source': 'boundary-layers',
      'source-layer': 'urban',
      'layout': { 'visibility': 'none' }, //how you turn off visibility on load
      'paint': {
        'fill-color': 'black',
        'fill-opacity': 0.5,
      }
    });
   map.addLayer({
    id: "urban_name",
    type: "symbol",
    source: 'boundary-layers',
    'source-layer': 'urban_pts',
    layout: {
        "visibility": 'none',
        "text-field": "{NAME10}",
        'symbol-placement': "point"
    },
    paint: {
        "text-color": "black",
        "text-halo-color": "#fff",
        "text-halo-width": 4,
        "text-halo-blur": 0,
    }
    });

    //RIVER BASINS: HUC6 + LABELS----------------------------------------------
    map.addLayer({
      'id': 'huc6',
      'type': 'line',
      'source': 'boundary-layers',
      'source-layer': 'huc6',
      'layout': {'visibility': 'none', },
      'paint': {'line-color': 'navy', 'line-width': 4}
    });
   map.addLayer({
    id: "huc6_name",
    type: "symbol",
    source: 'boundary-layers',
    'source-layer': 'huc6_pts',
    layout: {
        "visibility": 'none',
        "text-field": "{NAME}",
        'symbol-placement': "point"
    },
    paint: {
        "text-color": "navy",
        "text-halo-color": "#fff",
        "text-halo-width": 4,
        "text-halo-blur": 0,
    }
    });

    //WATERSHEDS: HUC8 + LABELS----------------------------------------------
    map.addLayer({
      'id': 'huc8',
      'type': 'line',
      'source': 'boundary-layers',
      'source-layer': 'huc8',
      'layout': { 'visibility': 'none',  },
      'paint': {'line-color': 'rgb(0,0,200)', 'line-width': 2}
    });
   map.addLayer({
    id: "huc8_name",
    type: "symbol",
    'source': 'boundary-layers',
    'source-layer': 'huc8',
    layout: {
        "visibility": 'none',
        "text-field": "{NAME}",
        'symbol-placement': "point"
    },
    paint: {
        "text-color": "rgb(0,0,200)",
        "text-halo-color": "#fff",
        "text-halo-width": 2,    "text-halo-blur": 0,
    }
  });

    //MAJOR RIVERS + LABELS----------------------------------------------
     // Add layers from vector source
     map.addLayer({
      'id': 'lakes',
      'type': 'fill',
      'source': 'boundary-layers',
      'source-layer': 'lakes',
      'paint': {
        'fill-color': 'blue',
        'fill-outline-color': 'blue',
        'fill-opacity': 0.8,
      }, //end paint
      'layout': {'visibility': 'none'}
    }); // end lakes map layer

    // Add gage lines layers from vector source
     map.addLayer({
      'id': 'gap_flowlines',
      'type': 'line',
      'source': 'gap-layer',
      'source-layer': 'gap_flowlines',
      //'minzoom': zoomThreshold,
      'paint': {
         'line-color': [
           'match',
           ['get', 'GGIIStatus'],
           'Poorly Gaged', '#6a0000',
           'Well Gaged', '#00006a',
           'Almost Well Gaged', '#006a6a',
           '#ccc'
         ],
        'line-width': {
          property: 'StrmOrder', // this will be your density property form you geojson
          stops: [
            [3, 1],
            [6, 2],
            [12, 3]
        ]
        } //end line width
      }, //end paint
      'layout': {'visibility': 'none'},
    }); // end river layer

    map.addLayer({
      id: "river_name",
      type: "symbol",
      'source-layer': 'gap_flowlines',
      source: "gap-layer",
      layout: {
          "visibility": 'none',
          "text-field": "{SU_Name}",
          //"text-size": 10,
          'symbol-placement': "line"
      },
      paint: {
          "text-color": "blue",
          "text-halo-color": "#fff",
          "text-halo-width": 2,   "text-halo-blur": 0,
      }
      });
  
    //#########################################################################################
    //         ADD GAUGE DATA HERE
    //#########################################################################################
      
    map.addLayer({
      'id': 'streamgauge-layer',
      'type': 'circle',
      'source-layer':'gages',
      'source': 'gage-layer',
      //'filter': ["in", "sitestatus", gageActive],
      'paint': {
        'circle-radius': 3,
        'circle-color': 'black',
        'circle-opacity': 0.8,
        'circle-stroke-width': 1,
        'circle-stroke-color' : 'black'
          /*https://docs.mapbox.com/mapbox-gl-js/example/data-driven-circle-colors/*/
        },//end paint
        'layout': {'visibility': 'visible'},
    }); //end addLayer
  });//end map.on load function
}//end Draw Map
drawMap();


//Script to call button has to go after function is defined
$('button').on('click', function(){ 
    var clickedLayer = this.id;
    console.log(clickedLayer)

    if(clickedLayer.length > 0){
      var mapLayer;
    //change visibility based on menu grabbed
      if (clickedLayer === "menuHUC12") {mapLayer = "huc12";}
      if (clickedLayer === "menuCounty") {mapLayer = "county";}
      if (clickedLayer === "menuUrban") {mapLayer = "urban";}
      if (clickedLayer === "menuWatershed") {mapLayer = "huc8";}
      if (clickedLayer === "menuBasins") {mapLayer = "huc6";}
      if (clickedLayer === "menuLakes") {mapLayer = "lakes";}
      if (clickedLayer === "menuRivers") {mapLayer = "gap_flowlines";}
      if (clickedLayer === "menuStreamGages") {mapLayer = "streamgauge-layer";}
 
      var visibility = map.getLayoutProperty(mapLayer, 'visibility'); 
      
    // toggle layer visibility by changing the layout object's visibility property
      if (visibility === 'visible') {
        map.setLayoutProperty(mapLayer, 'visibility', 'none');
          this.style.backgroundColor = 'lightgray';
          this.style.color = "black";
        } else {
          map.setLayoutProperty(mapLayer, 'visibility', 'visible');
          this.style.backgroundColor = '#3f97a8';
          this.style.color = "white";
        }

      //add label text as needed
      if (mapLayer === "huc8"){ 
          if (visibility === 'none') {
            map.setLayoutProperty('huc8_name', 'visibility', 'visible');
          } else {
            map.setLayoutProperty('huc8_name', 'visibility', 'none');
          }
        }//end if mapLaye4

        if (mapLayer === "county"){ 
          if (visibility === 'none') {
            map.setLayoutProperty('county_name', 'visibility', 'visible');
          } else {
            map.setLayoutProperty('county_name', 'visibility', 'none');
          }
        }//end if mapLayer

        if (mapLayer === "huc6"){ 
          if (visibility === 'none') {
            map.setLayoutProperty('huc6_name', 'visibility', 'visible');
          } else {
            map.setLayoutProperty('huc6_name', 'visibility', 'none');
          }
        }//end if mapLayer

        // if (mapLayer === "urban"){ 
        //   if (visibility === 'none') {
        //     map.setLayoutProperty('urban_name', 'visibility', 'visible');
        //   } else {
        //     map.setLayoutProperty('urban_name', 'visibility', 'none');
        //   }
        // }//end if mapLayer

        if (mapLayer === "gap_flowlines"){ 
          if (visibility === 'none') {
            map.setLayoutProperty('river_name', 'visibility', 'visible');
          } else {
            map.setLayoutProperty('river_name', 'visibility', 'none');
          }
        }//end if mapLayer

       //set up legend for streamgages and flowlines
       streamLegend.innerHTML = "<h4>Gage Analysis</h4>";
        var layers = ["Well Gaged", "Almost Well Gaged", "Poorly Gaged"];
        var colors = ["#00006a", "#006a6a", "#6a0000"];

        for (i = 0; i < layers.length; i++) {
            var layer = layers[i];   var color = colors[i];
            var item = document.createElement('div');  
            var key = document.createElement('span');
            key.style.width = "10px"; //change default span width
            key.className = 'legend-key';
            key.style.backgroundColor = color;

            var value = document.createElement('span');
            value.style.width = "120px"; //change default span width
            value.innerHTML = layer;
            item.appendChild(key);
            item.appendChild(value);
            streamLegend.appendChild(item);
          }

        if (mapLayer === "gap_flowlines"){ 
          if (visibility === 'none') {
            map.setLayoutProperty("gap_flowlines", 'visibility', 'visible');
            //add legend to map
             streamLegend.style.display = 'block';
          } else {
            map.setLayoutProperty('gap_flowlines', 'visibility', 'none');
            streamLegend.style.display = 'none';
          }
        }//end if mapLayer for streams----------------------------------------------------------------


    }//end if clickedLayer>0 (need to do because mapbox zoom are buttons)
    
}); // end button script







