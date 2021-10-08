// ##########################################################################################
//
//         THIS SCRIPT DRAWS DIFFERENT LEGENDS FOR THE HUC 12    
//
// ##########################################################################################


///////////////////////////////////////////////////////////////////////////////////////////////////
//  This function filters by selection
///////////////////////////////////////////////////////////////////////////////////////////////////
function setHUCLegendThis(target) {
  //Change variable
  hucLegendDrawn = document.getElementById('setHUCLegend').value;
  console.log(hucLegendDrawn);

  drawHUC12(hucLegendDrawn);
  return hucLegendDrawn
} // end setLegendThis function ---------------------------------------------------------




function drawHUC12(hucLegendDrawn){
  //remove map layer
  map.removeLayer('huc12');

  if(hucLegendDrawn === "none"){
    map.addLayer({
      'id': 'huc12',
      'type': 'fill',
      'source': 'huc12-layer',
      'source-layer': 'huc12',
      'paint': {
        'fill-color': 'rgba(0,0,0,0)',
        'fill-outline-color': 'black',
        'fill-opacity': 1,
      }, //end paint
    },
    'county' //I think this puts it below the county layer
    ); // end huc12 map layer

        //set up legend huc12-legend
          hucLegend.style.display = 'none';
  }//end if none-----------------------------------------------------------------------------------------=----

  if(hucLegendDrawn === "conservationStatus"){
    map.addLayer({
      'id': 'huc12',
      'type': 'fill',
      'source': 'huc12-layer',
      'source-layer': 'huc12',
      'paint': {
        'fill-color': [
          'match',
          ['get', 'cons_strat'],
          'Restore & Mitigate', '#ea3119',
          'Restore & Monitor', '#36bdcd',
          'Secure & Mitigate', '#cd8536',
          'Secure & Monitor', '#3680cd',
          '#ccc'
        ],
        'fill-outline-color': 'white',
        'fill-opacity': 0.8,
      }, //end paint
    },
    'county' //I think this puts it below the county layer
    ); // end huc12 map layer

    //set up legend huc12-legend
       hucLegend.innerHTML = "<h4>HUC Legend: Conservation Status</h4>";
        var layers = ["Restore & Mitigate", "Restore & Monitor", "Secure & Mitigate", "Secure & Monitor", "None"];
        var colors = ["#ea3119", "#36bdcd", "#cd8536", "#3680cd", "#ccc"];
  }//end if conservation Status--------------------------------------------------------------------------------------------

  //FIRO 
  if(hucLegendDrawn === "firoStatus"){
    map.addLayer({
      'id': 'huc12',
      'type': 'fill',
      'source': 'huc12-layer',
      'source-layer': 'huc12',
      'paint': {
        'fill-color': [
          'match',
          ['get', 'firo'],
          'yes', 'navy',
          'no', 'white',
          '#ccc'
        ],
        'fill-outline-color': 'white',
        'fill-opacity': 0.5,
      }, //end paint
    },
    'county' //I think this puts it below the county layer
    ); // end huc12 map layer

    //set up legend huc12-legend
      hucLegend.innerHTML = "<h4>HUC Legend: FIRO</h4>";
      var layers = ["FIRO presence", "No FIRO presence"];
      var colors = ["navy", "white"];
  }//end if firo Status-----------------------------------------------------------------------------------------------

  if(hucLegendDrawn === "condIndex"){
    map.addLayer({
      'id': 'huc12',
      'type': 'fill',
      'source': 'huc12-layer',
      'source-layer': 'huc12',
      'filter': ['<=', 'cond_index', 4],
      'paint': {
        'fill-color': {
          'property': "cond_index",
          "stops": [
            [0, 'navy'],
            [1, 'blue'],
            [2, 'orange'],
            [3, 'red'],
            [4, '#ccc'],
          ]
        },
        'fill-outline-color': 'white',
        'fill-opacity': 0.5,
      }, //end paint
    },
    'county' //I think this puts it below the county layer
    ); // end huc12 map layer

    //set up legend huc12-legend
      hucLegend.innerHTML = "<h4>HUC Legend: Conditional Index</h4>";
      var layers = ["0-1", "1.1-2", "2.1-3", "3.1-4", "None"];
      var colors = ["navy", "blue", "orange", "red", '#ccc'];
  }//end if conditional index Status--------------------------------------------------------------------------------------------

  if(hucLegendDrawn === "threatIndex"){
    map.addLayer({
      'id': 'huc12',
      'type': 'fill',
      'source': 'huc12-layer',
      'source-layer': 'huc12',
      'filter': ['<=', 'threat_index', 4],
      'paint': {
        'fill-color': {
          'property': "threat_index",
          "stops": [
            [0, 'navy'],
            [1, 'blue'],
            [2, 'orange'],
            [3, 'red'],
            [4, '#ccc'],
          ]
        },
        'fill-outline-color': 'white',
        'fill-opacity': 0.5,
      }, //end paint
    },
    'county' //I think this puts it below the county layer
    ); // end huc12 map layer

    //set up legend huc12-legend
      hucLegend.innerHTML = "<h4>HUC Legend: Threat Index</h4>";
      var layers = ["0-1", "1.1-2", "2.1-3", "3.1-4", "None"];
      var colors = ["navy", "blue", "orange", "red", '#ccc'];
  }//end if threat index-----------------------------------------------------------------------------------------


    //Aquatic Biodiversity Rank 
    if(hucLegendDrawn === "bioRankAll"){
      map.addLayer({
        'id': 'huc12',
        'type': 'fill',
        'source': 'huc12-layer',
        'source-layer': 'huc12',
        'paint': {
          'fill-color': [
            'match',
            ['get', 'bio_rank'],
            '0', 'darkred',
            '1', 'red',
            '2', 'orange',
            '3', 'yellow',
            '4', 'blue', 
            '5', 'navy',
            '#ccc'
          ],
          'fill-outline-color': 'white',
          'fill-opacity': 0.5,
        }, //end paint
      },
      'county' //I think this puts it below the county layer
      ); // end huc12 map layer
  
      //set up legend huc12-legend
        hucLegend.innerHTML = "<h5>Aquatic Biodiversity</h5>";
        var layers = ["0", "1", "2", "3", "4", "5", "unknown"];
        var colors = ["darkred",  "red", "orange", "yellow","blue", "navy", "#ccc"];
    }//end if aquatic biodiversity Status-----------------------------------------------------------------------------------------------


    //Gage Status for Headwaters
    if(hucLegendDrawn === "gageStatusHead"){
      map.addLayer({
        'id': 'huc12',
        'type': 'fill',
        'source': 'huc12-layer',
        'source-layer': 'huc12',
        'filter': ['>', 'headwater_poor', 0],
        'paint': {
          'fill-color': {
            'property': "headwater_poor",
            "stops": [
              [0, 'navy'],
              [25, 'blue'],
              [50, 'orange'],
              [75, 'red'],
              [100, 'darkred']
            ]
          },
          'fill-outline-color': 'white',
          'fill-opacity': 0.5,
        }, //end paint
      },
      'county' //I think this puts it below the county layer
      ); // end huc12 map layer
  
      //set up legend huc12-legend
        hucLegend.innerHTML = "<h5>Percent of Headwaters Poorly Gage</h5>";
        var layers = ["0-24", "25-49", "50-74", "75-99", "100"];
        var colors = ["navy", "blue", "yellow", "orange", "red", "darkred",];
    }//end if aquatic biodiversity Status-----------------------------------------------------------------------------------------------

    //Gage Status for Headwstreams
    if(hucLegendDrawn === "gageStatusStream"){
      map.addLayer({
        'id': 'huc12',
        'type': 'fill',
        'source': 'huc12-layer',
        'source-layer': 'huc12',
        'filter': ['>', 'stream_poor', 0],
        'paint': {
          'fill-color': {
            'property': "stream_poor",
            "stops": [
              [0, 'navy'],
              [25, 'blue'],
              [50, 'orange'],
              [75, 'red'],
              [100, 'darkred'],
            ]
          },
          'fill-outline-color': 'white',
          'fill-opacity': 0.5,
        }, //end paint
      },
      'county' //I think this puts it below the county layer
      ); // end huc12 map layer
  
      //set up legend huc12-legend
        hucLegend.innerHTML = "<h5>Percent of Streams Poorly Gage</h5>";
        var layers = ["0-24", "25-49", "50-74", "75-99", "100"];
        var colors = ["navy", "blue", "yellow", "orange", "red", "darkred",];
    }//end if aquatic biodiversity Status-----------------------------------------------------------------------------------------------

    //Gage Status for Headwstreams
    if(hucLegendDrawn === "gageStatusRiver"){
      map.addLayer({
        'id': 'huc12',
        'type': 'fill',
        'source': 'huc12-layer',
        'source-layer': 'huc12',
        'filter': ['>=', 'river_poor', 0],
        'paint': {
          'fill-color': {
            'property': "river_poor",
            "stops": [
              [0, 'navy'],
              [25, 'blue'],
              [50, 'orange'],
              [75, 'red'],
              [100, 'darkred'],
            ]
          },
          'fill-outline-color': 'white',
          'fill-opacity': 0.5,
        }, //end paint
      },
      'county' //I think this puts it below the county layer
      ); // end huc12 map layer

      //set up legend huc12-legend
        hucLegend.innerHTML = "<h5>Percent of Rivers Poorly Gage</h5>";
        var layers = ["0-24", "25-49", "50-74", "75-99", "100"];
        var colors = ["navy", "blue", "yellow", "orange", "red", "darkred",];
    }//end if aquatic biodiversity Status-----------------------------------------------------------------------------------------------





  //load legend
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
    hucLegend.appendChild(item);
  }
  hucLegend.style.display = 'block';


}

