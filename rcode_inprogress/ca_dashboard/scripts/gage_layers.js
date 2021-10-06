// ##########################################################################################
//
//         THIS SCRIPT TURNS GAGES ON AND OFF BASED ON MAP SELECTION     
//
// ##########################################################################################



//Loops through all the functions to change maplayer accordingly
function drawGages(gageActive){
    var mapFilter = ["all"]

    //filter gages based on activity
    if(gageActive !== "All"){
        //map.setFilter("streamgauge-layer", ["in", "sitestatus", gageActive]);
        mapFilter.push(["in", "sitestatus", gageActive])
    } //if not all then don't push a filter to the array
    
    //filter based on ownership

    //filter based on purpose

    //filter based on measurement


  //draw full Filter
  map.setFilter('streamgauge-layer', mapFilter)
}


// var breweryFilter=[
//     "all",
//     ["in", "stateNam", 'Utah','Texas','Florida'],
//     ["in", "breweryType", 'Irish','American']
// ]
// map.setFilter('breweriesLayer',breweryFilter)
