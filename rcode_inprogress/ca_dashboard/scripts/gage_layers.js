// ##########################################################################################
//
//         THIS SCRIPT TURNS GAGES ON AND OFF BASED ON MAP SELECTION     
//
// ##########################################################################################

///////////////////////////////////////////////////////////////////////////////////////////////////
//  This function filters by size of system
///////////////////////////////////////////////////////////////////////////////////////////////////
function setOwnerThis(target) {
  //Change variable
  owner = document.getElementById('setOwner').value;
  console.log(owner);

  drawGages(gageActive, owner, gageMeasure)
  return owner
} // end setOwnerThis function


///////////////////////////////////////////////////////////////////////////////////////////////////
//  This function draws the gage layer based on selections
///////////////////////////////////////////////////////////////////////////////////////////////////
//Loops through all the functions to change maplayer accordingly
function drawGages(gageActive, owner, gageMeasure){
    var mapFilter = ["all"]

    //filter gages based on activity
    if(gageActive !== "All"){
        //map.setFilter("streamgauge-layer", ["in", "sitestatus", gageActive]);
        mapFilter.push(["in", "sitestatus", gageActive])
    } //if not all then don't push a filter to the array
    
    //filter based on ownership
    if(owner !== "All"){
      mapFilter.push(["in", "operator2", owner])
    }

    //filter based on purpose
    if(gageMeasure[0] !== "All" & gageMeasure.length > 0){
      //This is for OR
      // if(gageMeasure.length === 1){ mapFilter.push(["in", gageMeasure[0], "Y"]) }
      // if(gageMeasure.length === 2){ mapFilter.push(["in", gageMeasure[0], gageMeasure[1], "Y"]) }
      // if(gageMeasure.length === 3){ mapFilter.push(["in", gageMeasure[0], gageMeasure[1], gageMeasure[2], "Y"]) }
      // if(gageMeasure.length === 4){ mapFilter.push(["in", gageMeasure[0], gageMeasure[1], gageMeasure[2], gageMeasure[3], "Y"]) }

      //This makes it "AND"
      for (i=0; i<gageMeasure.length; i++){
        mapFilter.push(["in", gageMeasure[i], "Y"])
      }

    }

    console.log(mapFilter);
  //draw full Filter
  map.setFilter('streamgauge-layer', mapFilter)
}


// var breweryFilter=[
//     "all",
//     ["in", "stateNam", 'Utah','Texas','Florida'],
//     ["in", "breweryType", 'Irish','American']
// ]
// map.setFilter('breweriesLayer',breweryFilter)
