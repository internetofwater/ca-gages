// ##########################################################################################
//
//         THIS SCRIPT TURNS GAGES ON AND OFF BASED ON MAP SELECTION     
//
// ##########################################################################################

///////////////////////////////////////////////////////////////////////////////////////////////////
//  This function filters by selection
///////////////////////////////////////////////////////////////////////////////////////////////////
function setStatusThis(target) {
  //Change variable
  gageActive = document.getElementById('setStatus').value;
  //console.log(gageActive);

  drawGages(gageActive, owner, gageMeasure, gageManage, gageOrder)
  return gageActive
} // end setStatusThis function ---------------------------------------------------------

function setOwnerThis(target) {
  //Change variable
  owner = document.getElementById('setOwner').value;
  //console.log(owner);

  drawGages(gageActive, owner, gageMeasure, gageManage, gageOrder)
  return owner
} // end setOwnerThis function -------------------------------------------------------------

function setGageOrderThis(target) {
  //Change variable
  gageOrder = document.getElementById('setGageOrder').value;
  //console.log(gageOrder)
  drawGages(gageActive, owner, gageMeasure, gageManage, gageOrder)
  return gageOrder
} // end setOwnerThis function



// the selector will match all input controls of type :checkbox and attach a click event handler 
var test; var allMeasure;
  $("input:checkbox").on('click', function() {
    // in the handler, 'this' refers to the box clicked on
    var $box = $(this);
    var box_name = $box.attr("name")
    
    //call onclick function for gage measuring
    if(box_name === "setGageMeasure"){
     gageMeasure = [];
      $("input:checkbox[name='" + box_name + "']:checked").each(function(i,v){
          gageMeasure.push($(v).val());
      });
      //console.log(gageMeasure);
      drawGages(gageActive, owner, gageMeasure, gageManage, gageOrder);
      return gageMeasure;
    }

    //call onclick function for gage measuring
    if(box_name === "setGageManage"){
     gageManage = [];
      $("input:checkbox[name='" + box_name + "']:checked").each(function(i,v){
          gageManage.push($(v).val());
      });
      //console.log(gageManage);
      drawGages(gageActive, owner, gageMeasure, gageManage, gageOrder);
      return gageManage;
    }
 });


///////////////////////////////////////////////////////////////////////////////////////////////////
//  This function draws the gage layer based on selections
///////////////////////////////////////////////////////////////////////////////////////////////////
//Loops through all the functions to change maplayer accordingly
var mapFilter;
function drawGages(gageActive, owner, gageMeasure, gageManage, gageOrder){
    mapFilter = ["all"]

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
      console.log("gage measure is running with length of: " + gageMeasure.length)
      //This makes it "AND"
      for (i=0; i<gageMeasure.length; i++){
        mapFilter.push(["in", gageMeasure[i], "Y"])
      }
    }//end gageMeasure

    //filter based on management
    if(gageManage[0] !== "All" & gageManage.length > 0){
      for (i=0; i<gageManage.length; i++){
        mapFilter.push(["in", gageManage[i], "Y"])
      }
    }
    
  //filter based on stream order size
  if(gageOrder !== "All"){
    if(gageOrder === "headwaterGage"){mapFilter.push(["<=", "strmorder", '3']) }
    if(gageOrder === "streamGage"){
      mapFilter.push([">", "strmorder", '3']) 
      mapFilter.push(["<", "strmorder", '7']) 
    }
    if(gageOrder === "riverGage"){mapFilter.push([">=", "strmorder", '7']) }
  }

  console.log(mapFilter);
  //draw full Filter
  map.setFilter('streamgauge-layer', mapFilter)

 
  var features = map.querySourceFeatures('gage-layer', {
    sourceLayer: 'gages',
    filter: mapFilter
    });
    //console.log(features)
  
  document.getElementById('gageCount').innerHTML = "There are " + features.length + " gages selected.";
}//end draw Gages function


// var breweryFilter=[
//     "all",
//     ["in", "stateNam", 'Utah','Texas','Florida'],
//     ["in", "breweryType", 'Irish','American']
// ]
// map.setFilter('breweriesLayer',breweryFilter)
