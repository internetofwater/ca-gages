#
# This is a Shiny web application. You can run the application by clicking the 'Run App' button above. Visit http://shiny.rstudio.com/
#

#function for each html tag
#tags$h1() is like <h1>; tabs$a() is like <a>
#the elements are lists

#to include an image in an app - make a www folder in the same folder with your app and put images in there
#fluidPage is equivalent with the div in html
#You can pass html directly into shiny using the HTML() function


library(shiny); library(leaflet); library(leaflet.extras)
library(tidyverse); library(sf);  library(httr);

#load in stream gages and other layers
swd_shiny = "..//www_data//";
#swd_shiny = "www_data//"
g <- sf::read_sf(paste0(swd_shiny, "stream_gages.geojson"))
huc6 <- sf::read_sf(paste0(swd_shiny, "huc6.geojson"))
huc8 <- sf::read_sf(paste0(swd_shiny, "huc8.geojson"))
#huc10 <- sf::read_sf(paste0(swd_shiny, "huc10.geojson"))

#set color of gages
g <- g %>% mutate(colGage = ifelse(sitestatus=="Active", "blue", "lightgray"), colOpacity = ifelse(sitestatus=="Active", 0.4, 0.2))

#create NLDI functions
#selectMethod <- c("UT"); #just to start
#endpoint <- "ca_gages"

get_gages <- function(nldi_point, method=c("DM","UM","UT","DD"), distance){
  d <- httr::GET(paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",
                        nldi_point,"/navigation/", method, "/ca_gages?f=json&distance=",distance), encoding="UTF-8")
  if(length(d$content)> 0){
    features <- read_sf(d)
    #features <- nhdplusTools::navigate_nldi(nldi_comid, mode=method, data_source="ca_gages", distance_km = 6000); #get gages
    return(features)
  } else { print ("no gages found")}
}#end function

get_rivers <- function(nldi_comid, method=c("DM","UM","UT","DD"), distance){
  flowlines <- nhdplusTools::navigate_nldi(nldi_comid, mode=method, data_source="flowlines", distance_km = distance); #get flowlines
  return(flowlines)
}

# User Interface - Creates html for page
ui <- fluidPage(
    HTML("<h1>California Stream Gages</h1>"),
    # Application title
    #titlePanel("California Stream Gages"),
    #h3("a second title"), #don't need to use tags
    #tags$a(href="www.rstudio.com", "RStudio"),
    #tags$p("this is a", tags$strong("Shiny"), "app."),

    # Put your inputs here
    sidebarLayout(
      mainPanel(
        width = 6,
        leafletOutput("map", height = "700px"),
      ),#end Main Panel
      
      sidebarPanel(
        width = 6,
        
        #add checkbox group for selecting direction of trace
        HTML("<h4>Select a direction to trace, then click on the map.</h4>"),
        
        #select direction to trace
        checkboxGroupInput("selectMethod", "Select the direction to search:",
                           choices = c("Upstream Tributaries" = "UT",
                             "Upstream Mainstem" = "UM",
                             "Downstream Mainstem" = "DM",
                             "Downstream Tributaries" = "DD"),
                           selected = c("UT")),
                textOutput("selectText"),
        
        #Select distance to trace
        br(),
        sliderInput("traceDistance", "Select Distance to search for gages (km)",
                    min = 25, max = 2000, value = c(1000), step = 25)
        
        
        #tableOutput("data")
      )#end sidebarPanel
      
     
      )#end SidebarLayout

) #end ui


server <- function(input, output) {
  # update selectMethod
  output$selectText <- renderText({
    #set selectMethod
    direction <- paste0(input$selectMethod, collapse = ",")
    paste("You chose ", direction, " which has a length of ", length(input$selectMethod))
  })
  
  
  
  #update map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng= -119.4, lat=36.8, zoom=5.5) %>% 
      #load tiles
      addProviderTiles("Stamen.TonerLite", group="Toner by Stamen") %>% 
      addProviderTiles("OpenStreetMap", group = "Open Street Map") %>%
      
      #add mapPanes to render layers in specific order
      addMapPane("selected", zIndex = 400) %>% 
      addMapPane("points", zIndex = 300) %>%
      addMapPane("lines", zIndex = 250) %>%
      addMapPane("polygons", zIndex = 200) %>% 

      #load layers
      addPolygons(data = huc8, group = "HUC 8",
                  color ="black", weight = 3,
                  fillOpacity= 0.5, fillColor = "gray", options = pathOptions(pane = "polygons"),
                  label = ~paste0(NAME) 
      ) %>% 
      addPolygons(data = huc6, group = "HUC 6",
                      color ="black", weight = 4,
                      fillOpacity= 0.5, fillColor = "gray", options = pathOptions(pane = "polygons"),
                      label = ~paste0(NAME) 
      )%>% 
      addCircleMarkers(data = g, group = "Stream Gages",
                       radius = 4,
                       stroke = TRUE, color ="black", weight = 1,
                       fillOpacity= g$colOpacity, fillColor = g$colGage, options = pathOptions(pane = "points"),
                       label = ~paste0("Site: ", site_id)
      ) %>% 
      
      #add zoom level for some layers
      #groupOptions("HUC 10", zoomLevels = 8:20) %>% 
      
      #hide the ones you don't want to show on map load
      hideGroup(c("HUC 6", "HUC 8")) %>% 
      
      #add control to turn layers on and off
      addLayersControl(
        baseGroups = c("Toner by Stamen", "Open Street Map"),
        overlayGroups = c("Stream Gages", "HUC 6", "HUC 8"),
        options = layersControlOptions(collapsed = FALSE)
      )  %>% 
      
      addLegend(position="bottomleft", 
                col=c("blue", "lightgray", "red"), 
                labels=c("Active Gages", "Inactive Gages", "Start Location"), 
                opacity=0.8) %>% 
      
      #add search bar
      addSearchOSM() 
  })
  
  #Get map lat long based on click and run script
  observeEvent(input$map_click, {
    click <- input$map_click;
    y = round(as.numeric(as.character(click$lat)),5);
    x = round(as.numeric(as.character(click$lng)), 5);
    
    #x <- -119.158; y <- 36.140227;
    text<-paste("Latitude ", y, "Longtitude ", x)
    leafletProxy("map") %>% clearPopups() %>% addPopups(click$lng, click$lat, text)
    
    #run results
    point <- sf::st_sfc(st_point(c(x,y)),crs=4326)# %>% st_as_sf(); #get location and create sf
    nldi_point <- nhdplusTools::discover_nhdplus_id(point); #find comid for sf
    nldi_comid = list(featureSource="comid", featureID=nldi_point); #create nldi_feature
    
    #call functions
    #selectMethod = "UT"
    #check to see if something is selected
    if(length(input$selectMethod == 0)){
      text2 <- paste0("You must select a direction first")
    }
    
    #run once for the first time
    if(length(input$selectMethod >= 1)){
      direction <- input$selectMethod[1]
      distance <- as.numeric(as.character(input$traceDistance))
      
      rivers <- get_rivers(nldi_comid, direction, distance)
      origin <- rivers$origin; rivers <- rivers[[2]]
      gages <- NA;
      gages <- get_gages(nldi_point, direction, distance)
    
        if(length(input$selectMethod >= 2)){
          for (i in 1:length(input$selectMethod)){
            direction <- input$selectMethod[i]
        
            zt.rivers <- get_rivers(nldi_comid, direction, distance); 
              zt.rivers <- zt.rivers[[2]]
            zt.gages <- NA;
            zt.gages <- get_gages(nldi_point, direction, distance)
            
            #bind to original
            rivers <- rbind(rivers, zt.rivers)
            
            if(typeof(gages) == "character" & typeof(zt.gages) != "character") { 
              gages <- zt.gages; 
              }
            if(typeof(gages) != "character" & typeof(zt.gages) != "character"){
              gages <- rbind(gages, zt.gages)
            }
          }#end for loop
        }#end if statemetn for many directions
          
        text2 <- paste0(text, " returned ", dim(gages)[1], " gage(s)")
        #function to grab more gages if needed
        
        
        #show results in map
        point <- point %>% st_as_sf()
        proxy <- leafletProxy("map")
        proxy %>% clearPopups() %>% addPopups(click$lng, click$lat, text2)
        proxy %>% clearGroup("Selected Gages") %>% clearGroup("Streams")
        if(typeof(gages) != "character"){
          g2 <- g %>% filter(site_id %in% gages$identifier)
          proxy %>% addCircleMarkers(data = g2, group = "Selected Gages",
                                   radius = 5,
                                   stroke = TRUE, color ="black", weight = 1,
                                   fillOpacity= 1, fillColor = g2$colGage, options = pathOptions(pane = "selected"),
                                   label = ~paste0("Site: ", site_id))
        }
          proxy %>% addCircleMarkers(data = point, group = "Selected Gages",
                                   radius = 4,
                                   stroke = TRUE, color ="black", weight = 1,
                                   fillOpacity= 1, fillColor = "red", options = pathOptions(pane = "selected")) %>% 
                  addPolylines(data = rivers, group = "Streams",
                              color ="blue", weight = 2, options = pathOptions(pane = "lines") 
                  )
        
        if(typeof(gages) == "character"){
          text2 <- paste0("no gages found")
          proxy %>% clearPopups() %>% addPopups(click$lng, click$lat, text2)
        }
    }#end if a direction is selected

  })#end Observe Event
  



    
} #end server function




# Run the application 
shinyApp(ui = ui, server = server)


# Define server logic that follows 3 rules
#(1) save outputs to display as output$ that matches the name in the output of the ui
#(2) render functions place an r object into the shiny web  e.g. renderPlot({#place all the code I need in here to create a plot})
#can call functions in here
#primary functions are: 
#(1) render to make objects to display: renderPlot(), renderImage(), renderDataTable(), renderPrint()
#(2) modularize code with reactivity... small chunks are better
#leaflet has its own reactive function
#(3) use input values with input$ to make reactive to users
#Code outside of server runs once



#shinyapps.io is one way to host apps for testing
#could create a shiny server designed to host web apps and open source... also a pro version

