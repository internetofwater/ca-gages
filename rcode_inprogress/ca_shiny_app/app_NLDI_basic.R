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
library(tidyverse); library(sf);  library(httr); library(htmltools)

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

get_gages <- function(nldi_point, method=c("DM","UM","UT","DD"), distance, nldi_data="ca_gages"){
  d <- httr::GET(paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",
                        nldi_point,"/navigation/", method, "/",nldi_data,"?f=json&distance=",distance), encoding="UTF-8")
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
  tags$head(
    tags$style(
      # Colorize the actionButton.
      HTML('#table_div{ 
            background-color:white;  
            margin: 0px;
            padding: 5px;
           }'),
      HTML('#filter_div{ 
            background-color:#ededed; 
            margin: 0px;
            padding: 10px;
            border-radius: 5px;
           }')
    )
  ), 
  
  
    HTML("<h1>California Stream Gages</h1>"),
    # Put your inputs here
    #sidebarLayout(
  fluidRow(
    column( 
      #mainPanel(
        width = 6, 
        leafletOutput("map", height = "800px"),
      ),#end Main Panel
      
      #sidebarPanel(
      column(
        width = 6, 
        #add checkbox group for selecting direction of trace
        
        tags$div(id="filter_div",
        HTML("<h4 style='color: rgb(26,131,130)'>Select direction(s) and distance, then click on the map to see what gages are already present.</h4>"),
        
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
        sliderInput("traceDistance", "Select distance to search for gages (km)",
                    min = 25, max = 2000, value = c(1000), step = 25),
        hr(),
        ), #end filter_div
        
        
        tags$div(id = "table_div",
          #provide information about what was selected
          htmlOutput("dataText"),
          br(),
          HTML("<h4>Summary of Active Gages Found</h4>"),
          htmlOutput("tableText"),
          div(tableOutput("dataPurpose"), 
              style="font-size: 12px"),
          br(),
          div(tableOutput("dataCollected"),
              style="font-size: 12px")
        )#end main div
      )#end sidebarPanel
      
     
      )#end SidebarLayout

) #end ui

server <- function(input, output) {
  # update selectMethod
  #output$selectText <- renderText({
  #set selectMethod
  #  direction <- paste0(input$selectMethod, collapse = ",")
  #  paste("You chose ", direction, " which has a length of ", length(input$selectMethod))
  #})
  
  
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
    
    x <- -119.158; y <- 36.140227;
    #text<-paste("Latitude ", y, "Longtitude ", x)
    #leafletProxy("map") %>% clearPopups() %>% addPopups(click$lng, click$lat, text)
    
    #run results
    point <- sf::st_sfc(st_point(c(x,y)),crs=4326)# %>% st_as_sf(); #get location and create sf
    nldi_point <- nhdplusTools::discover_nhdplus_id(point); #find comid for sf
    nldi_comid = list(featureSource="comid", featureID=nldi_point); #create nldi_feature
    
    #call functions
    #selectMethod = "UT"
    #check to see if something is selected
    if(length(input$selectMethod) == 0){
      text2 <- paste0("<strong><span style='color: red'>You must select a direction first</span></strong>")
      output$dataText <- renderUI({HTML(text2)})#end renderText
    }
    
    #run once for the first time
    if(length(input$selectMethod) >= 1){
      direction <- input$selectMethod[1]
      distance <- as.numeric(as.character(input$traceDistance))
      
      rivers <- get_rivers(nldi_comid, direction, distance)
      origin <- rivers$origin; rivers <- rivers[[2]]
      gages <- NA;
      gages <- get_gages(nldi_point, direction, distance)
      
      if(length(input$selectMethod) >= 2){
        for (i in 2:length(input$selectMethod)){
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
    }
      #text2 <- paste0(text, " returned ", dim(gages)[1], " gage(s)")
      
      #show results in map
      point <- point %>% st_as_sf()
      proxy <- leafletProxy("map")
      
      proxy %>% clearGroup("Selected Gages") %>% clearGroup("Streams")
      
      #Add origin point and rivers to the map regardless
      proxy %>% addCircleMarkers(data = point, group = "Selected Gages",
                                 radius = 4, stroke = TRUE, color ="black", weight = 1,
                                 fillOpacity= 1, fillColor = "red", options = pathOptions(pane = "selected")) %>% 
        addPolylines(data = rivers, group = "Streams",
                     color ="blue", weight = 2, options = pathOptions(pane = "lines"))
      
      #Add only if gages were found
      if(typeof(gages) != "character"){
        output$tableText <- renderUI({""})
        g2 <- g %>% filter(site_id %in% gages$identifier)
        text2 <- paste0("You have found ", dim(g2)[1], " gage(s).",  '<br/>', "There were ", dim(subset(g2, sitestatus=="Active"))[1], " active gages and ",
                        dim(subset(g2, sitestatus=="Inactive"))[1], " inactive gages.")
        
        #add layers to map  
        proxy %>% addCircleMarkers(data = g2, group = "Selected Gages",
                                   radius = 5, stroke = TRUE, color ="black", weight = 1,
                                   fillOpacity= 1, fillColor = g2$colGage, options = pathOptions(pane = "selected"),
                                   label = ~paste0("Site: ", site_id))
        
        #draw tables
        g2 <- g2 %>% filter(sitestatus == "Active")
        g3 <- g2 %>% select(operator2, floodmgmt, ecosysmgmt,grdwtrmgmt,pubsafety,wtrsupply, stage_yn,flow_yn,temp_yn,watqual_yn)
        g3[is.na(g3)] <- 'N'; #set all NA to zero to work in tables
        #create a table of results
        outTable <- as.data.frame(unique(g3$operator2)); colnames(outTable) <- c("operator")
        
        flood <- as.data.frame(table(g3$operator2, g3$floodmgmt)) %>% filter(Var2=='Y') %>% select(Var1, Freq) %>% rename(flood = Freq); flood
        eco <- as.data.frame(table(g3$operator2, g3$ecosysmgmt)) %>% filter(Var2=='Y') %>% select(Var1, Freq) %>% rename(ecosystem = Freq); eco
        gw <- as.data.frame(table(g3$operator2, g3$grdwtrmgmt)) %>% filter(Var2=='Y') %>% select(Var1, Freq) %>% rename(`ground water` = Freq); gw
        public <- as.data.frame(table(g3$operator2, g3$pubsafety)) %>% filter(Var2=='Y') %>% select(Var1, Freq) %>% rename(`public safety` = Freq); public
        supply <- as.data.frame(table(g3$operator2, g3$wtrsupply)) %>% filter(Var2=='Y') %>% select(Var1, Freq) %>% rename(`water supply` = Freq); supply
        
        outTable <- merge(outTable, flood, by.x="operator", by.y="Var1", all=TRUE)
        outTable <- merge(outTable, eco, by.x="operator", by.y="Var1", all=TRUE)
        outTable <- merge(outTable, gw, by.x="operator", by.y="Var1", all=TRUE)
        outTable <- merge(outTable, public, by.x="operator", by.y="Var1", all=TRUE)
        outTable <- merge(outTable, supply, by.x="operator", by.y="Var1", all=TRUE)
        t1Results <- outTable %>% mutate(operator = "<strong>TOTAL</strong>") %>% group_by(operator) %>% 
          summarize(flood = sum(flood, na.rm=TRUE), ecosystem = sum(ecosystem, na.rm=TRUE), `ground water`=sum(`ground water`, na.rm=TRUE), 
                    `public safety`=sum(`public safety`, na.rm=TRUE), `water supply`=sum(`water supply`, na.rm=TRUE))
        t1Results$flood <- paste0("<strong>",t1Results$flood,"</strong>")
        t1Results$ecosystem <- paste0("<strong>",t1Results$ecosystem,"</strong>")
        t1Results$`ground water` <- paste0("<strong>",t1Results$`ground water`,"</strong>")
        t1Results$`public safety` <- paste0("<strong>",t1Results$`public safety`,"</strong>")
        t1Results$`water supply` <- paste0("<strong>",t1Results$`water supply`,"</strong>")
        
        outTable <- rbind(outTable, t1Results);
        outTable[is.na(outTable)] <- 0; #set all NA to zero to work in tables
        
        
        #draw table
        output$dataPurpose = renderTable({head(outTable)}, sanitize.text.function=function(x){x},
                                         striped = FALSE,
                                         bordered = TRUE,
                                         hover=TRUE,
                                         spacing = 'xs',
                                         width = "auto",
                                         align = 'c',
                                         digits = 0);
        
        
        #Now the second table
        outTable2 <- as.data.frame(unique(g3$operator2)); colnames(outTable2) <- c("operator")
        stage <- as.data.frame(table(g3$operator2, g3$stage_yn)) %>% filter(Var2=='Y') %>% select(Var1, Freq) %>% rename(stage = Freq); stage
        flow <- as.data.frame(table(g3$operator2, g3$flow_yn)) %>% filter(Var2=='Y') %>% select(Var1, Freq) %>% rename(flow = Freq); flow
        quality <- as.data.frame(table(g3$operator2, g3$watqual_yn)) %>% filter(Var2=='Y') %>% select(Var1, Freq) %>% rename(`water quality` = Freq); quality
        temp <- as.data.frame(table(g3$operator2, g3$temp_yn)) %>% filter(Var2=='Y') %>% select(Var1, Freq) %>% rename(temperature = Freq); temp
        
        outTable2 <- merge(outTable2, stage, by.x="operator", by.y="Var1", all=TRUE)
        outTable2 <- merge(outTable2, flow, by.x="operator", by.y="Var1", all=TRUE)
        outTable2 <- merge(outTable2, quality, by.x="operator", by.y="Var1", all=TRUE)
        outTable2 <- merge(outTable2, temp, by.x="operator", by.y="Var1", all=TRUE)
        t2Results <- outTable2 %>% mutate(operator = "<strong>Total</strong>") %>% group_by(operator) %>% 
          summarize(stage = sum(stage, na.rm=TRUE), flow = sum(flow, na.rm=TRUE), `water quality`=sum(`water quality`, na.rm=TRUE), temperature=sum(temperature, na.rm=TRUE))
        t2Results$stage <- paste0("<strong>",t2Results$stage,"</strong>")
        t2Results$flow <- paste0("<strong>",t2Results$flow,"</strong>")
        t2Results$`water quality` <- paste0("<strong>",t2Results$`water quality`,"</strong>")
        t2Results$temperature <- paste0("<strong>",t2Results$temperature,"</strong>")
        outTable2 <- rbind(outTable2, t2Results);
        outTable2[is.na(outTable2)] <- 0; #set all NA to zero to work in tables
        #draw table in UI
        output$dataCollected = renderTable({head(outTable2)}, sanitize.text.function=function(x){x},
                                           striped = FALSE,
                                           bordered = TRUE,
                                           hover=TRUE,
                                           spacing = 'xs',
                                           width = "auto",
                                           align = 'c',
                                           digits = 0);
        
        
        
      }#end if gages found
      
      
      
      if(typeof(gages) == "character"){
        text2 <- paste0("No gages were found")
        output$tableText <- renderUI({"No active gages were found."})
      }
      
      #proxy %>% clearPopups() %>% addPopups(click$lng, click$lat, text2)
      #output$dataText <- renderText({text2})#end renderText
      output$dataText <- renderUI({HTML(text2)})#end renderText
      
      
    #}#end if a direction is selected
  })#end Observe Event
  
  
  
  
  
} #end server function




# Run the application 
shinyApp(ui = ui, server = server)

