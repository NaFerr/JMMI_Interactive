# SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER

server<-function(input, output,session) {
  #_________________________create map consisting of several layers and customization___________________
  output$map1 <- renderLeaflet({  #initiate map
    leaflet(options = leafletOptions(minZoom = 4.5)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>% #base map, can be changed
      setView(45.4064,15.0527, zoom = 8)
    # setMaxBounds( lng1 = -66.9
    #               , lat1 = 37
    #               , lng2 = -66.1
    #               , lat2 = 37.8 )
  })
  
  #"observe" inputs to define variables for map colors, titles, legends and subset district data
  observe({
    VARIA <- input$variable1
    #YlOrRd #YlGnBu #RdPu #OrRd #Greys #Greens #viridis #magma
    if (VARIA == "SMEB") {
      dataM<-Rshp[,c(1,5,7,8,10,29,28,30)] #subset exchange rate col
      mypal<-colorNumeric( palette="magma", domain=dataM@data[,6], na.color = "#9C9D9F", reverse = T)
      pLa<-"SMEB: "
      pLa2<-"SMEB"
      en<-" "
      unitA=" YER"
      
    }
    if (VARIA == "exchange_rates") {
      dataM<-Rshp[,c(1,5,7,8,10,27,28,30)] #subset exchange rate col
      mypal<-colorNumeric( palette="OrRd", domain=dataM@data[,6], na.color = "#9C9D9F")
      pLa<-"Parallel Exchange Rate: "
      pLa2<-"Parrallel Exchange Rate"
      en<-" "
      unitA=" YER"
      
    }
    if (VARIA == "petrol") {
      dataM<-Rshp[,c(1,5,7,8,10,19,28,30)] #subset exchange rate col
      mypal<-colorNumeric( palette="YlOrRd", domain=dataM@data[,6], na.color = "#9C9D9F")
      pLa<-"Petrol Price: "
      pLa2<-"Petrol Price"
      en<-" "
      unitA=" YER"
      
    }
    if (VARIA == "diesel") {
      dataM<-Rshp[,c(1,5,7,8,10,20,28,30)] #subset exchange rate col
      mypal<-colorNumeric( palette="magma", domain=dataM@data[,6], na.color = "#9C9D9F")
      pLa<-"Diesel Price: "
      pLa2<-"Diesel Price"
      en<-" "
      unitA=" YER"
      
    }
    if (VARIA == "bottled_water") {
      dataM<-Rshp[,c(1,5,7,8,10,21,28,30)] #subset exchange rate col
      mypal<-colorNumeric( palette="Reds", domain=dataM@data[,6], na.color = "#9C9D9F")
      pLa<-"Bottled Water Price: "
      pLa2<-"Bottled Water Price"
      en<-" "
      unitA=" YER"
      
    }
    if (VARIA == "treated_water") {
      dataM<-Rshp[,c(1,5,7,8,10,22,28,30)] #subset exchange rate col
      mypal<-colorNumeric( palette="YlGnBu", domain=dataM@data[,6], na.color = "#9C9D9F")
      pLa<-"Treated Water Price: "
      pLa2<-"Treated Water Price"
      en<-" "
      unitA=" YER"
      
    }
    if (VARIA == "soap") {
      dataM<-Rshp[,c(1,5,7,8,10,23,28,30)] #subset exchange rate col
      mypal<-colorNumeric( palette="viridis", domain=dataM@data[,6], na.color = "#9C9D9F")
      pLa<-"Soap Price: "
      pLa2<-"Soap Price"
      en<-" "
      unitA=" YER"
      
    }
    if (VARIA == "laundry_powder") {
      dataM<-Rshp[,c(1,5,7,8,10,24,28,30)] #subset exchange rate col
      mypal<-colorNumeric( palette="Greens", domain=dataM@data[,6], na.color = "#9C9D9F")
      pLa<-"Laundry Powder Price: "
      pLa2<-"Laundry Powder Price"
      en<-" "
      unitA=" YER" 
      
    }
    if (VARIA == "sanitary_napkins") {
      dataM<-Rshp[,c(1,5,7,8,10,25,28,30)] #subset exchange rate col
      mypal<-colorNumeric( palette="BuPu", domain=dataM@data[,6], na.color = "#9C9D9F")
      pLa<-"Sanitary Napkin Price: "
      pLa2<-"Sanitary Napkin Price"
      en<-" "
      unitA=" YER"
   
    }
    if (VARIA == "cost_cubic_meter") {
      dataM<-Rshp[,c(1,5,7,8,10,26,28,30)] #subset exchange rate col
      mypal<-colorNumeric( palette="RdPu", domain=dataM@data[,6], na.color = "#9C9D9F")
      pLa<-"Water Trucking Cost per Cubic Meter: "
      pLa2<-"Water Trucking Cost per Cubic Meter"
      en<-" "
      unitA=" YER"
      
    }
    
    #Have a vector of all of the districts that currently have data for the current month
    dataM_NAs<-dataM@data%>%
      filter(is.na(date2))%>%
      pull(admin2pcod)
    
    #get name of variable selected
    call_name<-colnames(dataM@data[6])
    
    #Need to subset out for with districts have had values in the past that arent in this current month, get that as a list
    Admin2data_out <- Admin2table %>% #subset out recent month dates to attach to shapefile
      filter(district_ID %in% dataM_NAs)%>% #next only keep data from places that have had an observation in the past from the right varialbe
      filter(!is.na(get(call_name)))
    
    Admin2data_out <- Admin2data_out[!duplicated(Admin2data_out$district_name),]
    
    Rshp@data<-Rshp@data%>%
      mutate(alt_dist = admin2pcod %in% Admin2data_out$district_ID)
    Rshp@data$alt_dist<-Rshp@data$alt_dist*1
    Rshp@data$alt_dist<-(dplyr::na_if(Rshp@data$alt_dist,0))
      
    #final dataset that needs to be pulled from the right Rshp (this is after you dynamically pull out the right data depending on what is selected)
    old_dist <- Rshp[,c(1,5,7,8,31)] 
    #convert the data list (shapefile list) to a more useable list we can filter from while holding the coordinates https://gis.stackexchange.com/questions/156268/r-filter-shapefiles-with-dplyr/156290
    old_dist_alt<-sf::st_as_sf(old_dist)
    #actually do a filter on the stuff you want (this corresponds to the Rshp 31 and the 1 and NA we did before), basically its a janky way to do a clip on the fly from a GIS perspective
    old_dist_alt<-old_dist_alt%>%filter(alt_dist == 1)
    #convert back to a spatial datalist https://gis.stackexchange.com/questions/239118/r-convert-sf-object-back-to-spatialpolygonsdataframe
    old_dist_alt_sp<-sf::as_Spatial(old_dist_alt)
    #create a new color pallete for the new over lay
    pal_alt<-colorBin( palette="white", domain=old_dist_alt_sp@data[,5])
    
    #leaflet map proxy updates created map to reflect obeserved changes
    map1<-leafletProxy("map1") %>%
      clearShapes() %>% #clear polygons, so new ones can be added
      clearControls()%>% #reset zoom etc
      
      
      addLabelOnlyMarkers(centroids, #ADD governorate LABELS
                          lat=centroids$lat, 
                          lng=centroids$lon, 
                          label=as.character(centroids$admin1name),
                          labelOptions = leaflet::labelOptions(
                            noHide = TRUE,
                            interactive = FALSE,
                            direction = "bottom",
                            textOnly = TRUE,
                            offset = c(0, -10),
                            opacity = 0.6,
                            style = list(
                              "color"= "black",
                              "font-size" = "13px",
                              "font-family"= "Helvetica",
                              "font-weight"= 600)
                          )) %>%
      addLabelOnlyMarkers(YEMl, lat=YEMl$lat, #Add Yemen label 
                          lng=YEMl$lon, 
                          label=as.character(YEMl$name),
                          labelOptions = leaflet::labelOptions(
                            noHide = TRUE,
                            interactive = FALSE,
                            direction = "bottom",
                            textOnly = TRUE,
                            offset = c(0, -10),
                            opacity = 1,
                            style = list(
                              "color"= "black",
                              "font-size" = "24px",
                              "font-family"= "Helvetica",
                              "font-weight"= 800,
                              "letter-spacing"= "3px")
                          )) %>%
      
      addPolygons(data= dataM,    # add subsetted district shapefiles
                  color = "grey",
                  weight = 0.8,
                  label= paste(dataM$admin2name," (", pLa, dataM@data[,6],en, ")"),
                  opacity = 1.0,
                  smoothFactor = 0.8,
                  fill = TRUE,
                  fillColor =  (~mypal(dataM@data[,6])),#custom palette
                  fillOpacity = .8,
                  layerId = ~admin2pcod,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = FALSE, sendToBack = FALSE),
                  popup = paste0(dataM$admin2name, "<br>",'<h7 style="color:black;">',
                                 pLa, "<b>"," ", dataM@data[,6],unitA, "</b>", '</h7>'),
      ) %>%
      addPolygons(data= old_dist_alt_sp,    # this is you clipped data file of previous districts (make sure it is below your main district on or it will not be seen)
                  color = "red",
                  weight = 1.0,
                  label = paste0(old_dist_alt_sp$admin2name,":previous ",pLa2," data pesent"), #added a different label that pops up
                  opacity = 1.0,
                  smoothFactor = 0.5,
                  fill = TRUE,
                  fillColor = ~pal_alt(old_dist_alt_sp@data[,5]), #custom palette as stated before
                  fillOpacity = .8,
                  layerId = ~admin2pcod,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                    bringToFront = FALSE, sendToBack = FALSE),
      )%>%
      addPolylines(data = Admin1, #add governorate lines for reference
                   weight= 2.5,
                   stroke = T,
                   color = "black",
                   fill=FALSE,
                   fillOpacity = 0.1,
                   opacity = 0.1 )
    
    map1 %>% clearControls()
    
    map1 %>% addLegend("topleft", pal = mypal, values = dataM@data[,6], #update legend to reflect changes in selected district/variable shown
                       labFormat=labelFormat(suffix=unitA),
                       title = "Price",
                       opacity = 5)
    
    #needed to make a custom label because i hate R shiny https://stackoverflow.com/questions/52812238/custom-legend-with-r-leaflet-circles-and-squares-in-same-plot-legends
    colors<-c("white")
    labels<-c("Districts with previous data")
    sizes<-c("20")
    shapes<-c("square")
    borders<-c("red")
    
    addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.5){
      
    make_shapes <- function(colors, sizes, borders, shapes) {
      shapes <- gsub("circle", "50%", shapes)
      shapes <- gsub("square", "0%", shapes)
      paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
    }
    make_labels <- function(sizes, labels) {
      paste0("<div style='display: inline-block;height: ", 
        sizes, "px;margin-top: 4px;line-height: ", 
        sizes, "px;'>", labels, "</div>")
    }
    
    legend_colors <- make_shapes(colors, sizes, borders, shapes)
    legend_labels <- make_labels(sizes, labels)
    
    return(addLegend(map1,"topleft", colors = legend_colors, labels = legend_labels, opacity = 0.5))
  }
    
    
    map1 %>% addLegendCustom(colors, labels, sizes, shapes, borders)
    
  })#end of MAP
  
  #_________________________create reactive objects for use in the chart___________________
  clicked_state<- eventReactive(input$map1_shape_click,{ #capture ID of clicked district
    return(input$map1_shape_click$id)
  })
  
  clicked_state_gov<-eventReactive(input$map1_shape_click,{
    gov_id<- substr(input$map1_shape_click$id,1,nchar(input$map1_shape_click$id)-2)
    return(gov_id)
  })
  
  dist_data<-reactive({
    dist_dat<-Admin2table[Admin2table$district_ID==clicked_state(),] #strangely reactive objects are stored as functions
    dist_dat  
  })
  
  gov_data<-reactive({
    gov_dat<-Admin1table[Admin1table$government_ID==clicked_state_gov(),] #adding the government data to the dataset
    gov_dat
  })
  
  nat_data<-reactive({
    nat_dat<-AdminNatTable
    nat_dat
  })
  
  gov_nat_data<-reactive({
    gov_nat_dat<-right_join(gov_data(),nat_data(), by = "date2")
  })
  
  state_data <- reactive({ #subset JMMI data table based on clicked state
    all_dat<-right_join(dist_data(),gov_nat_data(), by = "date2")#using a full join so that the data that was for the other month when district wasnt select is still shown
    all_dat$date<-as.yearmon(all_dat$date2)
    all_dat
  })
  
 
  chartData1<-reactive({  #subset JMMI data table based on variable of interest (soap, water etc)
    if (input$variable1 == "SMEB"){
      r<-state_data()[,c(1:5,16,17,31,43,15,30,42)]
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "exchange_rates"){
      r<-state_data()[,c(1:5,14,17,29,41,15,30,42)]
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "petrol"){
      r<-state_data()[,c(1:5,6,17,21,33,15,30,42)]
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "diesel"){
      r<-state_data()[,c(1:5,7,17,22,34,15,30,42)]
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "bottled_water"){
      r<-state_data()[,c(1:5,8,17,23,35,15,30,42)]
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "treated_water"){
      r<-state_data()[,c(1:5,9,17,24,36,15,30,42)]
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "soap"){
      r<-state_data()[,c(1:5,10,17,25,37,15,30,42)]
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "laundry_powder"){
      r<-state_data()[,c(1:5,11,17,26,38,15,30,42)]
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "sanitary_napkins"){
      r<-state_data()[,c(1:5,12,17,27,39,15,30,42)]
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "cost_cubic_meter"){
      r<-state_data()[,c(1:5,13,17,28,40,15,30,42)]
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    r
  })

  chartNAME<-reactive({ #define element to be used as title for selected variable
    if (input$variable1 == "SMEB"){
      y="SMEB"
    }
    
    if (input$variable1 == "exchange_rates"){
      y="Parallel Exchange Rate"
    }
    
    if (input$variable1 == "petrol"){
      y= "Petrol Price"
    }
    
    if (input$variable1 == "diesel"){
      y= "Diesel Price"
    }
    
    if (input$variable1 == "bottled_water"){
      y= "Bottled Water Price"
    }
    
    if (input$variable1 == "treated_water"){
      y= "Treated Water Price"
    }
    
    if (input$variable1 == "soap"){
      y="Soap Price"
    }
    
    if (input$variable1 == "laundry_powder"){
      y= "Laundry Powder Price"
    }
    
    if (input$variable1 == "sanitary_napkins"){
      y= "Sanitary Napkins Price"
    }
    
    if (input$variable1 == "cost_cubic_meter"){
      y= "Cost of Cubic Meter"
    }
    
    y
  })
  
  
  
  #_________________________Create highcharter element which uses dataset filtered by user inputs___________________
  #NEW OUT PUT FOR DATA TO BE SUBBED LATER
  
  observe({
    updateSelectInput(session = session, inputId = "varDateSelect", choices = chartData1()$date)
  })
  
  
  output$hcontainer <- renderHighchart({
    event <- (input$map1_shape_click) #Critical Line!!!
    
    validate(need(event$id != "",
                  "Please click on a district to display its history."))

    ChartDat<-chartData1() #define filtered table that is reactive element chartData1
    
    chosenD<- paste0(na.omit(unique(ChartDat[,2])),", ",na.omit(unique(ChartDat[,4]))) #TITLE FOR CHART (governorate and district name)
    
    highchart()%>% #high chart
      hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%b %Y')) %>%
      
      #data for national
      hc_add_series(data=ChartDat, type = "line", hcaes(date2, nat_val), color = "dodgerblue", name=paste(chartNAME(),"-National"))%>%
      #data for governorate
      hc_add_series(data=ChartDat, type = "line", hcaes(date2, governorate_val), color = "forestgreen", name=paste(chartNAME(),"-Governorate"))%>%
      #data for district
      hc_add_series(data=ChartDat, type = "line", hcaes(date2, variableSEL), color="#4F4E51", name = chartNAME())%>%
      
      hc_yAxis(title=list(text=paste0(chartNAME()," in YER")), opposite = FALSE) %>%
      hc_title(text=chosenD)%>%
      hc_add_theme(hc_theme_gridlight())%>%
      hc_plotOptions(line = list(
        lineWidth=1.5,
        dataLabels = list(enabled = FALSE)))
    
  })
  #building the table for the observations and prices
  #BUILDING TABLE  https://stackoverflow.com/questions/32149487/controlling-table-width-in-shiny-datatableoutput
  output$out_table_obs<-DT::renderDataTable({
    
  ChartDat_Date<-chartData1()#make a new dataset to play around with from the original state data one above
  
  #https://stackoverflow.com/questions/40152857/how-to-dynamically-populate-dropdown-box-choices-in-shiny-dashboard
  
  ChartDat_Date_filter<-ChartDat_Date%>% #filter out based on what was selected from the varDateSelect
    filter(date == input$varDateSelect)
  
  chosenD_Date<- paste0(na.omit(unique(ChartDat_Date[,2])),", ",na.omit(unique(ChartDat_Date[,4])))#make a quick title for the data table
  
  mat_date_test<-matrix(c(round(ChartDat_Date_filter[6],2),
                          round(ChartDat_Date_filter[8],2),
                          round(ChartDat_Date_filter[9],2),
                          ChartDat_Date_filter[10],
                          ChartDat_Date_filter[11],
                          ChartDat_Date_filter[12]), 
    nrow=2, ncol=3, byrow = T,
    dimnames=list(c("Price (YER)","Number of Observations"),c(paste0("District: \n",na.omit(unique(ChartDat_Date[,4]))),paste0("Governorate: \n",na.omit(unique(ChartDat_Date[,2]))),"Yemen")))
  
  DT::datatable(mat_date_test,options = list(dom = 't'))
   })
  
  
  output$text1 <- renderUI({ #customised text elements
    HTML(paste("Contact:  yemen@reach-initiative.org",
               "Coordinate System:  WGS 1984",
               "Administrative boundaries:  OCHA",
               "R Mapping Packages:  leaflet, shiny, highcharter",
               sep="<br/>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 12px;
         font-family: Helvetica} </style>')
  })
  #
  output$text2 <- renderUI({
    HTML(paste("<i>Note: Data, designations and boundaries contained on
               this map are not warranted to error-free and do not
               imply acceptance by the REACH partners, associated
               donors mentioned on this map</i>",
               sep="<br/>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 11px;
         font-family: Helvetica} </style>')
  })
  output$text3 <- renderText({ #LARGE TEXT ABOVE CHART
    paste(chartNAME(), " ", "Over Time")
  })

  output$text_DT<-renderText({
    paste(chartNAME(), " monthly costs and number of observations")
  })
  
  output$text4 <- renderUI({
    HTML(paste("<i>SMEB Calculation Explaination and Methodology 
                can be located on the information tab</i>",
      sep="<br/>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 11px;
         font-family: Helvetica} </style>')
  })
  #
  
}



#FRoM ONLINE https://github.com/ua-snap/shiny-apps/blob/master/cc4liteFinal/server.R