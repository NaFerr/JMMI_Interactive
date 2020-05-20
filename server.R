# SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER

server<-function(input, output,session) {
  #_________________________create map consisting of several layers and customization___________________
  output$map1 <- renderLeaflet({  #initiate map
    leaflet(options = leafletOptions(minZoom = 4.5)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>% #base map, can be changed
      setView(50.911019,15.889618, zoom = 6.5)
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
      #mypal<-colorNumeric( palette="RdYlGn", domain=(dataM@data[,6]), na.color = "#9C9D9F", reverse = T)
      mypal<-colorNumeric( palette=(colorRamp(c("#ADFFA5", "#A7383D", "#420A0D"), interpolate="linear")),domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      pLa<-"WASH SMEB: "
      pLa2<-"WASH SMEB"
      en<-" "
      unitA=" YER"
      title_legend<-"WASH SMEB Cost"
    }
    if (VARIA == "exchange_rates") {
      dataM<-Rshp[,c(1,5,7,8,10,27,28,30)] #subset exchange rate col
      mypal<-colorNumeric( palette="Greens", domain=dataM@data[,6], na.color = "#D0CFCF",reverse = F)
      pLa<-"Parallel Exchange Rate: "
      pLa2<-"Parrallel Exchange Rate"
      en<-" "
      unitA=" YER"
      title_legend<-"YER to 1 USD"
    }
    if (VARIA == "petrol") {
      dataM<-Rshp[,c(1,5,7,8,10,19,28,30)] #subset exchange rate col
      mypal<-colorNumeric( palette="YlOrBr", domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      pLa<-"Petrol Price: "
      pLa2<-"Petrol Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (1 L)"
    }
    if (VARIA == "diesel") {
      dataM<-Rshp[,c(1,5,7,8,10,20,28,30)] #subset exchange rate col
      #mypal<-colorNumeric( palette="Reds", domain=dataM@data[,6], na.color = "#9C9D9F", reverse = F)
      mypal<-colorNumeric( palette=(colorRamp(c("#FFD7D9", "#FF535B", "#FB000D", "#830007", "#480004"), interpolate="spline")),domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      pLa<-"Diesel Price: "
      pLa2<-"Diesel Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (1 L)"
    }
    if (VARIA == "bottled_water") {
      dataM<-Rshp[,c(1,5,7,8,10,21,28,30)] #subset exchange rate col
      #mypal<-colorNumeric( palette="PuBu", domain=dataM@data[,6], na.color = "#9C9D9F", reverse = F)
      mypal<-colorNumeric( palette=(colorRamp(c("#C7C0FF", "#7A6AFF", "#1501B9", "#0A005D", "#050033"), interpolate="spline")),domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      pLa<-"Bottled Water Price: "
      pLa2<-"Bottled Water Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (0.75 L)"
    }
    if (VARIA == "treated_water") {
      dataM<-Rshp[,c(1,5,7,8,10,22,28,30)] #subset exchange rate col
      mypal<-colorNumeric( palette=(colorRamp(c("#C3FFFD", "#6EFBF6", "#009F99", "#00504D"), interpolate="linear")),domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      #mypal<-colorNumeric( palette="PuBuGn", domain=dataM@data[,6], na.color = "#9C9D9F", reverse = F)
      pLa<-"Treated Water Price: "
      pLa2<-"Treated Water Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (10 L)"
    }
    if (VARIA == "soap") {
      dataM<-Rshp[,c(1,5,7,8,10,23,28,30)] #subset exchange rate col
      mypal<-colorNumeric( palette="RdPu", domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      pLa<-"Soap Price: "
      pLa2<-"Soap Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (100 g)"
    }
    if (VARIA == "laundry_powder") {
      dataM<-Rshp[,c(1,5,7,8,10,24,28,30)] #subset exchange rate col
      mypal<-colorNumeric( palette="Purples", domain=dataM@data[,6], na.color = "#D0CFCF",reverse = F)
      pLa<-"Laundry Powder Price: "
      pLa2<-"Laundry Powder Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (100 g)"
    }
    if (VARIA == "sanitary_napkins") {
      dataM<-Rshp[,c(1,5,7,8,10,25,28,30)] #subset exchange rate col
      mypal<-colorNumeric( palette="BuPu", domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      pLa<-"Sanitary Napkin Price: "
      pLa2<-"Sanitary Napkin Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (10 Pack)"
    }
    if (VARIA == "cost_cubic_meter") {
      dataM<-Rshp[,c(1,5,7,8,10,26,28,30)] #subset exchange rate col
      #mypal<-colorNumeric( palette="Blues", domain=dataM@data[,6], na.color = "#9C9D9F", reverse = F)
      mypal<-colorNumeric( palette=(colorRamp(c("#C9C3F8", "#5D52AD", "#FAD962", "#AA9239"), interpolate="linear")),domain=dataM@data[,6], na.color = "#D0CFCF", reverse = T)
      pLa<-"Water Trucking Cost per Cubic Meter: "
      pLa2<-"Water Trucking Cost per Cubic Meter"
      en<-" "
      unitA=" YER"
      title<-"Price (Cubic m)"
      title_legend<-title
    
      
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
    pal_alt<-colorBin( palette="#E5E5E5", domain=old_dist_alt_sp@data[,5])
    
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
                  fillColor =  (~mypal((dataM@data[,6]))),#custom palette
                  fillOpacity = .8,
                  layerId = ~admin2pcod,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = FALSE, sendToBack = FALSE),
                  popup = paste0(dataM$admin2name, "<br>",'<h7 style="color:black;">',
                                 pLa, "<b>"," ", dataM@data[,6],unitA, "</b>", '</h7>'),
      ) %>%
      addPolygons(data= old_dist_alt_sp,    # this is you clipped data file of previous districts (make sure it is below your main district on or it will not be seen)
                  color = "red",
                  weight = 1.5,
                  label = paste0(old_dist_alt_sp$admin2name,":previous ",pLa2," data present"), #added a different label that pops up
                  opacity = .40,
                  smoothFactor = 0.5,
                  fill = TRUE,
                  fillColor = ~pal_alt(old_dist_alt_sp@data[,5]), #custom palette as stated before
                  fillOpacity = .8,
                  layerId = ~admin2pcod,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                    bringToFront = FALSE, sendToBack = FALSE),
      )%>%
      addPolylines(data = Admin1, #add governorate lines for reference
                   weight= 3.25,
                   stroke = T,
                   color = "black",
                   fill=FALSE,
                   fillOpacity = 0.1,
                   opacity = 0.1 )
    
    map1 %>% clearControls()
    
   map1 %>% 
     addLegend_decreasing("topleft", pal = mypal, values =  dataM@data[,6], #update legend to reflect changes in selected district/variable shown
                     labFormat=labelFormat(suffix=unitA),
                     title = title_legend,
                     opacity = 5,
                     decreasing = T) 
  
    #needed to make a custom label because i hate R shiny https://stackoverflow.com/questions/52812238/custom-legend-with-r-leaflet-circles-and-squares-in-same-plot-legends
    colors<-c("white", "#D3D3D3", "#D3D3D3")
    labels<-c("Districts with previous data", "Governorate borders", "District borders")
    sizes<-c("20", "20", "20")
    shapes<-c("square", "line", "line")
    borders<-c("red", "#2B2B2B" , "#646464")
    
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
    
    #add new legend 
    map1 %>% addLegendCustom(colors, labels, sizes, shapes, borders)
  
    #add scale bar
    map1 %>% addScaleBar("topleft", options = scaleBarOptions(maxWidth = 100, metric = T, imperial = T, updateWhenIdle = T))
    
  
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

  if(FALSE){
  chartDatMIN<-reactive({  #subset JMMI data table based on variable of interest (soap, water etc)
    if (input$variable1 == "SMEB"){
      min<-min_max_out[2,10]
    }
    if (input$variable1 == "exchange_rates"){
      min<-min_max_out[2,9]
    }
    if (input$variable1 == "petrol"){
      min<-min_max_out[2,1]
    }
    if (input$variable1 == "diesel"){
      min<-min_max_out[2,2]
    }
    if (input$variable1 == "bottled_water"){
      min<-min_max_out[2,3]
    }
    if (input$variable1 == "treated_water"){
      min<-min_max_out[2,4]
    }
    if (input$variable1 == "soap"){
      min<-min_max_out[2,5]
    }
    if (input$variable1 == "laundry_powder"){
      min<-min_max_out[2,6]
    }
    if (input$variable1 == "sanitary_napkins"){
      min<-min_max_out[2,7]
    }
    if (input$variable1 == "cost_cubic_meter"){
      min<-min_max_out[2,8]
    }
    min
  })  
  
  chartDatMAX<-reactive({  #subset JMMI data table based on variable of interest (soap, water etc)
    if (input$variable1 == "SMEB"){
      max<-min_max_out[1,10]
    }
    if (input$variable1 == "exchange_rates"){
      max<-min_max_out[1,9]
    }
    if (input$variable1 == "petrol"){
      max<-min_max_out[1,1]
    }
    if (input$variable1 == "diesel"){
      max<-min_max_out[1,2]
    }
    if (input$variable1 == "bottled_water"){
      max<-min_max_out[1,3]
    }
    if (input$variable1 == "treated_water"){
      max<-min_max_out[1,4]
    } 
    if (input$variable1 == "soap"){
      max<-min_max_out[1,5]
    }
    if (input$variable1 == "laundry_powder"){
      max<-min_max_out[1,6]
    }
    if (input$variable1 == "sanitary_napkins"){
      max<-min_max_out[1,7]
    }
    if (input$variable1 == "cost_cubic_meter"){
      max<-min_max_out[1,8]
    }
    max
  })  
  }
  
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
      y= "Water Trucking Price"
    }
    
    y
  })
  
  
  
  #_________________________Create highcharter element which uses dataset filtered by user inputs___________________
  #NEW OUT PUT FOR DATA TO BE SUBBED LATER
  #https://stackoverflow.com/questions/38113507/r-shiny-keep-retain-values-of-reactive-inputs-after-modifying-selection
  #https://stackoverflow.com/questions/57468457/how-can-i-set-the-yaxis-limits-within-highchart-plot
  
  observe({
    updateSelectInput(session = session, inputId = "varDateSelect", choices = chartData1()$date,selected=lapply(reactiveValuesToList(input), unclass)$varDateSelect)
  })
  
  output$hcontainer <- renderHighchart({
    event <- (input$map1_shape_click) #Critical Line!!!
    
    (validate(need(event$id != "",
                  "Please click on a district to display its history.")))

    ChartDat<-chartData1() #define filtered table that is reactive element chartData1
    #y_min<- chartDatMIN()
    #y_max<- chartDatMAX()
    
    chosenD<- paste0(na.omit(unique(ChartDat[,2])),", ",na.omit(unique(ChartDat[,4]))) #TITLE FOR CHART (governorate and district name)
  
    highchart()%>% #high chart
      hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%b %Y')) %>%
        
      #data for national
      hc_add_series(data=ChartDat, type = "line", hcaes(date2, nat_val), color = "dodgerblue", name=paste(chartNAME(),"-National"))%>%
      #data for governorate
      hc_add_series(data=ChartDat, type = "line", hcaes(date2, governorate_val), color = "forestgreen", name=paste(chartNAME(),"-Governorate"))%>%
      #data for district
      hc_add_series(data=ChartDat, type = "line", hcaes(date2, variableSEL), color="#4F4E51", name=paste(chartNAME(),"-District"))%>%
      
      hc_yAxis(tithcle=list(text=paste0(chartNAME()," in YER")), opposite = FALSE
               #,min= as.numeric(y_min), max= as.numeric(y_max)
    )%>%
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
    dimnames=list(c("Median Price (YER)","Number of Markets Assessed"),c(paste0("District: \n",na.omit(unique(ChartDat_Date[,4]))),paste0("Governorate: \n",na.omit(unique(ChartDat_Date[,2]))),"Yemen")))
  
  DT::datatable(mat_date_test,options = list(dom = 't'))
   })
  
 
  #output infobox for the info exchange rate
  output$info_exchange<-renderValueBox({
    exchange_data<-AdminNatTable
    exchange_data$date<-as.yearmon(exchange_data$date)
    exchange_date<-exchange_data%>%
      filter(date == input$varDateSelect)
    exchange_rate<-exchange_date[1,10]
    
    valueBox(
            value = exchange_rate,
            subtitle="YER to 1 USD", 
            icon = icon("dollar"),
            #fill=T, 
            color = "green")
   
  })
  
  output$text1 <- renderUI({ #customised text elements
    HTML(paste("Coordinate System:  WGS 1984",
               "Administrative boundaries:  OCHA",
               "R Mapping Packages:  leaflet, shiny, highcharter",
               "Please do NOT use Microsoft Edge for best user interaction",
               sep="<br/>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 12px;
         font-family: Helvetica} </style>')
  })
  #
  output$text2 <- renderUI({
    HTML(paste("<i>Note: Data displayed on this map should be interpreted as indicative. 
               In addition, designations and boundaries used here do not imply acceptance 
               by REACH partners and associated donors.</i>",
               sep="<br/>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 11px;
         font-family: Helvetica} </style>')
  })
  
  output$text3 <- renderText({ #LARGE TEXT ABOVE CHART
    paste(chartNAME(), " ", "Medians Over Time")
  })

  output$text_DT<-renderText({
    paste0(chartNAME(),", Median Monthly Costs, and Number of Markets Assessed")
  })
  
  output$text4 <- renderUI({
    HTML(paste("<i>For more information, please visit our",a("REACH Website", target="_blank", href="https://www.reach-initiative.org"), 
    "or contact us directly at yemen@reach-initiative.org.</i>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 11px;
         font-family: Helvetica} </style>')
  })
  
  output$mytable = DT::renderDataTable({
  m_df<- mtcars%>% 
     formatStyle(columns = `mpg`,
                 color = styleInterval(17, (c('black','white'))),
                 backgroundColor = styleInterval(17, (c('white','red'))))
   
  
    #return(as.datatable(formattable(my_df, lapply(1:4, function(col){area(col = col) ~ color_tile("red", "green")}))))
   
  })
  #formattable(mtcars, align = c("l",rep("r", NCOL(mtcars) - 1)), 
   #           list(`mpg` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
    #               area(col = 2:4) ~ function(x) percent(x / 100, digits = 0),
     #              area(col = 2:3) ~ color_tile("#DeF7E9", "#71CA97")))%>%
 
  #
  

  



#FRoM ONLINE https://github.com/ua-snap/shiny-apps/blob/master/cc4liteFinal/server.R

#######################
#SMEB tracker
#######################
#Goal - build system that can adapt to X# of months back and benchmark at YY%
#Data needed - national but may need to clip to districts of each individual one based on methodology
#So pull number of district from most recent month and clip back based on X# of months.
#then build a table function that allows for that stuff to be adapted

#build the dataset

  
  output$table_smeb<-DT::renderDataTable({
    #observe({
    #https://stackoverflow.com/questions/50912519/select-the-number-of-rows-to-display-in-a-datatable-based-on-a-slider-input
    time<-input$months
    percent_time<- input$percent/100
    
    
    #time<-6
    national_data_test<-nat_data()
    national_data_test<-AdminNatTable
    national_data_test$date2 <- as.yearmon(national_data_test$date)
    national_data<-arrange(national_data_test,desc(date2))
    
    month_all<-sort(unique(national_data$date2),decreasing = T)
    time_pull<-month_all[time]
    month_list<-month_all[1:match(time_pull,month_all)]
    
    #now have the month_list which we can cut from in the future
    
    national_data_pull<-dplyr::filter(national_data, date2==month_list)%>%
      dplyr::select(-c(date,num_obs,exchange_rates))
    
    
    national_data_pull<-national_data_pull%>%
      reshape2::melt("date2")%>%
      reshape2::dcast(variable ~ date2)%>%
      round_df(.,0)
    
    col_data_pull<-ncol(national_data_pull)
    
    name_perc_change<-paste0("Percentage change between ",
                             colnames(national_data_pull[2]),
                             " - ",
                             colnames(national_data_pull[col_data_pull]))
    #Add SMEB base costs
    #https://stackoverflow.com/questions/13502601/add-insert-a-column-between-two-columns-in-a-data-frame
    national_data_pull<-national_data_pull%>%
      add_column(.,  `SMEB Base`= c(365,430,100,120,100,150,500,2000,12000), .after = 1)%>%
      add_column(.,  `Variable`= c("Petrol","Diesel","Bottled water","Treated water","Soap","Laundry powder","Sanitary napkins","Water trucking","SMEB total"), .after = 1)%>%
      dplyr::select(c(-1))
    
    #get number of columns now we will use later in the formatting of the table
    columns_of_data_begin<-ncol(national_data_pull)+1
    
    #get the column number of the percent change for future formatting
    percent_col<-time+2
    col_format_last<-time+1
    
    
    
    #https://duckduckgo.com/?q=dynamic+naming+in+mutate+R&t=brave&ia=web
    national_data_pull<-national_data_pull%>%
      dplyr::mutate_at(., .vars = c(3:ncol(.)),.funs = list(`Percent Change from Base`= ~((.-(national_data_pull[,2]))/(national_data_pull[,2]))                                                        ))
    
    #get number of columns now we will use later in the formatting of the table 
    columns_of_data_end<-ncol(national_data_pull)
    
    #get rid of the weird naming from the mutate_at
    names(national_data_pull) <- gsub("_", " ", names(national_data_pull))
    
    #maybe keep for later
    #dplyr::mutate(.,!!name_perc_change := (((.[,col_data_pull]-.[,2])/(.[,2]))))
    
    
    petrol_bench<-national_data_pull[2,2]*(1+percent_time)
    diesel_bench<-national_data_pull[3,2]*(1+percent_time)
    #Render the output DT
    #https://stackoverflow.com/questions/60659666/changing-color-for-cells-on-dt-table-in-shiny
    #https://blog.rstudio.com/2015/06/24/dt-an-r-interface-to-the-datatables-library/
    DT::datatable(national_data_pull,extensions = c('FixedColumns'), 
                  options = list(searching = F, paging = F, scrollX=T, fixedColumns = list(leftColumns = 2, rightColumns = 0)),
                  rownames = F)%>%
      formatStyle(columns = 2, color = "white", backgroundColor = "grey", fontWeight = "bold")%>%
      DT::formatPercentage(columns = c(columns_of_data_begin:columns_of_data_end),2)%>%
      formatStyle(columns = c(columns_of_data_begin:columns_of_data_end),
                  color = styleInterval(c(-percent_time,percent_time), c('grey', 'black','grey')),
                  backgroundColor = styleInterval(c(-percent_time,percent_time), c('#66FF66', 'white','#FA5353')),
                  fontWeight = styleInterval(c(-percent_time,percent_time),c('bold','normal','bold')))
    
    
  })
  

  
  
}