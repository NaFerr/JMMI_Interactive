#GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL

#install packages
library(devtools)
library(shiny)
library(rgdal)
library("googlesheets")
library(googlesheets4)
library(dplyr)
library(leaflet)
library(highcharter)
library(zoo)
library(ggplot2)
library(rmapshaper)
library(classInt)
library(geosphere)
library(shinythemes)
library(ggplot2)
library(sp)
library(purrr)

addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft", 
                                                    "topleft"), pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins	
      
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
      
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
      
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, 
                       na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}


##-------------------------- TABULAR DATA WRANGLE ----------------------
#Read in GoogleSheets
#googlesheets4::sheets_deauth()
#googlesheets4::sheets_auth_configure(.http-oauth)

GSh<- read_sheet('https://docs.google.com/spreadsheets/d/1F7HEGZEe5_6sk_xrFrwg8k9VVWkpprXSC7O3SosWbjc', sheet = 1)%>%
#GSh<-read_excel("data/updated_interactive.xlsx",sheet = 2)%>%
  as_tibble()
  #GSh[4:12]<-as_tibble(as.numeric(as.character(unlist(GSh[4:12]))))#Governorates
  Admin1data <- mutate(GSh, SMEB = as.numeric((soap*10.5+laundry_powder*20+sanitary_napkins*2+as.numeric(cost_cubic_meter)*3.15))) #The SMEB calculation

GSh2<- read_sheet('https://docs.google.com/spreadsheets/d/1NnQNwo3FnEyayGwUk-TUeSRxV04CkiqsDuTePpbYpWs')%>% #Districts
#GSh2<-read_excel("data/updated_interactive.xlsx",sheet = 1)%>%
  as_tibble()
#  GSh2[6:14]<-as_tibble(as.numeric(as.character(unlist(GSh2[6:14])))) #first worksheet
  Admin2data <- mutate(GSh2, SMEB = as.numeric((soap*10.5+laundry_powder*20+sanitary_napkins*2+as.numeric(cost_cubic_meter)*3.15))) #The SMEB caluclation

GShnat<-read_sheet('https://docs.google.com/spreadsheets/d/1Ct9OGhvc6HkuVwDdsukVNthH31fNxxBZmtQrhlVyD7w')%>%#National
#GShnat<-read_excel("data/updated_interactive.xlsx",sheet = 3)%>%
  as_tibble()
 # GShnat[2:10]<-as_tibble(as.numeric(as.character(unlist(GShnat[2:10])))) #first worksheet
  AdminNatData<-mutate(GShnat,SMEB = as.numeric((soap*10.5+laundry_powder*20+sanitary_napkins*2+as.numeric(cost_cubic_meter)*3.15))) #The SMEB caluclation)

#Wrangle Data into appropriate formats
#Governorates
Admin1table<-as.data.frame(Admin1data)
Admin1table$date2<- as.Date(Admin1table$date, format("%d-%b-%y"), tz="UTC")
#Admin1table$date2<- as.Date(x=paste("01-",Admin1table$date, sep=""), format="%d-%b-%y") #format a date column
#Admin1table$date <- as.yearmon(Admin1table$date2)

Admin1data_current <- Admin1table %>% #subset only recent month dates to attach to shapefile
  arrange(desc(date2)) %>%
  filter(date2 == max(date2))
currentD <- as.character(format(max(Admin1table$date2),"%B %Y")) #define current date for disply in dashboard
Admin1table[4:14] <- sapply(Admin1table[4:14], as.numeric)

#Districts
Admin2table <- as.data.frame(Admin2data)
Admin2table$date2<- as.Date(Admin2table$date, format("%d-%b-%y"), tz="UTC")
#Admin2table$date2 <- as.Date(x=paste("01-",Admin2table$date, sep=""), format="%d-%b-%y") #format a date column
#Admin2table$date <- as.yearmon(Admin2table$date2)

Admin2data_current <- Admin2table %>% #subset only recent month dates to attach to shapefile
  arrange(desc(date2)) %>%
  filter(date2 == max(date2))
currentD <- as.character(format(max(Admin2table$date2),"%B %Y")) #define current date for disply in dashboard
Admin2table[6:16] <- sapply(Admin2table[6:16], as.numeric)

#National
AdminNatTable<-as.data.frame(AdminNatData)
AdminNatTable$date2<- as.Date(AdminNatTable$date, format("%d-%b-%y"), tz="UTC")
#AdminNatTable$date2 <- as.Date(x=paste("01-",AdminNatTable$date, sep=""), format="%d-%b-%y")#format a date column
#AdminNatTable$date <- as.yearmon(AdminNatTable$date2)

AdminNatData_current <- AdminNatTable %>% #subset only recent month dates to attach to shapefile
  arrange(desc(date2)) %>%
  filter(date2 == max(date2))
currentD <- as.character(format(max(AdminNatTable$date2),"%B %Y")) #define current date for disply in dashboard

###################
#Build the min and max table
#min_max_tab<-Admin2table[,c(6:14,16)]

#min_max_out<-apply(min_max_tab,2, function(x) max(x,na.rm=T)*1.0)%>%
#  bind_rows(apply(min_max_tab,2, function(x) min(x,na.rm=T)*1.0))
#min_max_out<-apply(min_max_out,2,as.numeric)

##-------------------------- SPATIAL DATA WRANGLE ----------------------
#Read in shapefiles
Admin1<- readOGR("./www", "YEM_adm1_Governorates")
Admin2<- readOGR("./www", "YEM_adm2_Districts")

#Admin2@data<-Admin2@data[,c(4,6,7,9,10)]
Admin2@data<- Admin2@data %>% mutate_if(is.factor, as.character) 


##-------------------------- COMBINE TABULAR & SPATIAL DATA----------------------
#Merge data from Google Sheet with Rayon shp file
Rshp <- merge(x=Admin2,y=Admin2data_current, by.x="admin2pcod", by.y= "district_ID")
#Rshp@data<-Rshp@data[,c(-15,-29)] #remove date columns, was throwing errors later


DistsNumb<-sum(!is.na(Rshp@data$district_name)) #get number of districts covered...


# Reduce shapfile complexity for fast leaflet loading,  AND project
Rshp<-ms_simplify(Rshp, 0.5)
Rshp <- spTransform(x = Rshp, 
                    CRSobj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
Admin1<-ms_simplify(Admin1,0.5)
Admin1<- spTransform(x = Admin1, 
                     CRSobj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

lopt = labelOptions(noHide = TRUE,
                    direction = 'top',
                    textOnly = TRUE)

##-------------------------- CREATE MAP LABELS ----------------------
#GOVERNORATE LABELS
# Get polygons centroids
centroids <- as.data.frame(centroid(Admin1))
colnames(centroids) <- c("lon", "lat")
centroids <- data.frame("ID" = 1:nrow(centroids), centroids)

# Create SpatialPointsDataFrame object
coordinates(centroids) <- c("lon", "lat") 
proj4string(centroids) <- proj4string(Admin1) # assign projection
centroids@data <- sp::over(x = centroids, y = Admin1, returnList = FALSE)
centroids1 <- as.data.frame(centroid(Admin1))
colnames(centroids1) <- c("lon", "lat")
centroids@data<- cbind(centroids@data, centroids1)

#YEMEN LABEL
YEMl<- as.data.frame(cbind(48.5164,15.5527))
colnames(YEMl) <- c("lon", "lat")
YEMl <- data.frame("ID" = 1:nrow(YEMl), YEMl)
coordinates(YEMl) <- c("lon", "lat") 
proj4string(YEMl) <- proj4string(Admin1)
UKRl1<- as.data.frame(cbind(48.5164,15.5527))
YEMl@data<-cbind(YEMl@data, "YEMEN", UKRl1 )
colnames(YEMl@data) <- c("index","name","lon", "lat")


####THIS IS A PLAYGROUND AREA
# Admin2dd<-Admin2table[Admin2table$district_ID=='YE1806',]
# TRYIT<-14
# highchart() %>%
#   hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%b %Y')) %>%
#   hc_add_series(Admin2dd, "line", hcaes(Admin2dd$date2, Admin2dd[[TRYIT]]))%>%
#   hc_add_theme(hc_theme_gridlight())%>%
#   hc_plotOptions(line = list(
#     lineWidth=2,
#     dataLabels = list(enabled = FALSE)))%>%
# hc_tooltip(pointFormat = '{point.x: %B %Y}
#       
#       
#                             {point.y:.4f}%')
# 
# 
# 
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
#   setView(48.5164,15.5527, zoom = 6) %>%
#   addPolygons(data = Admin1,    # OBLASTS , LABELS
#               label = Admin1$admin1name,
#               options = pathOptions(clickable = TRUE),
#               
#               color = "black",
#               weight= 1,
#               fill=TRUE,
#               fillOpacity = 0.1,
#               opacity = 0.9 ) %>%
#   
#   addPolygons(data= Rshp,  # NUMBER OF IDPS
#               color = "black",
#               weight = .4,
#               opacity = 1.0,
#               smoothFactor = 0.5,
#               fill = TRUE,
#              # fillColor = ~palnumL(Rshp[[18]]),
#               fillOpacity = .8,
#               layerId = ~admin2refn,
#               highlightOptions = highlightOptions(color = "black", weight = 2,                                                     bringToFront = TRUE, sendToBack = TRUE),
#               popup = numIDP_Popup,
#               group= "Number of IDPs")
# # %>%
# #   
# #   addLegend("bottomright", pal = palnumL, values = Rshp[[18]],
# #             title = "Number of IDPs",
# #             opacity = 1) 
#   
# 

vars <- c(
  "SMEB"="SMEB",
  "Parallel Exchange Rates"="exchange_rates",
  "Petrol" = "petrol",
  "Diesel" = "diesel",
  "Bottled Water"="bottled_water",
  "Treated Water"="treated_water",
  "Soap"="soap",
  "Laundry Powder"="laundry_powder",
  "Sanitary Napkins"="sanitary_napkins",
  "Water Trucking"= "cost_cubic_meter"
)






