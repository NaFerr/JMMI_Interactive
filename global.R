#GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL

#install packages
library(devtools)
library(usethis)
library(shiny)
library(shinyjs)
library(rgdal)
library(dplyr)
library(leaflet)
library(highcharter)
library(zoo)
library(ggplot2)
library(rgeos)
library(classInt)
library(geosphere)
library(shinythemes)
library(ggplot2)
library(sf)
library(purrr)
library(shinydashboard)
library(readxl)
library(DT)
library(formattable)
library(tibble)
library(curl)
library(sp)


#library(googlesheets)
#library(googlesheets4)
#library(googledrive)
#library(gargle)

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

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}



##-------------------------- TABULAR DATA WRANGLE ----------------------
#Read in GoogleSheets
#googlesheets4::sheets_deauth()
#googlesheets4::sheets_auth_configure(.http-oauth)

#gargle::token_fetch()
#options(gargle_oauth_cache = T)
#sheets_token("https://accounts.google.com/o/oauth2/auth?client_id=603366585132-0l3n5tr582q443rnomebdeeo0156b2bc.apps.googleusercontent.com&scope=https%3A%2F%2Faccounts.google.com%2Fo%2Foauth2%2Fauth%3Fclient_id%3D603366585132-0l3n5tr582q443rnomebdeeo0156b2bc.apps.googleusercontent.com%26scope%3Dhttps%253A%252F%252Fwww.googleapis.com%252Fauth%252Fspreadsheets%2520https%253A%252F%252Fwww.googleapis.com%252Fauth%252Fuserinfo.email%26redirect_uri%3Dhttp%253A%252F%252Flocalhost%253A1410%252F%26response_type%3Dcode%26state%3Dx6Xxf9mH0V%20https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email&redirect_uri=http%3A%2F%2Flocalhost%3A1410%2F&response_type=code&state=x6Xxf9mH0V")
#options(gargle_quiet = FALSE)
#drive_auth(cache = ".secrets", 
#           email = TRUE) 
#sheets_auth(token = drive_token(),
#            scopes="https://www.googleapis.com/auth/spreadsheets.readonly")

#options(gargle_oauth_email = TRUE)
#options(sheets_user(TRUE))

#options(gargle_oauth_email = T)
#drive_auth(email = T)
#sheets_auth(email = gargle_oauth_email())
#token<-list.files(".secrets/")
 
#get the authorizations from the google sheets
#https://gargle.r-lib.org/articles/non-interactive-auth.html



# designate project-specific cache
#options(gargle_oauth_cache = ".secrets")

# check the value of the option, if you like
#gargle::gargle_oauth_cache()

# trigger auth on purpose --> store a token in the specified cache
#drive_auth()
#credentials_user_oauth2()
# see your token file in the cache, if you like
#list.files(".secrets/")


#drive_auth(cache=".secrets")

#options(
#  gargle_oauth_cache = ".secrets",
#  gargle_oauth_email = T,
#  gargle_quiet = F
#)
#options(gargle_oob_default=T)

#GSh<- read_sheet('https://docs.google.com/spreadsheets/d/1F7HEGZEe5_6sk_xrFrwg8k9VVWkpprXSC7O3SosWbjc/edit#gid=0', sheet = 1)%>%
#GSh<-read_excel("data/updated_interactive.xlsx",sheet = 2)%>%
GSh<-read.csv("data/governorate_interactive.csv")%>%
  as_tibble()%>%
  dplyr::select(-X)
#GSh[4:13]<-as.numeric(GSh[4:13])
  #GSh[4:12]<-as_tibble(as.numeric(as.character(unlist(GSh[4:12]))))#Governorates
  Admin1data <- mutate(GSh, SMEB = as.numeric((soap*10.5+laundry_powder*20+sanitary_napkins*2+as.numeric(cost_cubic_meter)*3.15))) #The SMEB calculation
  Admin1data$SMEB<-round(Admin1data$SMEB,0)
  
  
#GSh2<- read_sheet('https://docs.google.com/spreadsheets/d/1NnQNwo3FnEyayGwUk-TUeSRxV04CkiqsDuTePpbYpWs/edit#gid=0')%>% #Districts
#GSh2<-read_excel("data/updated_interactive.xlsx",sheet = 1)%>%
GSh2<-read.csv("data/district_interactive.csv")%>%
  as_tibble()%>%
  dplyr::select(-X)
#GSh2[6:15]<-as.numeric(GSh2[6:15])
#  GSh2[6:14]<-as_tibble(as.numeric(as.character(unlist(GSh2[6:14])))) #first worksheet
  Admin2data <- mutate(GSh2, SMEB = as.numeric((soap*10.5+laundry_powder*20+sanitary_napkins*2+as.numeric(cost_cubic_meter)*3.15))) #The SMEB caluclation
  Admin2data$SMEB<-round(Admin2data$SMEB,0)
  
#GShnat<-read_sheet('https://docs.google.com/spreadsheets/d/1k4CUjmjXRSRh6bm-JC8IaAAo5fuGIM-8_IzQC8hZF6c/edit#gid=0')%>%#National
#GShnat<-read_excel("data/updated_interactive.xlsx",sheet = 3)%>%
GShnat<-read.csv("data/national_interactive.csv")%>%
  as_tibble()%>%
  dplyr::select(-X)
#GShnat[2:11]<-as.numeric(GShnat[2:11])
 # GShnat[2:10]<-as_tibble(as.numeric(as.character(unlist(GShnat[2:10])))) #first worksheet
  AdminNatData<-mutate(GShnat,SMEB = as.numeric((soap*10.5+laundry_powder*20+sanitary_napkins*2+as.numeric(cost_cubic_meter)*3.15))) #The SMEB caluclation)
  AdminNatData$SMEB<-round(AdminNatData$SMEB,0)
  
max_date <- max(as.Date(as.yearmon(AdminNatData$date)))  

#Wrangle Data into appropriate formats
#Governorates
Admin1table<-as.data.frame(Admin1data)
Admin1table$date2<- as.Date(Admin1table$date, format("%d-%b-%y"), tz="UTC")
Admin1table$date2 <- as.Date(as.yearmon(Admin1table$date))
#Admin1table$date2<- as.Date(x=paste("01-",Admin1table$date, sep=""), format="%d-%b-%y") #format a date column
#Admin1table$date <- as.yearmon(Admin1table$date2)

Admin1data_current <- Admin1table %>% #subset only recent month dates to attach to shapefile
  arrange(desc(date2)) %>%
  filter(date2 == max_date)
currentD <- as.character(format(max(Admin1table$date2),"%B %Y")) #define current date for disply in dashboard
Admin1table[4:14] <- sapply(Admin1table[4:14], as.numeric)

#Districts
Admin2table <- as.data.frame(Admin2data)
#Admin2table$date2<- as.Date(Admin2table$date, format("%d-%b-%y"), tz="UTC")
#Potential way around issues with dates as NA in May 2020
#Admin2table$date2 <- (as.yearmon(Admin2table$date))
Admin2table$date2 <- as.Date(as.yearmon(Admin2table$date))
#Admin2table$date2 <- as.Date(x=paste("01-",Admin2table$date, sep=""), format="%d-%b-%y") #format a date column
#Admin2table$date <- as.yearmon(Admin2table$date2)

Admin2data_current <- Admin2table %>% #subset only recent month dates to attach to shapefile
  arrange(desc(date2))%>%
  filter(date2 == max_date)
currentD <- as.character(format(max(Admin2table$date2),"%B %Y")) #define current date for disply in dashboard
Admin2table[7:16] <- sapply(Admin2table[7:16], as.numeric)

#Admin2data_current$date <- lapply(Admin2data_current$date, as.character)

#National
AdminNatTable<-as.data.frame(AdminNatData)
#AdminNatTable$date2<- as.Date(AdminNatTable$date, format("%d-%b-%y"), tz="UTC")
AdminNatTable$date2 <- as.Date(as.yearmon(AdminNatTable$date))
#AdminNatTable$date2 <- as.Date(x=paste("01-",AdminNatTable$date, sep=""), format="%d-%b-%y")#format a date column
#AdminNatTable$date <- as.yearmon(AdminNatTable$date2)

AdminNatData_current <- AdminNatTable %>% #subset only recent month dates to attach to shapefile
  arrange(desc(date2)) %>%
  filter(date2 == max_date)
currentD <- as.character(format(max(AdminNatTable$date2),"%B %Y"))
  #define current date for disply in dashboard

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

Admin1@data$admin1name<-gsub("Amanat Al Asimah", "Sana'a City", Admin1@data$admin1name)
Admin1@data$admin1refn<-gsub("Amanat Al Asimah", "Sana'a City", Admin1@data$admin1refn)
Admin2@data$admin1name<-gsub("Amanat Al Asimah", "Sana'a City", Admin2@data$admin1name)
#Admin2@data<-Admin2@data[,c(4,6,7,9,10)]
Admin2@data<- Admin2@data %>% mutate_if(is.factor, as.character) 


##-------------------------- COMBINE TABULAR & SPATIAL DATA----------------------
#Merge data from Google Sheet with Rayon shp file
Rshp <- merge(x=Admin2,y=Admin2data_current, by.x="admin2pcod", by.y= "district_ID")

#delete extra X column in Rshp Data
#Rshp@data<-dplyr::select(Rshp@data, -X)
#Rshp@data<-Rshp@data[,c(-15,-29)] #remove date columns, was throwing errors later


DistsNumb<-sum(!is.na(Rshp@data$district_name)) #get number of districts covered...

#make sure all factors are characters.

#Rshp@data$date<-as.integer(as.character(Rshp@data$date))
# Reduce shapfile complexity for fast leaflet loading,  AND project

Rshp<-st_simplify(st_as_sf(Rshp), dTolerance = 0.5)
Rshp <- st_transform(x = Rshp, 
                     crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
Rshp<-as(Rshp,"Spatial")


Admin1<-st_simplify(st_as_sf(Admin1), dTolerance = 0.5)
Admin1<- st_transform(x = Admin1, 
                      crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
Admin1<-as(Admin1,"Spatial")

##-------------------------- CREATE MAP LABELS ----------------------
#GOVERNORATE LABELS
# Get polygons centroids
centroids <- as.data.frame(centroid(Admin1))
colnames(centroids) <- c("lon", "lat")
centroids <- data.frame("ID" = 1:nrow(centroids), centroids)

# Create SpatialPointsDataFrame object
coordinates(centroids) <- c("lon", "lat") 
proj4string(centroids) <- sp::proj4string(Admin1) # assign projection
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
  "WASH SMEB"="SMEB",
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






