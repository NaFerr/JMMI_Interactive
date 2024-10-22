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
#

GSh<-read.csv("data/governorate_interactive.csv")%>%
  as_tibble()%>%
  dplyr::select(-X)

  Admin1data <- mutate(GSh, SMEB = as.numeric((soap*10.5+laundry_powder*20+sanitary_napkins*2+as.numeric(cost_cubic_meter)*3.15))) #The SMEB calculation
  Admin1data$SMEB<-round(Admin1data$SMEB,0)
  
  
GSh2<-read.csv("data/district_interactive.csv")%>%
  as_tibble()%>%
  dplyr::select(-X)

  Admin2data <- mutate(GSh2, SMEB = as.numeric((soap*10.5+laundry_powder*20+sanitary_napkins*2+as.numeric(cost_cubic_meter)*3.15))) #The SMEB caluclation
  Admin2data$SMEB<-round(Admin2data$SMEB,0)

GShnat<-read.csv("data/national_interactive.csv")%>%
  as_tibble()%>%
  dplyr::select(-X)

  AdminNatData<-mutate(GShnat,SMEB = as.numeric((soap*10.5+laundry_powder*20+sanitary_napkins*2+as.numeric(cost_cubic_meter)*3.15))) #The SMEB caluclation)
  AdminNatData$SMEB<-round(AdminNatData$SMEB,0)
  
max_date <- max(as.Date(as.yearmon(AdminNatData$date)))  

#Wrangle Data into appropriate formats
#Governorates
Admin1table<-as.data.frame(Admin1data)
Admin1table$date2<- as.Date(Admin1table$date, format("%d-%b-%y"), tz="UTC")
Admin1table$date2 <- as.Date(as.yearmon(Admin1table$date))

Admin1data_current <- Admin1table %>% #subset only recent month dates to attach to shapefile
  arrange(desc(date2)) %>%
  filter(date2 == max_date)
currentD <- as.character(format(max(Admin1table$date2),"%B %Y")) #define current date for disply in dashboard
Admin1table[4:14] <- sapply(Admin1table[4:14], as.numeric)

#Districts
Admin2table <- as.data.frame(Admin2data)
Admin2table$date2 <- as.Date(as.yearmon(Admin2table$date))

Admin2data_current <- Admin2table %>% #subset only recent month dates to attach to shapefile
  arrange(desc(date2))%>%
  filter(date2 == max_date)
currentD <- as.character(format(max(Admin2table$date2),"%B %Y")) #define current date for disply in dashboard
Admin2table[7:16] <- sapply(Admin2table[7:16], as.numeric)

#National
AdminNatTable<-as.data.frame(AdminNatData)
AdminNatTable$date2 <- as.Date(as.yearmon(AdminNatTable$date))

AdminNatData_current <- AdminNatTable %>% #subset only recent month dates to attach to shapefile
  arrange(desc(date2)) %>%
  filter(date2 == max_date)
currentD <- as.character(format(max(AdminNatTable$date2),"%B %Y"))
  #define current date for disply in dashboard


##-------------------------- SPATIAL DATA WRANGLE ----------------------
#Read in shapefiles
Admin1<- readOGR("./www", "YEM_adm1_Governorates")
Admin2<- readOGR("./www", "YEM_adm2_Districts")

Admin1@data$admin1name<-gsub("Amanat Al Asimah", "Sana'a City", Admin1@data$admin1name)
Admin1@data$admin1refn<-gsub("Amanat Al Asimah", "Sana'a City", Admin1@data$admin1refn)
Admin2@data$admin1name<-gsub("Amanat Al Asimah", "Sana'a City", Admin2@data$admin1name)
Admin2@data<- Admin2@data %>% mutate_if(is.factor, as.character) 


##-------------------------- COMBINE TABULAR & SPATIAL DATA----------------------
#Merge data from Google Sheet with Rayon shp file
Rshp <- merge(x=Admin2,y=Admin2data_current, by.x="admin2pcod", by.y= "district_ID")

DistsNumb<-sum(!is.na(Rshp@data$district_name)) #get number of districts covered...

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






