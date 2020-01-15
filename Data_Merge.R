rm(list=ls())

library(readxl)
library(dplyr)
library(tidyr)
library(plyr)
library(purrr)
library(lubridate)
library(tidyselect)
library(qpcR)
library(stringr)
library(reachR)
library(zoo)
library(googlesheets)
library(openxlsx)

#for the server
#setwd("Z:/")
#for the dropbox
setwd("C:/Users/REACH_AO_YEMEN/Dropbox/REACH/YEM/YEM Assessment/YEM Cash and Markets/02_JMMI/4. Datasets and Data Analysis/Datasets/")

#All data saved on the z drive
April_2018 <- read_excel("2.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_April2018_recode_cols.xlsx",sheet = 2)
May_2018 <- read_excel("3.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_May2018_recode_cols.xlsx",sheet = 2)
June_2018 <- read_excel("4.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_June2018_recode_cols.xlsx", sheet=2)
July_2018 <- read_excel("5.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_July2018_recode_cols.xlsx", sheet=2)
August_2018 <-read_excel("6.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_August2018_recode_cols.xlsx", sheet=2)
September_2018 <-read_excel("7.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_September2018_recode_cols.xlsx", sheet=2)
October_2018 <-read_excel("8.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_October2018_recode_cols.xlsx", sheet=2)
November_2018 <-read_excel("9.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_November2018_recode_cols.xlsx", sheet=2)
December_2018 <-read_excel("10.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_December2018_recode_cols.xlsx", sheet=2)
January_2019 <-read_excel("11.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_January2019_recode_cols.xlsx", sheet=2)
February_2019<-read_excel("12.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_February2019_recode_cols.xlsx", sheet=2)
March_2019<-read_excel("13.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_March2019_recode_cols .xlsx", sheet=2)
April_2019<-read_excel("14.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_April2019_recode_cols.xlsx", sheet=2)
May_2019<-read_excel("15. REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_May2019.xlsx", sheet=2)
June_2019<-read_excel("16.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_June2019.xlsx", sheet = 2)
July_2019<-read_excel("17.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_July2019.xlsx", sheet = 2)
August_2019<-read_excel("18.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_August2019.xlsx", sheet = 2)
September_2019<-read_excel("19.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_September2019.xlsx", sheet = 2)
October_2019<-read_excel("20.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_October2019.xlsx", sheet = 2)
November_2019<-read_excel("21.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_November2019.xlsx", sheet = 2)


list_df = setNames(lapply(ls(), function(x) get(x)), ls())
list_df_names <- names(list_df)

col_name_initial<-c(colnames(April_2018%>% dplyr::select(starts_with('calc_price_'),contains("cost_cubic_metere"), contains("exchange_rate_result"),starts_with("governorate_"),starts_with("district_"),-contains('market'))))
data_all_JMMI<-as_tibble(data.frame(test="TEST"))

data_all_JMMI[,col_name_initial] <- NA

colnames_pulled_all<-as_tibble(data.frame(JMMI="TEST"))

name_object<-function(df){
  name<-deparse(substitute((df)))
  return(name)
}

#https://stackoverflow.com/questions/37360009/binding-values-from-function-to-global-environment-in-r
col_pull<-function(df, list_of_df){
  #name<- names(list_of_df[df])
 
  call1 <-  sys.call(1)
  call1[[1]] <- as.name("names")
  call1 <- call1[1:2]
  nm <- eval(call1)
  name<-nm[!is.na(match(list_of_df,list(df)))]
  
  colnames(df)<-tolower(colnames(df))
  colnames(df)<-gsub("_all","",colnames(df))
  
  df1<-df%>%
    as_tibble()%>%
    dplyr::select(starts_with('calc_price_'),contains("cost_cubic_meter"), contains("exchange_rate_result"),starts_with("governorate_"),starts_with("district_"))%>%
    #rename(replace=c(colnames(df)=( gsub("_normalized", "_normalised", colnames(df) ) )  ))%>%
    mutate(as_tibble(),jmmi = name)%>%
    map_if(is.factor,as.character)%>%
    as_tibble()
  
      #colnames(df1)<-gsub(".*/","",colnames(df1))
    
    colname_pull<-as_tibble(data.frame( holder = colnames(df1)))
    names(colname_pull)<-name
    colnames_pulled_all<<-as_tibble(rowr::cbind.fill(colnames_pulled_all, colname_pull, fill=NA ))
  
    data_all_JMMI <<- as_tibble(merge(df1,data_all_JMMI,all=T))
  
  #return(data_all_JMMI)
}

#debug(col_pull)
#col_pull(November_2019)
lapply((list_df), col_pull, list_of_df = list_df)

#delete the test column
data_all_JMMI<-dplyr::select(data_all_JMMI,-c(test, district_name, governorate_name))

total<-0
count_nrows<-function(df){
  nrow_count<-as.numeric(nrow(df))
  total<<-as.numeric(nrow_count+total)
  return(total)
}

lapply(list_df,count_nrows)

#once you have the entire dataset created
#https://stackoverflow.com/questions/35366803/how-to-use-gsub-for-the-list-of-multiple-patterns-in-r
#make all the column numeric that are 
#https://stackoverflow.com/questions/25391975/grepl-in-r-to-find-matches-to-any-of-a-list-of-character-strings
#https://stackoverflow.com/questions/38695967/how-to-convert-certain-columns-only-to-numeric
#https://stackoverflow.com/questions/33061116/how-to-convert-a-column-of-date-string-to-dates-in-r
toMatch <- c("calc" ,"exchange","cost")
col_to_numeric<-unique(grep(paste(toMatch, collapse = "|"), colnames(data_all_JMMI),value = T))
data_all_JMMI[col_to_numeric] <- sapply(data_all_JMMI[col_to_numeric], as.numeric)

#substitute out the pcodes to standardize the name (taken from JMMI scripting, with csv (utf-8) sheet)
this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)
source("./other scripts/add_pcodes.R")

#debug(add.pcodes)
data_all_JMMI<-add.pcodes(data_all_JMMI)



################then begin the ananlysis of the files######################

#get rid of all districts that have less than three observation
data_all_JMMI <- data_all_JMMI %>%
                        dplyr::group_by(jmmi)%>%
                        dplyr::group_by(district_id)%>%
                        filter(n()>2)%>%
                        as_tibble()

#make the JMMI column act as a date column and begin to sort by that, will be important for when you want to do that national by the previous month
data_all_JMMI$jmmi<-gsub("_"," ", data_all_JMMI$jmmi)
#this is the actual date that will be sorted by with in the server script
data_all_JMMI$jmmi_date <- as.character(as.Date(as.yearmon(as.character(data_all_JMMI$jmmi))))
date_list<-sort(unique(data_all_JMMI$jmmi_date))

#add a country ID to sort the national by (because aggregate_median needs a key column code)
data_all_JMMI$country_id<-"YE"

for(i in seq_along(date_list)){
  if (i ==1){
    df1<-data_all_JMMI%>%
      filter(jmmi_date==date_list[i])
      
    district_all<-df1%>%
      aggregate_median("district_id")
    
    district_obs<-df1%>%
      dplyr::select("district_id","jmmi","jmmi_date")%>%
      dplyr::count(district_id, jmmi)
      
    governorate_all<-df1%>%
      aggregate_median("governorate_id")
    
    governorate_obs<-df1%>%
      dplyr::select("governorate_id","jmmi","jmmi_date")%>%
      dplyr::count(governorate_id, jmmi)
    
    national_all<-df1%>%
      aggregate_median("country_id")
    
    national_obs<-df1%>%
      dplyr::select("country_id","jmmi","jmmi_date")%>%
      dplyr::count(country_id, jmmi)
    
      
  }else{
    df1<-data_all_JMMI%>%
      filter(jmmi_date==date_list[i])
    
    df0<-data_all_JMMI%>%
      filter(jmmi_date==date_list[i-1])
    
    district_all<-df1%>%
      aggregate_median("district_id")%>%
      bind_rows(district_all)
    
    district_obs<-df1%>%
      dplyr::select("district_id","jmmi","jmmi_date")%>%
      dplyr::count(district_id,jmmi)%>%
      bind_rows(district_obs)
    
    governorate_all<-df1%>%
      aggregate_median("governorate_id")%>%
      bind_rows(governorate_all)
    
    governorate_obs<-df1%>%
      dplyr::select("governorate_id","jmmi","jmmi_date")%>%
      dplyr::count(governorate_id,jmmi)%>%
      bind_rows(governorate_obs)
    
    df0_pull<-unique(df0$district_id)
    df_dist<-subset(df1, district_id %in% df0_pull)
    
    national_all<-df_dist%>%
      aggregate_median("country_id")%>%
      bind_rows(national_all)
    
    national_obs<-df_dist%>%
      dplyr::select("country_id","jmmi","jmmi_date")%>%
      dplyr::count(country_id, jmmi)%>%
      bind_rows(national_obs)
    
    print(date_list[i])
  }
}

#join the observations and the values

district_final<-dplyr::full_join(district_all,district_obs, by = c("district_id", "jmmi"))
governorate_final<-dplyr::full_join(governorate_all,governorate_obs, by = c("governorate_id", "jmmi"))
national_final<-dplyr::full_join(national_all,national_obs, by = c("country_id", "jmmi"))


#reorder the variables to fit with the google sheets templates
#http://www.sthda.com/english/wiki/reordering-data-frame-columns-in-r
#http://rprogramming.net/rename-columns-in-r/

district_final<-district_final[,c("jmmi_date","governorate_name","governorate_id","district_name","district_id","calc_price_petrol","calc_price_diesel","calc_price_bottled_water","calc_price_treated_water","calc_price_soap","calc_price_laundry","calc_price_sanitary","cost_cubic_meter","exchange_rate_result","n")]
colnames(district_final)<-c("date","government_name","government_ID","district_name","district_ID","petrol","diesel","bottled_water","treated_water","soap","laundry_powder","sanitary_napkins","cost_cubic_meter","exchange_rates","num_obs")

governorate_final<-governorate_final[,c("jmmi_date","governorate_name","governorate_id","calc_price_petrol","calc_price_diesel","calc_price_bottled_water","calc_price_treated_water","calc_price_soap","calc_price_laundry","calc_price_sanitary","cost_cubic_meter","exchange_rate_result","n")]
colnames(governorate_final)<-c("date","government_name","government_ID","petrol","diesel","bottled_water","treated_water","soap","laundry_powder","sanitary_napkins","cost_cubic_meter","exchange_rates","num_obs")

national_final<-national_final[,c("jmmi_date","calc_price_petrol","calc_price_diesel","calc_price_bottled_water","calc_price_treated_water","calc_price_soap","calc_price_laundry","calc_price_sanitary","cost_cubic_meter","exchange_rate_result","n")]
colnames(national_final)<-c("date","petrol","diesel","bottled_water","treated_water","soap","laundry_powder","sanitary_napkins","cost_cubic_meter","exchange_rates","num_obs")

final_list<-list("District"=district_final,"Governorate" = governorate_final, "National" = national_final)

this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)

write.xlsx(final_list, file = "./data/updated_interactive.xlsx")

#set up google sheets dynamically
#GS_dist<- gs_key('1NnQNwo3FnEyayGwUk-TUeSRxV04CkiqsDuTePpbYpWs') #Districts
#GS_gov<-read_sheet('https://docs.google.com/spreadsheets/d/1F7HEGZEe5_6sk_xrFrwg8k9VVWkpprXSC7O3SosWbjc/edit#gid=0') #Governorates
#GS_nat<-gs_key('1Ct9OGhvc6HkuVwDdsukVNthH31fNxxBZmtQrhlVyD7w')#National

#gs_edit_cells(GS_dist, ws='Sheet1', input=colnames(district_final),byrow=T, anchor = "A1")
#gs_edit_cells(GS_dist, ws='Sheet1', input=district_final,col_names = F,trim=T)

#gs_edit_cells(GS_gov, ws='Sheet1', input=colnames(governorate_final),byrow=T, anchor = "A1")
#gs_edit_cells(GS_gov, ws='Sheet1', input=governorate_final,anchor="A2",col_names = F,trim=T)

#gs_edit_cells(GS_nat, ws='Sheet1', input=colnames(national_final),byrow=T, anchor = "A1")
#gs_edit_cells(GS_nat, ws='Sheet1', input=national_final,col_names = F,trim=T)
