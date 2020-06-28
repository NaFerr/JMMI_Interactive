


  dist_dat<-Admin2table[Admin2table$district_ID=="YE2307",] #strangely reactive objects are stored as functions


  gov_dat<-Admin1table[Admin1table$government_ID=="YE23",] #adding the government data to the dataset


  nat_dat<-AdminNatTable


  gov_nat_dat<-right_join(gov_dat,nat_dat, by = "date2")

  all_dat<-right_join(dist_dat,gov_nat_dat, by = "date2")#using a full join so that the data that was for the other month when district wasnt select is still shown
  all_dat$date<-as.yearmon(all_dat$date2)
  all_dat

  
  r<-all_dat[,c(1:5,16,17,31,43,15,30,42)]
  colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
r  
  
