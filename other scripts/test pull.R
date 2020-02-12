


  dist_dat<-Admin2table[Admin2table$district_ID=="YE2307",] #strangely reactive objects are stored as functions


  gov_dat<-Admin1table[Admin1table$government_ID=="YE23",] #adding the government data to the dataset


  nat_dat<-AdminNatTable


  gov_nat_dat<-right_join(gov_dat,nat_dat, by = "date2")

  all_dat<-right_join(dist_dat,gov_nat_dat, by = "date2")#using a full join so that the data that was for the other month when district wasnt select is still shown
  all_dat$date<-as.yearmon(all_dat$date2)
  all_dat
