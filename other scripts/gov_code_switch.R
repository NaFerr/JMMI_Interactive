gov_code_switch<-function(data_col){

  gov_codes_data <- read_excel("other scripts/pcodes/gov_codes.xlsx")
  gov_codes_data <- as.data.frame(gov_codes_data)
  
  for(i in 1:nrow((gov_codes_data[1]))){
   
    k<-which(gov_codes_data[1]==gov_codes_data[i,1])
    data_col<-gsub(gov_codes_data[i,1],gov_codes_data[k,2],data_col)
  return(data_col) 
    
  }
  return(data_col)
}
