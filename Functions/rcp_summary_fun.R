
#RCP summary function
#This should either call or take the output of the species function:
#It will run for each RCP scenario within each model. 
#This function needs Lon_Lat_DBEM loaded to work. 

rcp_summary <- function(Model=NA, RCP=NA, species_output=NA, write_output=F){
  Name = paste("/nfs/mpasandclimatechange-data/Data/Results/", Model, "_RCP", RCP, sep="")
  
  summary <- species_output %>%
    group_by(INDEX, time_period, Data_Type, Measure) %>%
    mutate(n=length(unique(taxon_key))) %>% 
    summarize(total = sum(value, na.rm=TRUE),
              n=mean(n, na.rm=TRUE)) %>%
    mutate(RCP=RCP,
           Model=Model)
  RCP_DF <-  Lon_Lat_DBEM %>%
    left_join(summary)
  if(write_output==TRUE){
    write.csv(RCP_DF,
              Name,
              row.names=F)
  }
  
  return(RCP_DF)
}
