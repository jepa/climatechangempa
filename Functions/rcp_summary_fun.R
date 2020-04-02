
#RCP summary function
#This should either call or take the output of the species function:
#It will run for each RCP scenario within each model. 
#This function needs Lon_Lat_DBEM loaded to work. 

rcp_summary <- function(Model=NA, RCP=NA, species_output=NA, write_output=F){
  Name = paste("/nfs/mpasandclimatechange-data/Data/Results/", Model, "_RCP", RCP, sep="")
  
  #Summarize the RCP (RCP 2.6 or RCP8.5) by time period, data type (Abd or MCP), and Measure (Mean or SD)
  summary <- species_output %>%
    group_by(INDEX, time_period, Data_Type, Measure) %>%
    mutate(n=length(unique(taxon_key))) %>% 
    summarize(total = sum(value, na.rm=TRUE),
              n=mean(n, na.rm=TRUE)) %>%
    mutate(RCP=RCP,
           Model=Model)
  #Join results of RCP output to all grid_cells (Lon_Lat_DBEM)
  RCP_DF <-  Lon_Lat_DBEM %>%
    left_join(summary)
  if(write_output==TRUE){
    write.csv(RCP_DF,
              Name,
              row.names=F)
  }
  
  return(RCP_DF)
}
