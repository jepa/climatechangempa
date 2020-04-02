
# Species function
# Species is a single TaxonKey (will be applied over all exploited species)
# GCM is designed to be NA if running for all GCMs
# Year is a sequence of years. Can be all years (1995 to 2100) or the specific three periods we are looking at:
# This function needs country_prop_price loaded to work. 
# Model is Fish or MPA model
# Data_Type is DBEM abundance or MCP


species_function <- function(Model=NA, TaxonKey=NA, GCM=NA, RCP=NA, Year=NA){
  
  
  s <- Sys.time()
  
  Years <- year_df %>% pull(year)
  if(is.na(GCM)){
    gfdl_abd <- dbem_Import(TaxonKey,
                            Path= paste("/nfs/mpasandclimatechange-data/Data/DBEM/", Model, "_Model/", sep=""),
                            Years,
                            GCM = paste("GFDL", RCP, sep=""),
                            Model = Model,
                            Data_Type= "Abd"
    )
    
    mpi_abd <- dbem_Import(TaxonKey,
                           Path= paste("/nfs/mpasandclimatechange-data/Data/DBEM/", Model, "_Model/", sep=""),
                           Years,
                           GCM = paste("MPI", RCP, sep=""),
                           Model = Model,
                           Data_Type= "Abd"
    )
    ipsl_abd <- dbem_Import(TaxonKey,
                            Path= paste("/nfs/mpasandclimatechange-data/Data/DBEM/", Model, "_Model/", sep=""),
                            Years,
                            GCM = paste("IPSL", RCP, sep=""),
                            Model = Model,
                            Data_Type= "Abd"
    )
    gfdl_catch <- dbem_Import(TaxonKey,
                              Path= paste("/nfs/mpasandclimatechange-data/Data/DBEM/", Model, "_Model/", sep=""),
                              Years,
                              GCM = paste("GFDL", RCP, sep=""),
                              Model = Model,
                              Data_Type= "Catch"
    )
    mpi_catch <- dbem_Import(TaxonKey,
                             Path= paste("/nfs/mpasandclimatechange-data/Data/DBEM/", Model, "_Model/", sep=""),
                             Years,
                             GCM = paste("MPI", RCP, sep=""),
                             Model = Model,
                             Data_Type= "Catch"
    )
    ipsl_catch <- dbem_Import(TaxonKey,
                              Path= paste("/nfs/mpasandclimatechange-data/Data/DBEM/", Model, "_Model/", sep=""),
                              Years,
                              GCM = paste("IPSL", RCP, sep=""),
                              Model = Model,
                              Data_Type= "Catch"
    )
    
    raw_species_data <-  bind_rows(gfdl_abd, gfdl_catch, mpi_abd, mpi_catch, ipsl_abd, ipsl_catch)
    
  }else{
    
    gcm_abd <- dbem_Import(TaxonKey,
                           Path= paste("/nfs/mpasandclimatechange-data/Data/DBEM/", Model, "_Model/", sep=""),
                           Years,
                           GCM = paste(GCM, RCP, sep=""),
                           Model = Model,
                           Data_Type= "Abd"
    )
    gcm_catch <- dbem_Import(TaxonKey,
                             Path= paste("/nfs/mpasandclimatechange-data/Data/DBEM/", Model, "_Model/", sep=""),
                             Years,
                             GCM = paste(GCM, RCP, sep=""),
                             Model = Model,
                             Data_Type= "Catch"
    )
    
    raw_species_data <-  bind_rows(gcm_abd, gcm_catch) 
    # print("Single GCM specified, code not written yet.")
  }

  
  raw_species_data <- raw_species_data %>% 
    mutate(year = as.numeric(year))
  
  #Create blank_frame to allow all GCMs to match to all unique cells and years we have data for that species
  #Needed to properly estimate standard deviations
  cells <- unique(raw_species_data$INDEX)
  blank_frame <- expand.grid(INDEX=cells, year=as.numeric(Years), GCM= as.character(paste(c("GFDL", "IPSL", "MPI"), RCP, sep="")), Data_Type=c("Catch", "Abd"))
  blank_frame_full <- as_tibble(blank_frame)
  blank_frame_full <- blank_frame_full %>% 
    mutate(Data_Type=as.character(Data_Type),
           GCM = as.character(GCM))
  
  species_data <- left_join(blank_frame_full,
                            raw_species_data, by = c("INDEX", "year", "GCM", "Data_Type")) %>% 
    mutate(value = if_else(is.na(value), 0, value)) 
  
  
  paste("Loaded species data after", Sys.time()-s) #Progress update
  
  
  Mean_Spp <- species_data %>%
    left_join(year_df) %>%
    group_by(INDEX, time_period, GCM, Data_Type) %>%
    summarize(Mean = mean(value,na.rm=T)) %>% 
    ungroup() %>%
    filter(is.na(time_period)==FALSE) 
  
  # Step 2. Average MCP of all models ####
  Average <- Mean_Spp %>%
    group_by(INDEX, time_period, Data_Type) %>%
    summarize(SD = sd(Mean, na.rm = T),
              Mean = mean(Mean, na.rm = T)
    ) %>% 
    mutate(SD = if_else(is.na(SD), 0, SD)) %>% 
    ungroup()
  
  final_ecology <- Average %>% 
    gather("Measure", "value", -INDEX:-Data_Type) %>% 
    mutate(taxon_key = as.character(TaxonKey))
  
  #### CHECKPOINT- Step 2 ####
  # Average should be a dataset for the world with the mean of all models for each grid (Check)
  # Check that the NA's are zeros and numeric for futher addition (Check)
  ###_________________ END OF CHECKPOINT _______________ ###
  
  paste("Finished ecology after", Sys.time()-s)
  
  ### End of Step 1 Ecology ####
  
  present_catch <- sum(final_ecology %>% 
                         filter(Measure=="Mean", Data_Type=="Catch", time_period=="Today") %>%
                         pull(value))
  mid_catch <- sum(final_ecology %>% 
                     filter(Measure=="Mean", Data_Type=="Catch", time_period=="Mid Century") %>%
                     pull(value))
  mid_catch <- ((mid_catch-present_catch)/present_catch)
  end_catch <- sum(final_ecology %>% 
                     filter(Measure=="Mean", Data_Type=="Catch", time_period=="End of Century") %>%
                     pull(value))
  end_catch <- ((end_catch-present_catch)/present_catch)
  
  adj_catch <- tibble(taxon_key=as.character(TaxonKey),
                      time_period=c("Today", "Mid Century", "End of Century"),
                      new_catch = c(0, mid_catch, end_catch))
  
  # Step 2. Economic calculations 
  country_prop_species_price <- country_prop_price %>% 
    ungroup() %>% 
    filter(taxon_key==TaxonKey) %>% 
    left_join(adj_catch, by = "taxon_key") %>% 
    mutate(nonuse_value = NA,
           revenue = NA) %>% 
    mutate(price = price*(((-1 * flexibility)*new_catch)+1)) %>% 
    select(-new_catch)
  paste("Calculated elasticity after ", Sys.time()-s)
  
  partial_economic <- final_ecology %>% 
    left_join(country_prop_species_price)
  
  #If there is no price for that taxon at all, it takes the 1496 value (theoretical mean fish price in FERU data). 
  partial_economic$price <- replace_na(partial_economic$price, 1496)
  
  final_economic <- partial_economic %>% 
    mutate(nonuse_value= NA, 
           revenue = NA) %>% 
    mutate(nonuse_value = if_else(Data_Type=="Abd", price * value, 0),
           revenue = if_else(Data_Type=="Catch", price * value, 0)) %>% 
    select(-c(price, Data_Type, value)) %>% 
    gather("Data_Type", "value", -INDEX:-taxon_key) %>% 
    filter(value != 0)
  
  paste("Finished econ after", Sys.time()-s)
  
  output <- bind_rows (final_ecology, final_economic)
  
  #### CHECKPOINT- Step 3 and NoSpeciesError ####
  # Final_Dataset should be the addition of each species 10 years mean of all models
  # Manually checked that each model was in fact added for each gridcell using 600243 and 600244
  # Cells that have value for each species are added as well as cells that have only one value (for one spp) 
  
  # Error check should jump those species in the list that do not have data. (Checked)
  ###_________________ END OF CHECKPOINT _______________ ###
  
  
  print(paste("Biological analysis done for",TaxonKey))
  
  return(output)
}
