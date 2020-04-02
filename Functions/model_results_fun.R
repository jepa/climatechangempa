
# This funtion starts the analysis sequence applying all other functions needed


model_results <- function(Model=NA, #Needs a model specified (either 'Fish' or 'MPA')
                          RCP=NA, #Needs an RCP specified (either 'RCP2.6' or 'RCP8.5')
                          species_list=NA,
                          GCM=NA, #If GCM = NA, then it will run for all GCMs. 
                          year_df=NA,
                          country_prop_price = country_prop_price, #dataframe with prices for different species and proportion of catch by different countries in a grid cell 
                          Habitat = NA){ # for habitat-specific data
  
  # Added 20 to the name to sinalize 20 years avrg.
  Name <- paste("/nfs/mpasandclimatechange-data/Data/Results/", Model, "_RCP", RCP,"_", Habitat,"_All_GCMs_20.csv", sep="")

  # --------------- # 
  # Run species_function
  # --------------- # 
  species_output_raw <- lapply(species_list, species_function,
                               Model=Model, GCM=GCM, RCP=RCP)
  
  species_output <- bind_rows(species_output_raw)
  
  # --------------- # 
  # Set rcp_summary
  # --------------- # 
  model_output <- rcp_summary(Model=Model, 
                              RCP=RCP, 
                              species_output=species_output,
                              write_output = FALSE) %>% 
    mutate(Habitat = Habitat)
  
  # --------------- # 
  # Function end
  # --------------- # 
  write.csv(model_output, Name, row.names=F)
  print(paste("Model output saved for ",  Model, " ", RCP, " to ", Name, sep=""))
  return(model_output)
}
