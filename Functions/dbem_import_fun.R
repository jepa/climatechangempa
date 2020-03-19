# This function loads results from the DBEM

dbem_import <- function(TaxonKey, #Except taxon_key 
                        Year, #Expects sequence of years 
                        GCM, #Expects a GCM with numeric RCP after (e.g., GFDL26)
                        Model, #Expects a model type (e.g., "Fish" or "MPA")
                        Data_Type, #Expects either "Abd" or "Catch"
                        Path = "NA"){
  
  #### Set the Path####
  if(Model == "Fish"){
    if(Path == "NA"){
      Path <- "/nfs/mpasandclimatechange-data/Data/DBEM/Fish_Model/"
    }
    D_Path <- paste(Path,GCM,"/", #<- drobo path for 8.5
                    TaxonKey,"/",TaxonKey,Data_Type,Year,".txt",
                    sep="")
  }
  
  if(Model == "MPA"){
    if(Path == "NA"){
      Path <- "/nfs/mpasandclimatechange-data/Data/DBEM/MPA_Model/"
    }
    D_Path <- paste(Path,GCM,"/", #<- drobo path for 8.5
                    TaxonKey,"/",TaxonKey,Data_Type,Year,".txt",
                    sep="")
    
  }
  D_Path <- as.list(D_Path)
  
  #Filter this list down to actually existing files:
  acceptable_paths <- paste(Path,GCM,"/",TaxonKey,"/",
                            list.files(paste(Path,GCM,"/", TaxonKey,"/", sep="")),
                            sep="")
  D_Path_accepted <- D_Path[which(D_Path %in% acceptable_paths)]
  
  #### Importing data ####
  cur <- lapply(D_Path_accepted, FUN=fread, na.strings="NA")
  if(length(cur)>0){
    cur <- cur[sapply(cur, function(d) nrow(d) >= 1)] 
    colnames <- c("INDEX", "value")
    cur <- lapply(cur, setNames, colnames)
    df <- bind_rows(cur, .id = "column_label")
    if(nrow(df)>0){
      frame_key <- tibble(column_label=seq(1,length(unlist(Year)),1), "year"=Year) %>% 
        mutate(column_label=as.character(column_label))
      df <- left_join(df, frame_key, by="column_label") %>% select(-column_label)
      df <- df %>% mutate(Data_Type=Data_Type,
                          GCM = GCM)
    }
  } else {
    df <- tibble()
  }
  return(df)
} #Function end
