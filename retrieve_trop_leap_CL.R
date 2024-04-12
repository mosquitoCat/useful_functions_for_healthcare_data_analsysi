# this script aims to retrieve the specimencollectiontime and the value of valid troponins - basically have a version where valid values are in consecutive columns labelled by ordered nubmers
retrieve_trop <- function(dataframe) {
  start <- Sys.time()
  
  n_available_position <- dataframe %>% select(starts_with("position")) %>% ncol()
  #n_available_position <- 3
  
  # loop through all positions
  for (i in 1:n_available_position) {
    
    # prepare variables on both sides
    position <- paste0("position", i)
    trop <- paste0("troponin_{position", i, "}")
    datetime <- paste0("SpecimenCollectionDateTime_{position", i, "}")
    
    trop_name <- paste0("trop_cl_", i)
    trop_value_name <- paste0("trop_value_cl_", i)
    
    datetime_name <- paste0("SpecimenCollectionDateTime_cl_", i)
    datetime_value_name <- paste0("SpecimenCollectionDateTime_value_cl_", i)
    
    # construct the mutation part
    dataframe <- dataframe %>%
      mutate(
        !!trop_name := if_else(
          !is.na(get(position)),
          glue(trop), NA),
        
        !!trop_value_name := apply(
          across(starts_with("troponin_"), ~ case_when(get(trop_name) == cur_column() ~ .)),
                 1, function(x) na.omit(x)[1]
        ),
        
        !!datetime_name := if_else(
          !is.na(get(position)),
          glue(datetime), NA),
        
        !!datetime_value_name := apply(
          across(starts_with("SpecimenCollectionDateTime_"), ~ case_when(get(datetime_name) == cur_column() ~ .)),
                 1, function(x) na.omit(x)[1]
        )
      )
  }
  
  print(Sys.time() - start)
  dim(dataframe)
  #View(dataframe)
  return(dataframe)
}