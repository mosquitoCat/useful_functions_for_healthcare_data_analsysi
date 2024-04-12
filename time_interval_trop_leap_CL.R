# this script aims to generate the time difference between any two different troponin measurements
interval_trop <- function(dataframe, trop_index1, trop_index2) {
  # check available troponin time
  n_available_trop_time <- dataframe %>% select(starts_with("SpecimenCollectionDateTime_value_cl_")) %>% ncol()
  
  if (trop_index1 > n_available_trop_time) {
    stop("First troponin index does not exist in the dataset.")
  }
  
  if (trop_index2 > n_available_trop_time) {
    stop("Second troponin index does not exist in the dataset.")
  }
  
  if (trop_index1 > trop_index2) {
    stop("Please ensure trop_index1 is smaller than trop_index2.")
  }
  
  time1 <- paste0("SpecimenCollectionDateTime_value_cl_", trop_index1)
  time2 <- paste0("SpecimenCollectionDateTime_value_cl_", trop_index2)
  interval_name <- paste0("interval_trop_", trop_index1, "_and_", trop_index2)
  
  new_dataframe <- dataframe %>%
    mutate(
      !!interval_name := case_when(
        !is.na(get(time1)) & !is.na(get(time2)) ~ interval(get(time1), get(time2))/dminutes(1)
      )
    )
  
  return(new_dataframe)
  
}