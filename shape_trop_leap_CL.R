# this script aims to grab dataframe with pivoted wide troponin, generate the index where valid troponin values were found. These indexes will be used to retrieve the valid SpecimencollectionDataTime and troponin values
# if not specified, retrieving all valid troponin values

index_trop <- function(dataframe, n_trop_retrieve = n_trop_available) {
  
  n_trop_available <- dataframe %>% select(starts_with("troponin_")) %>% ncol() # "troponin_1", "troponin_2", ..., "troponin_x"
  
  # only work with the troponin data 
  sub_dataframe <- dataframe %>% select(starts_with("troponin_"))
  #dim(sub_troponin); View(sub_troponin)
  
  # generate position dataframe
  position_df <- data.frame()
  
  for (j in 1:nrow(sub_dataframe)) {
    position_list <- c()
    for (i in 1:n_trop_available) {
      current_trop <- sub_dataframe[j, i]
      
      if (!is.na(current_trop)) {
        position_list <- c(position_list, i)
      }
      
      if (length(position_list) == n_trop_retrieve) {
        break
      }
      
      if (i == n_trop_available & length(position_list) < n_trop_retrieve) {
        n_na_toadd <- n_trop_retrieve - length(position_list)
        na_toadd <- rep(NA, n_na_toadd)
        position_list <- c(position_list, na_toadd)
      }
    }
    position_df <- rbind(position_df, position_list)
  }
  
  colnames(position_df) <- paste0("position", 1:n_trop_retrieve)
  #dim(position_df); View(position_df)
  
  return(position_df)
}