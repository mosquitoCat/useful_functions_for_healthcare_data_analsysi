# this script aims to reshape the original lab test data so that the data is grouped by patient id, arranged by Date-Time Executed, and individual test items are pivoted wider. 

lab_test_handler <- function(dataframe) {
  
  # generate date-time variable by combining Date and Time
  dataframe <- dataframe %>%
    mutate(
      datetime = ymd_hms(paste0(DateExecuted, " ", TimeExecuted))
    )
  
  # it's confusing to actually process all the items at the same time, decided to split on TestItem
  var_list <- split(dataframe, f = dataframe$TestItem)
  
  for (k in 1:length(var_list)) {
    current_var <- var_list [[k]]
    current_var_name <- names(var_list)[k]
    tidy_current_var_name <- gsub(" ", "", current_var_name)
    # duplicates survey
    duplicate <- current_var %>%
      group_by(ppid, DataLochPersonIDType, DateExecuted, TimeExecuted, GroupName, datetime, TestItem) %>%
      summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(n > 1L)
    write.csv(duplicate, file = paste0("/../../research/Cass/output/csv/", tidy_current_var_name, "_duplicate.csv"))
    
    # group by patients, then group by date, then arrange by time, and pivot_wider
    reshape_current_var <- current_var %>%
      group_by(ppid, DataLochPersonIDType, DateExecuted, TimeExecuted, GroupName, datetime, TestItem) %>%
      slice(1L) %>%
      group_by(ppid, DateExecuted, TimeExecuted) %>%
      arrange(datetime) %>%
      pivot_wider(
        names_from = TestItem,
        names_sep = "_",
        values_from = c(Value, Status, RangeMin, RangeMax)
      )
    ## remove spaces in the col names
    names(reshape_current_var) <- gsub(" ", "", names(reshape_current_var))
    write.csv(reshape_current_var, file =  paste0("/../../research/Cass/output/csv/", tidy_current_var_name, "_reshaped.csv"))
  }
  
  
  
  

  
  
  
    
  
}