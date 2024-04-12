# This function serves as a beginner script to going through all the variables in a dataframe.
# report the variable name, type, report number of missing values. If categorical, report individual levels; if numeric, report basic stats; if binary, change whatever number that is not 1 to 0.
# It is very useful when you are handed a large amount of data files and want to have a quick review. Starting with generating a list of files and check through using this function.

overview <- function(name, dataframe, tag = "R") {

  # Sometimes, the data could be released in batches. If mistakes made in previous batches, you will get correct ones in later batches. The tag parameter is here to help distinguish overviews from different releases if needed. 
  
  # make a new dataframe ----
  df = data.frame()
  for (i in 1:ncol(dataframe)) {
    current_col = dataframe[, i]
    var_name = colnames(dataframe)[i]
    var_type = class(current_col)
    
    # handle missing values after empty values are dealt with ----
    no_of_missing = sum(is.na(current_col))
    
    # report var type specific summary ----
    if (var_type == "character") {
      var_levels = levels(as.factor(current_col[!is.na(current_col)]))
      no_of_var_levels = length(var_levels)
      if (no_of_var_levels <= 20) {
        # add a little code here that can report the numbers for the levels
        
        value_n_list <- c()
        for (value in var_levels) {
          n_value <- sum(current_col[!is.na(current_col)] == value)
          value_n_pair <- paste0(value, ":", n_value)
          value_n_list <- c(value_n_pair, value_n_list)
        }
        
        var_summary = paste0("the levels are: ", paste(value_n_list, collapse = ", "))
        #var_summary = paste0("the levels are: ", paste(var_levels, collapse = ", "))
      } else {
        var_summary = paste0("the variable has ", no_of_var_levels, " levels")
      }
    }
    
    # check if binary with 1 in the value, replace the other value with 0, and report in the var summary. if single value, report; if multiple values, get the summary ----
    if (var_type %in% c("numeric", "double", "integer")) {
      var_levels = levels(as.factor(current_col))
      no_of_var_levels = length(var_levels)
      # is binary number
      if (no_of_var_levels == 2 & "1" %in% var_levels) {
        value_to_change = var_levels[var_levels != "1"]
        current_col[current_col == value_to_change] <- "0"
        var_summary = paste0("the original variable has 2 levels: 1 and ", value_to_change, "; the modified varialbe has 2 levels: 1 and 0")
      }
      # is not binary number
      if (no_of_var_levels > 2) {
        var_summary = paste0("the min is: ", min(current_col, na.rm = TRUE), "; the max is: ", max(current_col, na.rm = TRUE), "; the mean is: ", mean(current_col, na.rm = TRUE), "; the median is: ", median(current_col, na.rm = TRUE))
      }
      if (no_of_var_levels < 2) {
        var_summary = paste0("the varialbe has a single level ", var_levels)
      }
    }
    
    if (var_type %in% c("POSIXct", "POSIXt")) {
      var_summary = "This is a date time variable."
    }
    
    # save the collected values into a dataframe and concatenate
    output = c(var_name, var_type, no_of_missing, var_summary)
    df = rbind(df, output)
  }
  
  colnames(df) = c("var_name", "var_type", "no_of_missing", "var_summary")
  
  write.csv(df, file = paste0("/../../research/Cass/output/csv/", name, "_overview_", tag, ".csv"))

}