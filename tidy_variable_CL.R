# this function aims to tidy up the values of variables
tidy_variable <- function(dataframe) {
  df <- data.frame()
  for (i in 1:ncol(dataframe)) {
    
    current_col <- dataframe[, i]
    
    # change binary double values 1 and x to 1 and 0
    if (is.double(class(current_col)) & length(unique(current_col)) == 2 & (1 %in% unique(current_col))) {
      current_col[current_col != 1] <- 0
    }
    # change yes and no to 1 and 0
    if (is.character(current_col) & identical(unique(current_col), c("Yes", "No"))) {
      current_col[current_col == "Yes"] <- 1
      current_col[current_col == "No"] <- 0
    }
    
    current_col <- as.factor(current_col)
    df <- cbind(df, current_col)
  }
  return(df)
}
