# this script aims to provide a missing value report for any data frame

missing_report <- function(dataframe) {

"%!in%" = Negate("%in%")
      
  if ("data.frame" %!in% class(dataframe)) {
    stop("Please provide a dataframe.")
  }
  
  missing <- data.frame(number = colSums(is.na(dataframe)))
  missing$percentage <- percent(missing$number/nrow(dataframe), digits = 2)
  missing$completeness <- if_else(missing$number > 0, "incomplete", "complete")
  missing <- missing %>% arrange(desc(number))
  print(missing)
  return(missing)
}

