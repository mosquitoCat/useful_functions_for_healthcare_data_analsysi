# this script aims to provide a missing value report for any data frame

missing_report <- function(dataframe) {

"%!in%" = Negate("%in%")
      
  if ("data.frame" %!in% class(dataframe)) {
    stop("Please provide a dataframe.")
  }
  
  missing <- data.frame(number = colSums(is.na(dataframe)))
  missing$percentage <- format(round(missing$number/nrow(dataframe), 2), nsmall = 2)
  missing <- missing %>% arrange(desc(percentage))
  print(missing)
  return(missing)
}

