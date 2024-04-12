# a function comparing two dates and generate a new column with the later date of the two if the dates exist. If no, return NA.

later_date <- function(dataframe, first_date, second_date) {
  
  new_colname <- paste0("later_of_", first_date, "_and_", second_date)
  
  dataframe <- dataframe %>%
    mutate(
      !!first_date := as.Date(get(first_date)),
      !!second_date := as.Date(get(second_date)),
      !!new_colname := case_when(
        !is.na(get(first_date)) & !is.na(get(second_date)) ~ pmax(get(first_date), get(second_date)),
        !is.na(get(first_date)) & is.na(get(second_date)) ~ get(first_date),
        is.na(get(first_date)) & !is.na(get(second_date)) ~ get(second_date),
        is.na(get(first_date)) & is.na(get(second_date)) ~ as.Date(NA)
      )
    )
  
  print(glue("{sum(is.na(dataframe[, new_colname]))} cases have missing later date."))
  return(dataframe)
}