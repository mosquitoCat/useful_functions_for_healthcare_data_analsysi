# this function takes a dataframe as input and check the format of the date column. If it's integer, then the column will be converted to ymd format.
shake_date <- function(dataframe, event_date) {
  type <- lapply(dataframe, class)[event_date]
  print(type)
  if (type == "integer") {
    dataframe <- dataframe %>%
      mutate(
        !!event_date := ymd(get(event_date))
      )
    print("the date column is class integer")
  }
  return(dataframe)
}