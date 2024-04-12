# a function which check whether an event occurs before, on, or after a certain date
# event_date and reference_date are names of the variables
# calculate the absolute time difference in days

when_event_occurs <- function(dataframe, event_date, reference_date, day) {
  
  new_colname <- paste0(event_date, "_vs_", day, "days_after_",reference_date)
  abs_time_difference <- paste0("abs_time_difference_", event_date, "_vs_", reference_date)
  
  dataframe <- dataframe %>%
    mutate(
      !!event_date := as.Date(get(event_date)),
      !!reference_date := as.Date(get(reference_date)),
      !!new_colname := case_when(
        !is.na(get(event_date)) & !is.na(get(reference_date)) & get(event_date) < get(reference_date) + day ~ "before reference",
        !is.na(get(event_date)) & !is.na(get(reference_date)) & get(event_date) == get(reference_date) + day ~ "same day",
        !is.na(get(event_date)) & !is.na(get(reference_date)) & get(event_date) > get(reference_date) + day ~ "after reference"
      ),
      !!abs_time_difference := case_when(
        !is.na(get(event_date)) & !is.na(get(reference_date)) ~ abs(interval(get(reference_date), get(event_date))/ddays(1)),
        is.na(get(event_date)) & !is.na(get(reference_date)) ~ NA,
        !is.na(get(event_date)) & is.na(get(reference_date)) ~ NA
      )
    )
  
  print(as_tibble(table(dataframe[, new_colname])))
  return(dataframe)
}


