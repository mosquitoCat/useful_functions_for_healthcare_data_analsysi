# this function aims to take first date and second date to generate tags for different follow-up times
tag_and_leave <- function(dataframe, 
                            first_date, 
                            second_date, 
                            time_unit, 
                            add_time) {
  # first_date is discharge, second_date is the date_of_event
  
  event_name <- second_date %>% gsub("final_|date_cl", "", .)
  length_name <- paste0("final_", event_name, "_time_in_", time_unit, "_cl") # duration
  tag_name <- paste0(event_name, "_within_", add_time, time_unit, "_cl")
  print(event_name)
  print(length_name)
  print(tag_name)
  print(add_time)
  

  dataframe <- dataframe %>%
    mutate(
      !!first_date := as.Date(get(first_date)),
      !!second_date := as.Date(get(second_date)),
      !!length_name := case_when(
        time_unit == "days" ~ interval(get(first_date), get(second_date))/ddays(1),
        time_unit == "years" ~ interval(get(first_date), get(second_date))/dyears(1),
        time_unit == "months" ~ interval(get(first_date), get(second_date))/dmonths(1)
      ),
      !!tag_name := case_when(
        get(length_name) <= add_time ~ 1,
        get(length_name) > add_time ~ 0
      )
    )
  return(dataframe)
}
