# this function aims to take first date and second date to generate the Status and Time needed for the survival analysis.

status_and_time <- function(dataframe, event_name, time_unit, add_time) {
  
  # use event, time_unit, and add_time to assemble the variable names
  tag_touse <- paste0(event_name, "_within_", add_time, time_unit, "_cl")
  length_name <- paste0("final_", event_name, "_time_in_", time_unit, "_cl")
  
  status_name <- paste0(event_name, "_status_at_", add_time, time_unit, "_cl")
  time_name <- paste0("time_to_", event_name, "_at_", add_time, time_unit, "_cl")
  
  censor_tag <- paste0("alldeath_within_", add_time, time_unit, "_cl")
  censor_length <- paste0("final_alldeath_time_in_", time_unit, "_cl")
  
  dataframe <- dataframe %>%
    mutate(
      !!status_name := case_when(
        get(tag_touse) == 1 ~ 1,
        get(tag_touse) == 0 ~ 0,
        is.na(get(tag_touse)) ~ 0,
      ),
      !!time_name := case_when(
        get(status_name) == 1 ~ get(length_name),
        get(status_name) == 0 & get(censor_tag) == 1 ~ get(censor_length),
        get(status_name) == 0 & get(censor_tag) == 0 ~ add_time,
        TRUE ~ add_time
      )
    )
  return(dataframe)
}

