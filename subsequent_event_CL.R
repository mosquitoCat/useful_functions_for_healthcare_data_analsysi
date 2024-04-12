# this function aim to identify the first event after a reference date
# this is built based on mi data as it's likely to have multiple events
# base reference and the event date(s) would be from the dataframe and an extra parameter to use for tuning the reference

subsequent_event <- function(dataframe, add_days) {

  # change data frame to wide form
  dataframe <- dataframe %>%
    group_by(patientid) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(order = row_number())%>%
    ungroup() |>
    pivot_wider(
      id_cols = c(patientid, discharge_touse),
      names_from = order,
      values_from = c("date")
    )
  
  # modify column names
  names(dataframe)[3:ncol(dataframe)] <- 
    paste0("date", names(dataframe)[3:ncol(dataframe)])
  
  # formatting dates
  dataframe <- dataframe %>%
    mutate(
      across(where(is.integer) & !c(patientid, discharge_touse),
             ~ ymd(.x))
    )
  
  # remove events before the discharge date + follow-up days
  dataframe <- dataframe %>%
    rowwise() %>%
    mutate(
      across(where(is.Date) & !c(patientid, discharge_touse),
             ~ if_else(
               .x > discharge_touse + add_days,
               .x,
               as.Date(NA)
             ))
    ) %>%
    ungroup()
  
  # find the first non-NA value and save it to a new column
  dataframe <- dataframe %>%
    mutate(
      subdate = as.Date(NA),
      subdate = do.call(coalesce, across(-c(patientid, discharge_touse)))
      ) %>%
    select(patientid, discharge_touse, subdate) %>%
    ungroup()

  return(dataframe)
}
