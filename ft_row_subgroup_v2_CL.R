# this function should have been part of cox_summary_toft, however as the function grows too big, I might as well take the outputs instead to write a new one to use subgroups for rows and split the dataset to save for individual events
# v2 makes it more flexible in taking on different files generated for different years

ft_row_subgroup <- function (subgroup_list, time_unit, add_time) {
  
  list_csv <- paste0(glue("../../output/csvs/Cox_stats_allevents_{add_time}{time_unit}_"), subgroup_list, ".csv")
  
  list_df <- c()
  
  for (i in list_csv) {
    
    df <- fread(i)
    
    ## get HR with CIs ----
    df1 <- df %>% 
      select(event, subgroup_name, 
             matches("adjusted_HR_95pCIs")) %>%
      pivot_longer(
        cols = starts_with("adjusted_HR_95pCIs_"),
        names_to = "subgroup_cate",
        names_pattern = "adjusted_HR_95pCIs_(.*)",
        values_to = "adjusted_HR_95pCI"
      )
    
    ## get p values ----
    df2 <- df %>% 
      select(event, subgroup_name, 
             matches("p_value_")) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(
        cols = starts_with("p_value_"),
        names_to = "subgroup_cate",
        names_pattern = "p_value_(.*)",
        values_to = "p_value"
      )
    
    ## get HRs ----
    df3 <- df %>% 
      select(event, subgroup_name, 
             matches("adjusted_HR_[A-Za-z]")) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(
        cols = starts_with("adjusted_HR_"),
        names_to = "subgroup_cate",
        names_pattern = "adjusted_HR_(.*)",
        values_to = "adjusted_HR"
      )
    
    ## get lowers ----
    df4 <- df %>% 
      select(event, subgroup_name, 
             matches("lower_95pCI")) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(
        cols = starts_with("lower_95pCI"),
        names_to = "subgroup_cate",
        names_pattern = "lower_95pCI_(.*)",
        values_to = "lower_95pCI"
      )
    
    ## get uppers ----
    df5 <- df %>% 
      select(event, subgroup_name, 
             matches("upper_95pCI")) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(
        cols = starts_with("upper_95pCI"),
        names_to = "subgroup_cate",
        names_pattern = "upper_95pCI_(.*)",
        values_to = "upper_95pCI"
      )
    
    ## get event rate ----
    df6 <- df %>% 
      select(event, subgroup_name, 
             matches("event_numberNrate_")) %>%
      pivot_longer(
        cols = starts_with("event_numberNrate_"),
        names_to = c("subgroup_cate", "pathway"),
        names_pattern = "event_numberNrate_(.*)_(.*)",
        values_to = "event_numberNrate"
      ) %>% 
      pivot_wider(
        names_from = pathway,
        values_from = event_numberNrate,
        names_prefix = "event_numberNrate"
      ) |>
      left_join(df1, by = c("event", "subgroup_name", "subgroup_cate")) |>
      left_join(df2, by = c("event", "subgroup_name", "subgroup_cate")) |>
      left_join(df3, by = c("event", "subgroup_name", "subgroup_cate")) |>
      left_join(df4, by = c("event", "subgroup_name", "subgroup_cate")) |>
      left_join(df5, by = c("event", "subgroup_name", "subgroup_cate"))
    
    ## combined all data sets ----
    list_df[[i]] <- df6
  }
  combined_df <- do.call(rbind, list_df)
  
  group_names <- combined_df %>% group_by(event) %>% group_keys(event) %>% pull(1)
  
  list_of_df <- combined_df %>%
    split(combined_df$event)
  
  for (j in 1:length(list_of_df)) {
    df <- list_of_df[[j]]
    df <- df %>%
      select(- event)
    ft <- flextable(df)
    save_as_docx(ft, path = paste0(glue("../../output/docxs/Cox_stats_allsubgroups_{add_time}{time_unit}_", group_names[j],".docx")))
    write.csv(df, file = paste0(glue("../../output/csvs/Cox_stats_allsubgroups_{add_time}{time_unit}_", group_names[j],".csv")))
  }
}








