# the process of getting flextable from the cox_summary generated from cox_flex_combo is too tedious, and it's worth writing a function to deal with it
cox_summary_to_flextable <- function (cox_summary_table, dataframe){
  
  # start with no subgroup situation ----
  event_list <- unique(cox_summary_table$event)
  
  if (is.null(cox_summary_table$subgroup_name)) {
    print("No subgroup detected, thus no subgroup analysis.")
    
    ## generate event number for all combinations of pathway, and outcomes
    df_list <- c()
    for (j in event_list) {
      print(j)
      filter_var = paste0(j, "_status_at_5years_cl")
      df <- dataframe %>%
        filter(get(filter_var) == 1) %>%
        count(pathway) %>%
        pivot_wider(names_from = 1, values_from = n, names_prefix = "event_number_") %>%
        mutate(event = j)
      df_list[[j]] <- df
    }
    df_list_combined <- do.call(rbind, df_list)
    #rownames(df_list_combined) <- seq(nrow(df_list_combined))
    
    ## get total number for each risk_group x pathway combination ---- 
    total_numbers <- dataframe %>%
      count(pathway) %>%
      pivot_wider(names_from = 1, values_from = n, names_prefix = "total_number_")
    total_numbers <- do.call(rbind, replicate(nrow(df_list_combined), total_numbers, simplify = FALSE))
    df_list_combined <- cbind(df_list_combined, total_numbers)
    dim(df_list_combined)
    
    ### need a bit formatting here ----
    #### love this piece here. neat. numbers checked out.
    new_df_list_combined <- df_list_combined %>%
      mutate(
        across(
          starts_with("event_number_"),
          ~ . / cur_data()[[sub("event", "total", cur_column())]],
          .names = "{sub('number', 'rate', col)}"
        ),
        across(
          starts_with("event_rate_"),
          ~ percent(.x, accuracy = 0.1)
        )
      )
    ### generate formatted event number and rate ----
    new_df_list_combined1 <- new_df_list_combined %>%
      mutate(
        across(
          starts_with("event_number_"),
          ~ paste0(., " (", cur_data()[[sub("number", "rate", cur_column())]], ")"),
          .names = "{sub('number', 'numberNrate', col)}"
        )
      )
    
    ### extract useful data from cox summary and combine with event rate ----
    table_tosave <- cox_summary_table %>%
      mutate(
        event_rate = percent(event_rate, accuracy = 0.1),
        p_value = if_else(
          p_value >= 0.001,
          as.character(round(p_value, 4)),
          "< 0.001"
        ),
        adjusted_HR_95pCIs = paste0(sprintf('%.2f', adjusted_HR), " (", sprintf('%.2f', lower_95pCI), ", ", sprintf('%.2f', upper_95pCI), ")")
      ) %>%
      left_join(., new_df_list_combined1, by = "event")
    
    ### save to csv before trimming to get the nice table ----
    write.csv(table_tosave, file = paste0(glue("../../output/csvs/Cox_stats_allevents_5year_nosubgroup.csv")))
    
    ### generate pretty table ----
    small_table_tosave <- table_tosave %>%
      select(event, 
             matches("numberNrate"), 
             matches("adjusted_HR_95pCIs"), 
             matches("p_value")) %>%
      mutate(
        event = recode(event,
                       "mi" = "myocardial infarction",
                       "cardiacdeath" = "cardiac death",
                       "cardiovasculardeath" = "cardiovascular death",
                       "alldeath" = "all-cause death",
                       "mi_cardiacdeath" = "myocardial infarction or cardiac death",
                       "mi_cardiovasculardeath" = "myocardial infarction or cardiovascular death",
                       "mi_alldeath" = "myocardial infarction or all-cause death")
      )
    ft <- flextable(small_table_tosave) 
    save_as_docx(ft, path = paste0(glue("../../output/docxs/Cox_stats_allevents_5year_nosubgroup.docx")))
    return()
  }
  
  # now with subgroup situation ----
  subgroup_list <- unique(cox_summary_table$subgroup_name)
  
  for (i in subgroup_list) {
    ## generate event number for all combinations of subgroup, pathway, and outcomes
    print(i)
    new_var <- glue("{i}_pathway")
    df_list <- c()
    for (j in event_list) {
      print(j)
      filter_var = paste0(j, "_status_at_5years_cl")
      df <- dataframe %>%
        filter(get(filter_var) == 1) %>%
        count(get(i), pathway) %>%
        setNames(c(i, "pathway", "n")) %>%
        unite("new_col", 1:2) %>%
        setNames(c(new_var, "n")) %>%
        pivot_wider(names_from = 1, values_from = n, names_prefix = "event_number_") %>%
        mutate(event = j)
      df_list[[j]] <- df
    }
    df_list_combined <- do.call(rbind, df_list)
    #rownames(df_list_combined) <- seq(nrow(df_list_combined))
    
    ## get total number for each risk_group x pathway combination
    total_numbers <- dataframe %>%
      count(get(i), pathway) %>%
      setNames(c(i, "pathway", "n")) %>%
      unite("new_col", 1:2) %>%
      pivot_wider(names_from = 1, values_from = n, names_prefix = "total_number_")
    total_numbers <- do.call(rbind, replicate(nrow(df_list_combined), total_numbers, simplify = FALSE))
    df_list_combined <- cbind(df_list_combined, total_numbers)
    dim(df_list_combined)
    
    ### need a bit formatting here ----
    #### love this piece here. neat. numbers checked out.
    new_df_list_combined <- df_list_combined %>%
      mutate(
        across(
          starts_with("event_number_"),
          ~ . / cur_data()[[sub("event", "total", cur_column())]],
          .names = "{sub('number', 'rate', col)}"
        ),
        across(
          starts_with("event_rate_"),
          ~ percent(.x, accuracy = 0.1)
        )
      )
    ### generate formatted event number and rate ----
    new_df_list_combined1 <- new_df_list_combined %>%
      mutate(
        across(
          starts_with("event_number_"),
          ~ paste0(., " (", cur_data()[[sub("number", "rate", cur_column())]], ")"),
          .names = "{sub('number', 'numberNrate', col)}"
        )
      )
    
    ### extract useful data from cox summary and combine with event rate ----
    table_tosave <- cox_summary_table %>%
      mutate(
        event_rate = percent(event_rate, accuracy = 0.1),
        p_value = if_else(
          p_value >= 0.001,
          as.character(round(p_value, 4)),
          "< 0.001"
        ),
        adjusted_HR_95pCIs = paste0(sprintf('%.2f', adjusted_HR), " (", sprintf('%.2f', lower_95pCI), ", ", sprintf('%.2f', upper_95pCI), ")")
      ) %>%
      filter(subgroup_name == i) %>%
      pivot_wider(
        names_from = subgroup_cate,
        values_from = c(total_number, event_number, adjusted_HR, lower_95pCI, upper_95pCI, p_value, event_rate, adjusted_HR_95pCIs)
      ) %>%
      left_join(., new_df_list_combined1, by = "event")
    
    ### save to csv before trimming to get the nice table ----
    write.csv(table_tosave, file = paste0(glue("../../output/csvs/Cox_stats_allevents_5year_{i}.csv")))
    
    ### generate pretty table ----
    small_table_tosave <- table_tosave %>%
      select(event, 
             matches("numberNrate"), 
             matches("adjusted_HR_95pCIs_"), 
             matches("p_value_"))
    
    similar_list <- unique(cox_summary_table[cox_summary_table$subgroup_name == i,]$subgroup_cate)
    
    name_list <- c()
    for (k in similar_list) {
      name_list[[k]] <- names(small_table_tosave)[grepl(k, names(small_table_tosave))]
    }
    
    new_colnames <- c("event", unlist(name_list, recursive = FALSE))
    
    small_table_tosave <- small_table_tosave[, new_colnames]
    
    small_table_tosave <- small_table_tosave %>%
      mutate(
        event = case_when(
          event == "mi" ~ "myocardial infarction",
          event == "cardiacdeath" ~ "cardiac death",
          event == "cardiovasculardeath" ~ "cardiovascular death",
          event == "alldeath" ~ "all-cause death",
          event == "mi_cardiacdeath" ~ "myocardial infarction or cardiac death",
          event == "mi_cardiovasculardeath" ~ "myocardial infarction or cardiovascular death",
          event == "mi_alldeath" ~ "myocardial infarction or all-cause death"
        )
      )
    
    # flextable can't deal with Unicode sequences like \u2264 in colnames
    unicode_check <- grepl("\u2264", names(small_table_tosave))
    if (TRUE %in% unicode_check) {
      names(small_table_tosave) <- gsub("\u2264", "<", names(small_table_tosave))
    } # change to less than might be better
    
    print(dim(small_table_tosave))
    
    ft <- flextable(small_table_tosave) 
    save_as_docx(ft, path = paste0(glue("../../output/docxs/Cox_stats_allevents_5year_{i}.docx")))
  } # end of subgroup list loop
} # end of whole function

