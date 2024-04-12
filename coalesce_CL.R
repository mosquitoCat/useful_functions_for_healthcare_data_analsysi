# this function aims to find the first value satifies the condition
coalesce_cl <- function(threshold, direction, dataframe) {
  source("../functions/match_CL.R")
  #threshold = 0.995
  #direction = ">="
  #dataframe = final_df
  
  
  # to get the npv ----
  df <- dataframe %>%
    select(where(is.numeric), everything()) %>%
    mutate(
      across(where(is.numeric), ~ round(.x, digits = 3)) 
    ) %>%
    mutate(
      across(where(is.numeric), ~ if_else(match_CL(.x, threshold, direction = direction), .x, as.numeric(NA)))
    ) 
  #view(df)
  
  # rev the columns so I can use coalesce
  newdf <- df %>%
    select(where(is.numeric))
  
  list <- rev(colnames(newdf))
  #print(list)

  newdf <- newdf[, list]
  #view(newdf)
  
  # find the final value to use
  newdf <- newdf %>%
    mutate(
      npv_touse = do.call(coalesce, across(where(is.numeric)))
    )
  
  # to get the concentration ----
  df1 <- dataframe %>%
    select(where(is.numeric), everything()) %>%
    mutate(
      across(where(is.numeric), ~ round(.x, digits = 3)) 
    ) %>%
    mutate(
      across(where(is.numeric), ~ if_else(match_CL(.x, threshold, direction = direction), as.numeric(gsub("<", "", cur_column())), as.numeric(NA)))
    ) 
  #view(df1)
  
  # rev the columns so I can use coalesce
  newdf1 <- df1 %>%
    select(where(is.numeric))
  
  list1 <- rev(colnames(newdf1))
  #print(list1)
  
  newdf1 <- newdf1[, list1]
  #view(newdf1)
  
  # find the final value to use
  newdf1 <- newdf1 %>%
    mutate(
      conc_touse = do.call(coalesce, across(where(is.numeric)))
    )
  
  combined_df <- cbind(dataframe, newdf$npv_touse, newdf1$conc_touse)
  # view(combined_df)
  colnames(combined_df)[(ncol(combined_df) - 1): ncol(combined_df)] <- c("npv_touse", "conc_touse")

  return(combined_df)
}
