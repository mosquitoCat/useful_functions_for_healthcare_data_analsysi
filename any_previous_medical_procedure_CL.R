# this script aims to add previous medical conditions provided by GP data using the admissiondatetime of the first master episode for each patient.
add_prev_procedure <- function(smr01Data, mainData, procedureName, icd10Code) {
  
  smr01Data = df_toadd1
  mainData = df20
  procedureName = "prev_PCI"
  icd10Code = paste0("K", 40:48)
  icd10Code_prep = paste(icd10Code, collapse = "|")
  
  print(paste0("The new variable name is: ", procedureName))
  print(paste0("The ICD10 code(s) used for this variable is(are): ", paste(icd10Code, collapse = ", "), "."))
  
  df <- smr01Data %>%
    filter(stringr::str_starts(code, icd10Code_prep)) %>%
    group_by(ppid) %>%
    arrange(eventdate, .by_group = TRUE) %>%
    slice_head(n = 1) %>%
    mutate(!!sym(procedureName) := ifelse(eventdate < admissiondatetime, 1, 0)) %>%
    select(ppid, !!sym(procedureName))
  
  newdf <- left_join(mainData, df, by = c("ppid")) %>%
    mutate(!!sym(procedureName) := coalesce(!!sym(procedureName), 0))
  
  newdf %>% select(ppid, !!sym(procedureName)) %>% distinct() %>% group_by(!!sym(procedureName)) %>% tally() %>% print()
  
  return(newdf)
}
