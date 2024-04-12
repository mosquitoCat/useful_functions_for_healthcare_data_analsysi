# this script aims to create previous medical conditions provided by GP data using the admissiondatetime of the first master episode for each patient.
add_prev_condition <- function(gpData, phenotypeName, newvariableName) {
  
  print(paste0("The new variable name is: ", newvariableName))
  print(paste0("The phenotype name(s) used for this variable is(are): ", paste(phenotypeName, collapse = ", "), "."))
  
  names(gpData) <- names(gpData) %>% tolower()
  
  df <- gpData %>%
    filter(phenotype_name %in% phenotypeName) %>%
    group_by(ppid) %>%
    arrange(eventdate, .by_group = TRUE) %>%
    slice_head(n = 1) %>%
    mutate(!!sym(newvariableName) := ifelse(eventdate < masterepisodedatetime, 1, 0)) %>%
    select(ppid, !!sym(newvariableName))
  
  df %>% select(ppid, !!sym(newvariableName)) %>% distinct() %>% group_by(!!sym(newvariableName)) %>% tally() %>% print()
  
  return(df)
}
