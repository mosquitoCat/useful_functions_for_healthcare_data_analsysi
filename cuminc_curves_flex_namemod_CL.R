# this function aims to generate cuminc curves using existing tags
# need to make this neater

cuminc_flex_namemod <- function(dataframe, event_name, time_unit, add_time, factor, ylim_touse, subgroup = NULL, notable = TRUE) {
  
  status_name <- paste0(event_name, "_status_at_", add_time, time_unit, "_cl")
  print(status_name)
  time_name <- paste0("time_to_", event_name, "_at_", add_time, time_unit, "_cl")
  print(time_name)
  
  formula <- glue("Surv({time_name}, as.factor({status_name})) ~ {factor}")
  
  if (event_name == "mi") {ylabel_name = "Myocardial infarction"} 
  if (event_name == "cardiacdeath") {ylabel_name = "Cardiac death"}
  if (event_name == "cardiovasculardeath") {ylabel_name = "Cardiovascular death"} 
  if (event_name == "noncardiovasculardeath") {ylabel_name = "Non-cardiovascular death"} 
  if (event_name == "alldeath") {ylabel_name = "All death"} 
  if (event_name == "mi_cardiacdeath") {ylabel_name = "Myocardial infarction or cardiac death"} 
  if (event_name == "mi_cardiovasculardeath") {ylabel_name = "Myocardial infarction or cardiovascular death"} 
  if (event_name == "mi_noncardiovasculardeath") {ylabel_name = "Myocardial infarction or non-cardiovascular death"} 
  if (event_name == "mi_alldeath") {ylabel_name = "Myocardial infarction or all death"} 
  
  # no subgroup yes table ----
  if (is.null(subgroup) & notable == FALSE) {
    fig <- cuminc(as.formula(formula), data = dataframe) %>%
      ggcuminc() +
      add_confidence_interval() +
      add_risktable() +
      coord_cartesian(xlim = c(0, add_time)) +
      coord_cartesian(ylim = c(0, ylim_touse)) +
      scale_y_continuous(
        labels = scales::percent) +
      scale_x_continuous(
        #n.breaks = 10) +
        n.breaks = 5) +
      scale_color_manual(
        values = c("blue", "red")) +
      scale_fill_manual(
        values = c("blue", "red")) +
      theme_bw() +
      theme(plot.title = element_text(size = 11, hjust = 0.5)) +
      #theme(legend.position = "bottom", legend.box = "horizontal") +
      theme(legend.position = c(0.25, 0.85), legend.box = "vertical", legend.box.background = element_rect(fill = "transparent", color = "transparent")) +
      theme(text = element_text(size = 10)) +
      labs(
        title = "Overall",
        #y = paste0("Cumulative incidence of \n ", event_name),
        y = paste0("Cumulative incidence of \n ", ylabel_name),
        x = paste0(time_unit, " since discharge")
      )
    
    file_name_touse <- paste0("../../output/graphs/cuminc_", add_time, "_", time_unit, "_", event_name, ".png")
    save_plot(filename = file_name_touse, fig, dpi = 1200, base_height = 6, base_width = 5)
  }
  
  # no subgroup no table ----
  if (is.null(subgroup) & notable == TRUE) {
    fig <- cuminc(as.formula(formula), data = dataframe) %>%
      ggcuminc() +
      add_confidence_interval() +
      coord_cartesian(xlim = c(0, add_time)) +
      coord_cartesian(ylim = c(0, ylim_touse)) +
      scale_y_continuous(
        labels = scales::percent) +
      scale_x_continuous(
        #n.breaks = 10) +
        n.breaks = 5) +
      scale_color_manual(
        values = c("blue", "red")) +
      scale_fill_manual(
        values = c("blue", "red")) +
      theme_bw() +
      theme(plot.title = element_text(size = 11, hjust = 0.5)) +
      #theme(legend.position = "bottom", legend.box = "horizontal") +
      theme(legend.position = c(0.25, 0.85), legend.box = "vertical", legend.box.background = element_rect(fill = "transparent", color = "transparent")) +
      theme(text = element_text(size = 10)) +
      labs(
        title = "Overall",
        #y = paste0("Cumulative incidence of \n ", event_name),
        y = paste0("Cumulative incidence of \n ", ylabel_name),
        x = paste0(time_unit, " since discharge")
      )
    
    file_name_touse <- paste0("../../output/graphs/cuminc_", add_time, "_", time_unit, "_", event_name, "_notable.png")
    save_plot(filename = file_name_touse, fig, dpi = 1200, base_height = 3.5, base_width = 3.5)
  }
  
  # yes subgroup yes table ----
  if (!is.null(subgroup) & notable == FALSE) {
    
    subgroup_list <- levels(dataframe[[subgroup]])
    print(length(subgroup_list))
    
    for (i in 1:length(subgroup_list)) {
      
      subdata <- dataframe %>%
        filter(get(subgroup) == subgroup_list[i])
      
      fig <- cuminc(as.formula(formula), data = subdata) %>%
        ggcuminc() +
        add_confidence_interval() +
        add_risktable() +
        coord_cartesian(xlim = c(0, add_time)) +
        coord_cartesian(ylim = c(0, ylim_touse)) +
        scale_y_continuous(
          labels = scales::percent) +
        scale_x_continuous(
          #n.breaks = 10) +
          n.breaks = 5) +
        scale_color_manual(
          values = c("blue", "red")) +
        scale_fill_manual(
          values = c("blue", "red")) +
        theme_bw() +
        theme(plot.title = element_text(size = 11, hjust = 0.5)) +
        #theme(legend.position = "bottom", legend.box = "horizontal") +
        theme(legend.position = c(0.25, 0.85), legend.box = "vertical", legend.box.background = element_rect(fill = "transparent", color = "transparent")) +
        theme(text = element_text(size = 10)) +
        labs(
          title = subgroup_list[i],
          #y = paste0("Cumulative incidence of \n ", event_name),
          y = paste0("Cumulative incidence of \n ", ylabel_name),
          x = paste0(time_unit, " since discharge")
        )
      file_name_touse <- paste0("../../output/graphs/cuminc_", add_time, "_", time_unit, "_", event_name, "_", subgroup_list[i], ".png")
      save_plot(filename = file_name_touse, fig, dpi = 1200, base_height = 6, base_width = 5)
    }
  }
  
  # yes subgroup no table ----
  if (!is.null(subgroup) & notable == TRUE) {
    
    subgroup_list <- levels(dataframe[[subgroup]])
    print(length(subgroup_list))
    
    for (i in 1:length(subgroup_list)) {
      
      subdata <- dataframe %>%
        filter(get(subgroup) == subgroup_list[i])
      
      fig <- cuminc(as.formula(formula), data = subdata) %>%
        ggcuminc() +
        add_confidence_interval() +
        coord_cartesian(xlim = c(0, add_time)) +
        coord_cartesian(ylim = c(0, ylim_touse)) +
        scale_y_continuous(
          labels = scales::percent) +
        scale_x_continuous(
          #n.breaks = 10) +
          n.breaks = 5) +
        scale_color_manual(
          values = c("blue", "red")) +
        scale_fill_manual(
          values = c("blue", "red")) +
        theme_bw() +
        theme(plot.title = element_text(size = 11, hjust = 0.5)) +
        #theme(legend.position = "bottom", legend.box = "horizontal") +
        theme(legend.position = c(0.25, 0.85), legend.box = "vertical", legend.box.background = element_rect(fill = "transparent", color = "transparent")) +
        theme(text = element_text(size = 10)) +
        labs(
          title = subgroup_list[i],
          #y = paste0("Cumulative incidence of \n ", event_name),
          y = paste0("Cumulative incidence of \n ", ylabel_name),
          x = paste0(time_unit, " since discharge")
        )
      file_name_touse <- paste0("../../output/graphs/cuminc_", add_time, "_", time_unit, "_", event_name, "_", subgroup_list[i], "_notable.png")
      save_plot(filename = file_name_touse, fig, dpi = 1200, base_height = 3.5, base_width = 3.5)
    }
  }
  
}

# need to make it less clumsy