CreateHeatmap_Dummy <- function(data, cl_all, metric, validation_set, additional_title = "")
{ 
  
  blues_ = brewer.pal(9,"Blues")[2:7]
  purples_ = brewer.pal(9,"Purples")[2:7]
  
  
  if (cl_all == TRUE) 
  {
    filtered_data <- data %>%
      filter(Cell.line == "All" & Metric == metric & Validation.Set == validation_set) 
    
  } else 
  {
    filtered_data <- data %>%
      filter(Cell.line != "All" & Metric == metric & Validation.Set == validation_set)  
    
  }
  
  if (metric == "AUPR") 
  {
    colors = blues_
  } else if(metric == "AUROC") 
  {
    colors = purples_
  }
  
  
  plot = filtered_data %>% 
    mutate(y = paste0(Study.name, "(", Cell.line, ")")) %>%
    mutate(y = str_replace_all(y, "\\(All\\)", "")) %>%
    mutate(value = round(value,2)) %>%
    group_by(y, Metric) %>%
    mutate(scaled_value = (value - min(value)) / (max(value) - min(value))) %>%
    
    
    mutate(is_max = value == max(value,na.rm = TRUE)) %>%
    ungroup() %>%
    ggplot( aes(y = y, x = Score, fill = scaled_value)) +
    geom_tile(show.legend = FALSE) +

    
    theme_minimal() +
    labs( x = "", y = "", subtitle = paste0(additional_title,  metric)) +
    
    theme(
      axis.text.x.top = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      axis.text.x.bottom = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      plot.subtitle = element_text(hjust = 0.5),
      text = element_text(size = 17),          # General text size
      plot.title = element_text(size = 22),   # Title text size
      axis.title = element_text(size = 18),   # Axis title size
      axis.text = element_text(size = 14) ) +
    scale_fill_gradientn(colors = colors, na.value = "grey90", name = "Metric")
  return(plot)
}
results = results %>% filter(Score != "Horlbeck")
depmap_auroc_dummy = CreateHeatmap_Dummy(results, cl_all = TRUE , c("AUROC"), "DepMap Hits" , additional_title = "" )
# Fig 3b
depmap_aupr_dummy = CreateHeatmap_Dummy(results, cl_all = TRUE , c("AUPR"), "DepMap Hits", additional_title = "" )
combined_plot_dummy = (depmap_auroc_dummy / depmap_aupr_dummy) #/ (depmap_ind_auroc | depmap_ind_aupr)+
combined_plot_dummy
ggsave("Analysis/Figures/plots/Figure-Dummy.png",plot =  combined_plot_dummy, dpi = 600, width = 30,height = 15, units = "cm",bg = "white")
