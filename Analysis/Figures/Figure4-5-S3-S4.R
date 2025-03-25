# Script to create figures for the manuscript
# Load required libraries 
library(tidyr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(patchwork)
library(stringr)
library(RColorBrewer)

results = read.csv("Analysis/Output/Compiled/Combined_Results.csv")
results = results %>% 
  mutate(Validation.Set = ifelse(Validation.Set == "Köferle List", "Köferle Hits", Validation.Set)) %>%
  filter(Score != "Horlbeck")

# cl_all indicates if all cell lines of a study are combined or if each cell line has to be assessed individually

CreateHeatmap <- function(data, cl_all, metric, validation_set, additional_title = "")
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
    geom_text(data = . %>% filter(!is_max),
              aes(label = sprintf("%.2f", round(value, 2))), 
              color = "black"
    ) +
    geom_text(data = . %>% filter(is_max),
              aes(label = sprintf("%.2f", round(value, 2))), 
              color = "black",# size = 5, 
              fontface = "bold") +
    
    theme_minimal(base_size = 14) +
    labs( x = "Score", y = "Study", subtitle = paste0(additional_title,  metric)) +
    theme(plot.subtitle = element_text(hjust = 0.5))+  # Center-align subtitle
  
    #theme(
      # axis.text.x.top = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      # axis.text.x.bottom = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      # plot.subtitle = element_text(hjust = 0.5),
      # text = element_text(size = 16),          # General text size
      # plot.title = element_text(size = 22),   # Title text size
      # axis.title = element_text(size = 18),   # Axis title size
      # axis.text = element_text(size = 16) ) +
    scale_fill_gradientn(colors = colors, na.value = "grey90", name = "Metric")
  return(plot)
}


# Fig 3a
depmap_auroc = CreateHeatmap(results, cl_all = TRUE , c("AUROC"), "DepMap Hits" , additional_title = "(a)\n" )
# Fig 3b
depmap_aupr = CreateHeatmap(results, cl_all = TRUE , c("AUPR"), "DepMap Hits", additional_title = "(b)\n" )
#Fig S2a
depmap_ind_auroc = CreateHeatmap(results, cl_all = FALSE , c("AUROC"), "DepMap Hits" ,additional_title = "(a)\n")
#Fig S2b
depmap_ind_aupr = CreateHeatmap(results, cl_all = FALSE , c("AUPR"), "DepMap Hits",additional_title = "(b)\n" )

# Combine all 4 together
combined_plot_main_paper = (depmap_auroc / depmap_aupr) 
combined_plot_main_paper

# +  
  #plot_annotation(title = "AUROC and AUPR Across Different Studies Using DepMap Hits as the Benchmark",
  #                theme = theme(plot.title = element_text(hjust = 0.5, size = 18)) )
ggsave("Analysis/Figures/plots/Figure-4.png",plot =  combined_plot_main_paper, dpi = 600, width = 18,height = 16, units = "cm",bg = "white")

combined_plot_supp = (depmap_ind_auroc / depmap_ind_aupr) + 
  plot_annotation("",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 18)))
combined_plot_supp

ggsave("Analysis/Figures/plots/Figure-S3.png",plot =  combined_plot_supp, dpi = 600, width = 20,height = 25, units = "cm",bg = "white")

# Fig 4a
koferle_auroc = CreateHeatmap(results, cl_all = TRUE , c("AUROC"), "Köferle Hits" , additional_title = "(a)\n" )
# Fig 4b
koferle_aupr = CreateHeatmap(results, cl_all = TRUE , c("AUPR"), "Köferle Hits", additional_title = "(b)\n" )
#Fig S3a
koferle_ind_auroc = CreateHeatmap(results, cl_all = FALSE , c("AUROC"), "Köferle Hits" ,additional_title = "(a)\n")
#Fig S3b
koferle_ind_aupr = CreateHeatmap(results, cl_all = FALSE , c("AUPR"), "Köferle Hits",additional_title = "(b)\n" )

combined_plot_main_paper = (koferle_auroc / koferle_aupr) #+ 
  #plot_annotation("AUROC and AUPR Across Different Studies Using Köferle Hits as the Benchmark",
  #                theme = theme(plot.title = element_text(hjust = 0.5, size = 18)))  
combined_plot_main_paper
ggsave("Analysis/Figures/plots/Figure-5.png",plot =  combined_plot_main_paper, dpi = 600, width = 18,height = 16, units = "cm",bg = "white")

combined_plot_supp =  (koferle_ind_auroc / koferle_ind_aupr) + 
  plot_annotation("",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 18)))
combined_plot_supp
ggsave("Analysis/Figures/plots/Figure-S4.png",plot =  combined_plot_supp, dpi = 600, width = 20,height = 25, units = "cm",bg = "white")
