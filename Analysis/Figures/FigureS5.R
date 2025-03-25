
# compare AUROC of Orthrus on CHYMERA with or without filtering
library(tidyr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(patchwork)
library(stringr)
library(RColorBrewer)
custom_labeller <- function(labels) {
  # Generate prefixes (a), (b), etc.
  prefixes <- paste0("(", letters[seq_along(labels)] ,")\n ")
  # Concatenate prefixes with original facet titles
  labels <- paste0(prefixes, labels)
  return(labels)
}


results = read.csv("Analysis/Output/Compiled/Combined_Results.csv")
results_filtered = read.csv("Analysis/Output/Filtered/Compiled/Combined_Results.csv")

# compare AUROC of Gemini on CHYMERA before and after pre-process
filtered = results_filtered %>% 
  filter( Score == "Orthrus" , Cell.line == "All",Study.name == "CHyMErA" )  %>% select(-X)

unfiltered = results %>% 
  filter(Score == "Orthrus", Cell.line == "All",Study.name == "CHyMErA")  %>% select(-X)




diff = filtered %>% inner_join(unfiltered, by = c("Metric" ,  "Score", "Cell.line", "Validation.Set", "Study.name"),
                               suffix = c(".filtered", ".unfiltered")) %>%
  select_if(~ n_distinct(.) > 1) %>%
  mutate(difference =  value.filtered - value.unfiltered) %>%
  filter(Metric != "AUPRG") %>%
  mutate(Metric = factor(Metric , levels = c( "AUROC","AUPR"))) %>%
  mutate(Validation.Set = ifelse(Validation.Set == "DepMap Hits", "De Kegel Hits", Validation.Set)) %>% #Changed it in the paper
 mutate(Validation.Set = ifelse(Validation.Set == "Köferle List", "Köferle Hits", Validation.Set)) %>%
  mutate(direction = ifelse(difference < 0, "Negative", "Positive"))


plotS5ab = ggplot(diff, aes(x = Metric, y = difference, fill = direction)) +
  geom_bar( stat = "identity" ,
           width = 0.6, # Narrower bars
           position = position_dodge(width = 0.01) # Reducing space between bars
  ) +
  labs(x = "Study", y = "Change", fill = "Study") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")+
  
  geom_hline(yintercept = 0, color = "black") +
  facet_grid(cols = vars(Validation.Set), scales = "free_x",
             labeller = labeller(Validation.Set = custom_labeller))+
  ylim(c(-0.05,0.025))+  
  scale_fill_manual(
    values = c("Negative" = "#D6604D", "Positive" = "#4393C3"), # Blue for positive, red for negative
    name = "")+                                 # Legend title
    
  theme(
    panel.spacing = unit(3, "lines"), # Increase space between facets here
    
    #legend.text = element_text(size = 6), # Decrease legend text size
    #legend.title = element_text(size = 7), # Decrease legend title size
    #plot.subtitle = element_text(size = 7), # Adjust subtitle text size here
    panel.border = element_rect(color = "darkgrey", fill = NA, size = 0.1) # Adds a border around each facet panel
  ) + 
  coord_flip() +
  xlab("")
plotS5ab

ggsave("Analysis/Figures/plots/Fig-S5.png",plot =  plotS5ab, dpi = 600, width = 18,height = 9, units = "cm",bg = "white")
