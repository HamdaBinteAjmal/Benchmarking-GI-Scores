library(tidyr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(patchwork)
library(stringr)
library(RColorBrewer)
results = read.csv("Analysis/Output/Compiled/Combined_Results.csv")
results_filtered = read.csv("Analysis/Output/Filtered/Compiled/Combined_Results.csv")

# compare AUROC of Gemini on CHYMERA before and after pre-process
filtered = results_filtered %>% 
  filter( Score == "Gemini(Sens)" , Cell.line == "All" ) 

unfiltered = results %>% 
  filter(Score == "Gemini(Sens)", Cell.line == "All")


custom_labeller <- function(labels) {
  # Generate prefixes (a), (b), etc.
  prefixes <- paste0("(", letters[seq_along(labels)] ,")\n ")
  # Concatenate prefixes with original facet titles
  labels <- paste0(prefixes, labels)
  return(labels)
}

combine = filtered %>% inner_join(unfiltered, by = c("Metric" ,  "Score", "Cell.line", "Validation.Set", "Study.name"),
                                  suffix = c(".filtered", ".unfiltered")) %>%
  select_if(~ n_distinct(.) > 1)  %>%
  pivot_longer(
    cols = c(value.filtered, value.unfiltered, Common.samples.filtered, Common.samples.unfiltered, Positive.Samples.filtered, Positive.Samples.unfiltered),  # Columns to pivot
    names_to = c(".value", "Time"),  # Pivoting into .value (before/after) and Time (before/after)
    names_pattern = "(.*)\\.(filtered|unfiltered)",  # Regex to match and separate the column names
  ) %>% 
  mutate(Time = factor(Time, levels = c("filtered", "unfiltered"))) %>%
  mutate(baseline = ifelse(Metric == "AUROC", 0.5, Positive.Samples/Common.samples))



diff = filtered %>% inner_join(unfiltered, by = c("Metric" ,  "Score", "Cell.line", "Validation.Set", "Study.name"),
                               suffix = c(".filtered", ".unfiltered")) %>%
  select_if(~ n_distinct(.) > 1) %>%
  mutate(difference =  (value.filtered - value.unfiltered) ) %>%
  mutate(Validation.Set = ifelse(Validation.Set == "DepMap Hits", "De Kegel Hits", Validation.Set)) %>%
  mutate(Validation.Set = ifelse(Validation.Set == "Köferle List", "Köferle Hits", Validation.Set)) %>%
  mutate(direction = ifelse(difference < 0, "Negative", "Positive"))

plot = ggplot(diff, aes(x = Study.name, y = difference, fill = direction)) +
  geom_bar(stat = "identity" ,
           width = 0.6, # Narrower bars
           position = position_dodge(width = 0.01) # Reducing space between bars
  ) +
  labs(x = "Study", y = "Change", fill = "Change") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")+
  
  geom_hline(yintercept = 0, color = "black") +

  facet_grid(cols = vars(Validation.Set), rows = vars(Metric), scales = "free_x", switch = "y", 
             labeller = labeller(Validation.Set = custom_labeller))+
  ylim(c(-0.025,0.08))+  
  scale_fill_manual(
    values = c("Negative" = "#D6604D", "Positive" = "#4393C3"), # Blue for positive, red for negative
   name = "")+
  theme(
    #legend.text = element_text(size = 6), # Decrease legend text size
    #legend.title = element_text(size = 7), # Decrease legend title size
    #plot.subtitle = element_text(size = 7), # Adjust subtitle text size here
    panel.border = element_rect(color = "darkgrey", fill = NA, size = 0.1) # Adds a border around each facet panel

  ) + 
  
  coord_flip()
plot
ggsave("Analysis/Figures/plots/Fig-6ab.png",plot =  plot, dpi = 600, width = 18,height = 16, units = "cm",bg = "white")