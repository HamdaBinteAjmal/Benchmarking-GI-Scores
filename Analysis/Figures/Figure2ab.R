library(tidyr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(patchwork)
library(stringr)
library(RColorBrewer)
results = read.csv("Analysis/Output/Compiled/Combined_Results.csv")

######### FIGURE 2 ########################

labels <- c("DepMap Hits" = "(a)\nDe Kegel Hits", "Köferle List" = "(b)\nKöferle Hits")

# Calculate the overlap the pairs in each study with DepMap Hits validation set
overlaps <- results %>% 
  filter(Cell.line  != "All") %>%
  mutate(total.pairs.in.study = case_when(
    Study.name == "Dede" ~ 400,
    Study.name == "Thompson" ~ 1191,
    Study.name == "Parrish"~  1030,
    Study.name == "Ito"~  5065,
    Study.name == "CHyMErA"~  672,
    
  )) %>%
  
  #mutate(Study.name = paste0(Study.name, " (n=",total.pairs.in.study,")" )) %>%
  select(Validation.Set, Study.name, Common.samples, Positive.Samples,total.pairs.in.study ) %>%
  group_by(Validation.Set, Study.name) %>% # Group by Validation.Set and Study.name
  filter(Common.samples == max(Common.samples)) %>% # Keep rows with max Common.samples
  ungroup() %>%
  distinct() %>%
  mutate(prop = (Common.samples)/total.pairs.in.study*100) 


Fig2ab = ggplot(overlaps, aes(x = Study.name, y = prop, fill = Study.name, color = Study.name)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Common.samples, y = prop), color = "#404040", size = 3, hjust = -0.1) +
  facet_grid(~ Validation.Set, labeller = labeller(Validation.Set = labels)) + # Create separate plots for each Validation.Set
  labs(title = "", 
       y = "Percentage of common gene pairs") +

  scale_y_continuous(limits = c(0, 100), 
                     breaks = c(0,25,50,75, 100), 
                     labels = c("0%","25%","50%", "75%", "100%")) +
  scale_fill_manual(values = c("steelblue", "steelblue", "steelblue", "steelblue", "steelblue")) + # Provide 5 colors
  scale_color_manual(values = c("steelblue", "steelblue", "steelblue", "steelblue", "steelblue")) + # Provide 5 colors
  theme_minimal(base_size = 14)+
  theme(panel.border = element_rect(color = "darkgrey", fill = NA, size = 0.1),
        panel.spacing =  unit(1.5, "cm"))+
  xlab("")+
  guides(color = "none", fill = "none")+
  coord_flip()



labels <- c("DepMap Hits" = "(c)\nDe Kegel Hits", "Köferle List" = "(d)\nKöferle Hits")

# Calculate the percentage positives from each study
# Using zdLFC score as it does not drop any pairs in pre-processing. 
Fig2cd = overlaps %>%
  
  mutate(pp = Positive.Samples / Common.samples * 100)%>% 
  
  ggplot(aes(x = Study.name, y = pp, fill = Study.name)) +
  geom_text(aes(label = Positive.Samples, y = pp), color = "#404040", size = 3, hjust = -0.1) +
  
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0, 100), 
                     breaks = c(0,25,50,75, 100), 
                     labels = c("0%","25%","50%", "75%", "100%")) + 
  scale_fill_manual(values = c("steelblue", "steelblue", "steelblue", "steelblue", "steelblue")) + # Provide 5 colors
  scale_color_manual(values = c("steelblue", "steelblue", "steelblue", "steelblue", "steelblue")) + # Provide 5 colors
  
  ylab("Percentage of SL pairs")+ xlab("")+
  coord_flip()+
  facet_grid(~Validation.Set,labeller = labeller(Validation.Set = labels))+
  theme_minimal(base_size = 14)+
  theme(legend.position = "none",    
        panel.border = element_rect(color = "darkgrey", fill = NA, size = 0.1),
        panel.spacing =  unit(1.5, "cm"), # Adds a border around each facet panel

)

combined_plot = Fig2ab/Fig2cd  
combined_plot

ggsave("Analysis/Figures/plots/Figure-2.png",plot =  combined_plot, dpi = 600, width = 18,height = 18, units = "cm",bg = "white")

#ggsave("Analysis/Figures/plots/Figure-2a.png",plot =  Fig2ab, dpi = 600, width = 18,height = 9, units = "cm",bg = "white")
#ggsave("Analysis/Figures/plots/Figure-2b.png",plot =  Fig2cd, dpi = 600, width = 18,height = 9, units = "cm",bg = "white")
