# Hows the number of dropped pairs if pre-processing is applied.
library(tidyr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(patchwork)
library(stringr)
library(RColorBrewer)

results = read.csv("Analysis/Output/Filtered/Compiled/Combined_Results.csv")


p = results %>%
  filter(Score != "Horlbeck") %>%
  filter(Metric == "AUROC" & Validation.Set == "DepMap Hits") %>%
  filter(Cell.line == "All") %>%
  group_by(Study.name, Cell.line) %>% 
  mutate(dropped = (max(Common.samples) - Common.samples)/max(Common.samples)*100 )%>% 
  
  ggplot(aes(x = Score, y = dropped, fill = Score)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("steelblue", "steelblue", "steelblue", "steelblue", "steelblue", "steelblue")) + # Provide 6 colors

  scale_y_continuous(limits = c(0, 100), 
                     breaks = c(0,25,50,75, 100), 
                     labels = c("0%","25%","50%", "75%", "100%")) +
  ylab("Percentage of filtered pairs")+ xlab("")+
  coord_flip()+
  facet_wrap(~Study.name, ncol = 1)+
  theme_minimal()+
  theme(legend.position = "none")
ggsave("Analysis/Figures/plots/Figure-S1.jpeg",plot =  p,  height = 15,width = 10,dpi = 600,bg = "white",units = "cm",)
