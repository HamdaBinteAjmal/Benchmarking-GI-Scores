library(tidyr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(patchwork)
library(stringr)
library(reshape2)



pre_process <- function(df, study, test)
{
  new_order = c("Gemini(Sens)", "Gemini(Strong)", "Orthrus", "Parrish", "zdLFC")
  df = df[new_order, new_order]
  #df =  get_upper_tri(df)
  df = df %>% rownames_to_column()
  df_long <- melt(df, na.rm = TRUE)
  colnames(df_long) = c("Var1", "Var2", "value")
  df_long = df_long %>% mutate(Study = study, Test= test)
  
  return(df_long)
  
}

cor_pal = "viridis"
jacc_pal = "viridis"
jac_title = "(b) Jaccard Similarity"
corr_title = "(a) Average Correlation"

ito_correlations = read.csv("Analysis/Output/Comparison/Ito_Correlations.csv" ,check.names = FALSE,row.names = 1) 
dede_correlation = read.csv("Analysis/Output/Comparison/Dede_Correlations.csv" ,check.names = FALSE,row.names = 1) 
chymera_correlation = read.csv("Analysis/Output/Comparison/Chymera_Correlations.csv" ,check.names = FALSE,row.names = 1) 
Thompson_correlation = read.csv("Analysis/Output/Comparison/Thompson_Correlations.csv" ,check.names = FALSE,row.names = 1) 
Parrish_correlation = read.csv("Analysis/Output/Comparison/Parrish_Correlations.csv" ,check.names = FALSE,row.names = 1) 


average_correlations  = (ito_correlations + dede_correlation + chymera_correlation + Thompson_correlation + Parrish_correlation)/5


ito_correlations = pre_process(ito_correlations, "Ito Study", corr_title)

dede_correlation = pre_process(dede_correlation, "Dede Study",corr_title)

chymera_correlation = pre_process(chymera_correlation, "CHyMErA Study",corr_title)

Parrish_correlation = pre_process(Parrish_correlation, "Parrish Study",corr_title)

Thompson_correlation = pre_process(Thompson_correlation, "Thompson Study",corr_title)


all_correlations = rbind(ito_correlations, dede_correlation,chymera_correlation, Parrish_correlation, Thompson_correlation)

average_correlations = pre_process(average_correlations, "", corr_title)

## Now plot Jaccard for each study

dede_jaccard = read.csv("Analysis/Output/Comparison/Dede_Jaccard.csv", row.names = 1, check.names = FALSE)
ito_jaccard = read.csv("Analysis/Output/Comparison/Ito_Jaccard.csv", row.names = 1, check.names = FALSE)
Thompson_jaccard = read.csv("Analysis/Output/Comparison/Thompson_Jaccard.csv", row.names = 1, check.names = FALSE)
Parrish_jaccard = read.csv("Analysis/Output/Comparison/Parrish_Jaccard.csv", row.names = 1, check.names = FALSE)
Thompson_jaccard = read.csv("Analysis/Output/Comparison/Thompson_Jaccard.csv", row.names = 1, check.names = FALSE)
Chymera_jaccard = read.csv("Analysis/Output/Comparison/Chymera_Jaccard.csv", row.names = 1, check.names = FALSE)



all_jacards = rbind(pre_process(dede_jaccard,study = "Dede Study", test = jac_title),
                    pre_process(ito_jaccard, study = "Ito Study", test = jac_title),
                    pre_process(Parrish_jaccard, study = "Parrish Study", test = jac_title),
                    pre_process(Thompson_jaccard, study = "Thompson Study", test = jac_title),
                    pre_process(Chymera_jaccard, study = "CHyMErA Study", test = jac_title))

average_jaccards = (dede_jaccard + ito_jaccard + Parrish_jaccard + Thompson_jaccard + Chymera_jaccard) / 5
average_jaccards = pre_process(average_jaccards, "", jac_title)


main_fig =  rbind(average_correlations, average_jaccards) %>%
  ggplot( aes(x = Var1, y = Var2, fill = value))+
  geom_tile(show.legend = FALSE) + 
  theme_minimal((base_size = 26)) +
  geom_text(aes(label = round(value, 2)), color = "black",  size = 5) + 
  labs( x = "", y = "") +
  theme(
  # text = element_text(size = 20),                 # Base text size
   axis.text.x = element_text(angle = 45,hjust = 1),          # X-axis tick labels
  # axis.text.y = element_text(size = 18),          # Y-axis tick labels
  # strip.text.x = element_text(size = 22),  # Facet titles (top)
  # strip.text.y = element_text(size = 22),  # Facet titles (side)
  # plot.title = element_text(size = 24, hjust = 0.5),     # Title size (if added)
  # plot.subtitle = element_text(size = 28, hjust = 0))+     # Title size (if added)
  )+

  scale_fill_distiller(
    palette = "Greens",      # Using the built-in "Greens" palette
    na.value = "grey90", 
    limits = c(0, 1), 
    oob = scales::squish,
    direction = 1)+
  
  
  # Square tiles using coord_fixed()
  coord_fixed(ratio = 1) +
  facet_wrap(~ Test)

ggsave(main_fig, width = 18, height = 9, filename = "Analysis/Figures/plots/Figures-3ab.png", dpi = 600, bg = "white")


supp_figa =  all_correlations  %>%
  ggplot( aes(x = Var1, y = Var2, fill = value))+
  geom_tile(show.legend = FALSE) + 
  theme_minimal((base_size = 20)) +
  geom_text(aes(label = round(value, 2)), color = "black", color = "black", size = 5) + 
  labs( x = "", y = "", subtitle = "(a) Average correlation across methods") +
  
  scale_fill_distiller(
    palette = "Greens",      # Using the built-in "Greens" palette
    na.value = "grey90", 
    limits = c(0, 1), 
    oob = scales::squish,
    direction = 1)+
  
  # Square tiles using coord_fixed()
  coord_fixed(ratio = 1) + 
  theme(                # Base text size
     axis.text.x = element_text(angle = 90,hjust = 1),
    
  )+
  facet_grid(~Study, switch = "y")


supp_figb =  all_jacards  %>%
  ggplot( aes(x = Var1, y = Var2, fill = value))+
  geom_tile(show.legend = FALSE) + 
  theme_minimal(base_size = 20) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 5) + 
  labs( x = "", y = "", subtitle = "(a) Average Jaccard similarity of top 5% scored gene pairs across methods") +
  
  scale_fill_distiller(
    palette = "Greens",      # Using the built-in "Greens" palette
    na.value = "grey90", 
    limits = c(0, 1), 
    oob = scales::squish,
    direction = 1)+
  
  # Square tiles using coord_fixed()
  coord_fixed(ratio = 1) + 
  theme( 
    #text = element_text(size = 16),
    #text = element_text(size = 20),                 # Base text size
    axis.text.x = element_text(angle = 90,hjust = 1),          # X-axis tick labels
    #axis.text.y = element_text(size = 18),          # Y-axis tick labels
    #strip.text.x = element_text(size = 22),  # Facet titles (top)
    #strip.text.y = element_text(size = 22),  # Facet titles (side)
    #plot.title = element_text(size = 24, hjust = 0.5),     # Title size (if added)
    #plot.subtitle = element_text(size = 28, hjust = 0)     # Title size (if added)
    
  )+
  facet_grid(~Study, switch = "y")

supp_fig = supp_figa/supp_figb
ggsave(supp_fig, width = 20, height = 15, filename = "Analysis/Figures/plots/Figures-S2ab.png", dpi = 600, bg = "white")
