# Source the external R file
source("Analysis/Figures/Figure7ab.R")

# Keep only p1 and p2, remove everything else
rm(list = setdiff(ls(), c("plot")))


library(pROC)
library(PRROC)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(patchwork)

ground_truth_Koferle = read_excel('InputData/Benchmarks/Koferle.xlsx',sheet =  "PaCT")

b2  = ground_truth_Koferle %>% 
  separate(Pair, into = c("gene1", "gene2"), sep = "_") %>%
  mutate(sorted_gene_pair = ifelse(gene1<gene2, paste0(gene1,"_", gene2), paste0(gene2, "_", gene1))) %>%
  select("sorted_gene_pair", "Sub-Dataset", "Correlation Spearman", "Screen Type", "Hit?")%>%
  filter(`Screen Type` == "AVANA", `Sub-Dataset` == "expr_paralog") %>%
  mutate(ground_truth = NA) %>%
  mutate(ground_truth = ifelse(`Correlation Spearman` > 0 & `Hit?` == "yes", TRUE, ground_truth)) %>%
  mutate(ground_truth = ifelse(`Correlation Spearman` < 0 & `Hit?` == "no", FALSE, ground_truth)) %>%
  drop_na(ground_truth)  
  
  

b2 = b2 %>% select(sorted_gene_pair,ground_truth ) %>%
  drop_na(ground_truth) 


## Read Orthrus Scores : Filtered, find out which rows were filtered (NAs)
orthrus_filtered = read.csv("Orthrus Scripts/OrthrusOutput/Filtered/chymera_orthrus.tsv", sep='\t') %>% 
  select(gene1, gene2, contains("differential")) %>% 
  select(gene1, gene2, contains("HAP1_T18") | contains("RPE1_T24"))  %>%
  mutate(gene_pair = ifelse(gene1 < gene2, paste0(gene1,"_", gene2), paste0(gene2,"_", gene1))) %>%
  rename(HAP1_T18 = differential_combn_vs_single_HAP1_T18,
         RPE1_T24 = differential_combn_vs_single_RPE1_T24) %>%
  select(-gene1, -gene2) %>% 
  mutate(HAP1_T18 = HAP1_T18 * -1, RPE1_T24 = RPE1_T24 * -1) 

filtered_out_rows = orthrus_filtered %>%
  filter(is.na(HAP1_T18) | is.na(RPE1_T24))%>%
  pull(gene_pair)

rm(orthrus_filtered)

## Read Orthrus Scores with no proprocessing and separate into two sets:
# gene pairs which were filtered and those which were not
orthrus_unfiltered = read.csv("Orthrus Scripts/OrthrusOutput/chymera_orthrus.tsv", sep='\t') %>% 
  select(gene1, gene2, contains("differential")) %>% 
  select(gene1, gene2, contains("HAP1_T18") | contains("RPE1_T24"))  %>%
  mutate(gene_pair = ifelse(gene1 < gene2, paste0(gene1,"_", gene2), paste0(gene2,"_", gene1))) %>%
  rename(HAP1_T18 = differential_combn_vs_single_HAP1_T18,
         RPE1_T24 = differential_combn_vs_single_RPE1_T24) %>%
  select(-gene1, -gene2) %>% 
  mutate(HAP1_T18 = HAP1_T18 * -1, RPE1_T24 = RPE1_T24 * -1) %>%
  filter(!(gene_pair %in% filtered_out_rows))


orthrus_unfiltered = orthrus_unfiltered %>%
  inner_join(b2,by = c("gene_pair" = "sorted_gene_pair")) %>%
  pivot_longer(cols = c(HAP1_T18, RPE1_T24), 
               names_to = "Cell_Line", 
               values_to = "Orthrus")%>%
  mutate(korlefe_hit = ifelse(ground_truth == TRUE, 1,0))




orthrus_filtered = read.csv("Orthrus Scripts/OrthrusOutput/chymera_orthrus.tsv", sep='\t') %>% 
  select(gene1, gene2, contains("differential")) %>% 
  select(gene1, gene2, contains("HAP1_T18") | contains("RPE1_T24"))  %>%
  mutate(gene_pair = ifelse(gene1 < gene2, paste0(gene1,"_", gene2), paste0(gene2,"_", gene1))) %>%
  rename(HAP1_T18 = differential_combn_vs_single_HAP1_T18,
         RPE1_T24 = differential_combn_vs_single_RPE1_T24) %>%
  select(-gene1, -gene2) %>% 
  mutate(HAP1_T18 = HAP1_T18 * -1, RPE1_T24 = RPE1_T24 * -1) %>%
  filter(gene_pair %in% filtered_out_rows) %>%
  inner_join(b2,by = c("gene_pair" = "sorted_gene_pair")) %>%
  pivot_longer(cols = c(HAP1_T18, RPE1_T24), 
               names_to = "Cell_Line", 
               values_to = "Orthrus") %>%
  mutate(korlefe_hit = ifelse(ground_truth == TRUE, 1,0))








# Compute ROC Curve
roc_filtered <- roc.curve(scores.class0 = orthrus_filtered$Orthrus[orthrus_filtered$ground_truth == 1],
                          scores.class1 = orthrus_filtered$Orthrus[orthrus_filtered$ground_truth == 0],
                          curve = TRUE)
roc_unfiltered <- roc.curve(scores.class0 = orthrus_unfiltered$Orthrus[orthrus_unfiltered$ground_truth == 1],
                            scores.class1 = orthrus_unfiltered$Orthrus[orthrus_unfiltered$ground_truth == 0],
                            curve = TRUE)
roc_filtered_auc = round(roc_filtered$auc,2)
roc_unfiltered_auc = round(roc_unfiltered$auc,2)
roc_filtered = as.data.frame(roc_filtered$curve)[,1:2]
colnames(roc_filtered) = c("FPR", "TPR") 
roc_unfiltered = as.data.frame(roc_unfiltered$curve)[,1:2]
colnames(roc_unfiltered) = c("FPR", "TPR") 

roc_random_line <- data.frame(
  FPR = c(0, 1),
  TPR = c(0, 1)
)


filtered_label_roc <- paste0("Filtered (AUC = ", sprintf("%.2f", roc_filtered_auc), ")")
unfiltered_label_roc <- paste0("Unfiltered (AUC = ", sprintf("%.2f", roc_unfiltered_auc), ")")
random_label_roc <- "Random (AUC = 0.50)"

roc = rbind(roc_unfiltered %>% mutate(Status = unfiltered_label_roc),
            roc_filtered %>% mutate(Status = filtered_label_roc),
            roc_random_line %>% mutate(Status = random_label_roc)) %>%
  mutate(Status = factor(Status, levels = c(filtered_label_roc, 
                                            unfiltered_label_roc, 
                                            random_label_roc)))



# Compute PR Curve
pr_filtered_baseline = round(length(orthrus_filtered$Orthrus[orthrus_filtered$ground_truth == 1])/length(orthrus_filtered$Orthrus),2)
pr_unfiltered_baseline = round(length(orthrus_unfiltered$Orthrus[orthrus_unfiltered$ground_truth == 1])/length(orthrus_unfiltered$Orthrus),2)



pr_filtered <- pr.curve(scores.class0 = orthrus_filtered$Orthrus[orthrus_filtered$ground_truth == 1],
                        scores.class1 = orthrus_filtered$Orthrus[orthrus_filtered$ground_truth == 0],
                        curve = TRUE)
pr_unfiltered <- pr.curve(scores.class0 = orthrus_unfiltered$Orthrus[orthrus_unfiltered$ground_truth == 1],
                          scores.class1 = orthrus_unfiltered$Orthrus[orthrus_unfiltered$ground_truth == 0],
                          curve = TRUE)
pr_filtered_auc = round(pr_filtered$auc.integral,2)
pr_unfiltered_auc = round(pr_unfiltered$auc.integral,2)


pr_filtered = as.data.frame(pr_filtered$curve)[,1:2]
colnames(pr_filtered) = c("Recall", "Precision") 
pr_unfiltered = as.data.frame(pr_unfiltered$curve)[,1:2]
colnames(pr_unfiltered) = c("Recall", "Precision") 

pr_filtered_random_line <- data.frame(
  Recall = c(0, 1),
  Precision = c(pr_filtered_baseline, pr_filtered_baseline)
)

pr_unfiltered_random_line <- data.frame(
  Recall = c(0, 1),
  Precision = c(pr_unfiltered_baseline, pr_unfiltered_baseline)
)


filtered_label_pr <- paste0("Filtered (AUC = ", sprintf("%.2f", pr_filtered_auc), ")")
unfiltered_label_pr <- paste0("Unfiltered (AUC = ", sprintf("%.2f", pr_unfiltered_auc) , ")")
random_label_pr_filtered <- paste0("Baseline (AUC = ", sprintf("%.2f", pr_filtered_baseline),")")
random_label_pr_unfiltered <- paste0("Baseline (AUC = ", sprintf("%.2f", pr_unfiltered_baseline),")")

pr = rbind(pr_unfiltered %>% mutate(Status = unfiltered_label_pr),
           pr_filtered %>% mutate(Status = filtered_label_pr),
           pr_filtered_random_line %>% mutate(Status = random_label_pr_filtered),
           pr_unfiltered_random_line %>% mutate(Status = random_label_pr_unfiltered)) %>%
  mutate(Status = factor(Status, levels = c(filtered_label_pr, 
                                            unfiltered_label_pr, 
                                            random_label_pr_filtered,
                                            random_label_pr_unfiltered)))

# Create the ggplot
p1 = ggplot(roc) +
  # Add the filtered ROC curve
  geom_line(aes(x = FPR, 
                y = TPR, 
                color = Status, linetype = Status) )+
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  labs(subtitle =  "(c) ROC Curve")+
  scale_color_manual(values=c('#E69F00', '#009E73', 'black'))+
  guides(color = guide_legend(order = 1, nrow  = 2),
         linetype = guide_legend(order = 1, nrow = 2)) +  # Filtered and unfiltered curves will stay above
  scale_linetype_manual(values = c("solid", "solid", "dashed")) +  # Set dashed line for Random Classifier
  
  theme_minimal(base_size = 11)+
  theme(legend.position = "bottom",  # Move legend to the bottom
        legend.title = element_blank(),
        plot.title =  element_text(hjust = 0.5),
        plot.subtitle =  element_text(hjust = 0.5))


p2 = ggplot(pr) +
  # Add the filtered ROC curve
  geom_line(aes(x = Recall, 
                y = Precision, 
                color = Status, linetype = Status) )+
  xlab("Recall") +
  labs(subtitle = "(d) PR Curve") +
  ylab("Precision") +
  scale_color_manual(values=c('#E69F00', '#009E73', '#E69F00', '#009E73'))+
  guides(color = guide_legend(order = 1,nrow = 2),
         linetype = guide_legend(order = 1,nrow = 2)) +  # Filtered and unfiltered curves will stay above
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed")) +  # Set dashed line for Random Classifier
  
  theme_minimal(base_size = 11)+
  theme(legend.position = "bottom",  # Move legend to the bottom
        legend.title = element_blank(),
        plot.subtitle = element_text(hjust = 0.5))

plot7cd = (p1 | p2) 
 
full_plot = plot/ plot7cd
ggsave("Analysis/Figures/plots/Figure-7.png",plot =  full_plot, dpi = 600, width = 20,height = 20, units = "cm",bg = "white")
