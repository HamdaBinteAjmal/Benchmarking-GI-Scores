
library("gemini")
library(tidyr)
library(dplyr)
library(tibble)
library(stringr)

parrish = read.csv("InputData/Parrish//GSE178179_pgPEN_counts_PC9.txt", sep = "\t")
parrish = parrish %>%
  separate(paralog_pair, into = c("gene1", "gene2"), sep = "\\|", remove = FALSE, extra = "merge") %>%
  mutate( gene1 = ifelse(grepl("^nt[1-8]$",gene1), "NT", gene1)) %>%
  mutate( gene2 = ifelse(grepl("^nt[1-8]$",gene2), "NT", gene2)) %>%
  mutate(gene1 = ifelse(grepl("^NTpg\\d+$", gene1), "NT", gene1)) %>%
  mutate(gene2 = ifelse(gene2 == "NA", "NT", gene2 )) %>%
  mutate(rownames = paste0(gRNA1_seq,":",gRNA2_seq)) %>%
  column_to_rownames("rownames")


guide.annotation_parrish = parrish %>% select(c("gene1", "gene2", "gRNA1_seq", "gRNA2_seq")) %>%
  rename(gene1.guide = gRNA1_seq, gene2.guide = gRNA2_seq ) %>%
  tibble::rownames_to_column() 


essentials = read.csv("InputData/Parrish/AchillesCommonEssentialControls.csv",header = TRUE)
essentials <- essentials %>%
  separate(Gene, into = c("gene_symbol", "entrez_id"), sep = "\\s*\\(", convert = TRUE) %>%
  mutate(entrez_id = str_replace(entrez_id, "\\)", ""))  %>% select(gene_symbol) %>% unlist() %>% unname()
parrish_genes = c(parrish$gene1, parrish$gene2) %>% unique()
pc_genes = essentials[essentials %in% parrish_genes]

nc_genes = c("NT")
parrishPC9 = parrish %>%
  select(c("PC9_ETP_RepA", "PC9_ETP_RepB", "PC9_ETP_RepC", 
              "PC9_LTP_RepA", "PC9_LTP_RepB", "PC9_LTP_RepC"))

### PC9 ##
ETP = 1:3
LTP = 4:6

sample.replicate.annotation_parrish <- data.frame(
  colname = c("PC9_ETP_RepA", "PC9_ETP_RepB", "PC9_ETP_RepC", 
              "PC9_LTP_RepA", "PC9_LTP_RepB", "PC9_LTP_RepC"),
  samplename = c("PC9_ETP",  "PC9_ETP",  "PC9_ETP", "PC9_LTP", "PC9_LTP", "PC9_LTP"),
  replicate = c("RepA", "RepB", "RepC" ,"RepA", "RepB", "RepC")
)

Input <- gemini_create_input(counts.matrix = parrishPC9 ,
                             sample.replicate.annotation = sample.replicate.annotation_parrish,
                             guide.annotation = guide.annotation_parrish,
                             ETP.column = ETP, 
                             LTP.column = LTP,
                             gene.column.names = c("gene1", "gene2"),
                             sample.column.name = "samplename",
                             verbose = TRUE)  ## 

Input %<>% gemini_calculate_lfc(normalize = TRUE, 
                                CONSTANT = 32)


Model_PC9 <- gemini_initialize(Input = Input, 
                           nc_gene = c("NT"), 
                           pattern_join = ':',
                           pattern_split = ':', 
                           cores = 1,
                           verbose = TRUE)

Model_PC9 %<>% gemini_inference(cores = 1,
                            verbose = TRUE, force_results = TRUE)

gemini_plot_mae(Model_PC9)
# pc_threshold to remove essential gene filter
Score <- gemini_score(Model = Model_PC9, pc_threshold = -Inf)



write.csv(file = "Gemini Scripts/GeminiOutput/Gemini_Parrish_PC9_Strong.csv", Score$strong)
write.csv(file = "Gemini Scripts/GeminiOutput/Gemini_Parrish_PC9_Sensitive.csv", Score$sensitive_lethality)

Score <- gemini_score(Model = Model_PC9, pc_gene = pc_genes)
write.csv(file = "Gemini Scripts/GeminiOutput/Filtered/Gemini_Parrish_PC9_Strong.csv", Score$strong)
write.csv(file = "Gemini Scripts/GeminiOutput/Filtered/Gemini_Parrish_PC9_Sensitive.csv", Score$sensitive_lethality)



########## HELA #######################

parrishHeLA = parrish %>%
  select(c("HeLa_ETP", 
           "HeLa_LTP_RepA", "HeLa_LTP_RepB", "HeLa_LTP_RepC"))


ETP = 1 # ETPs for both cell lines are seleted after carefully going through the Parrish Paper. 
LTP = 2:4


sample.replicate.annotation_parrish <- data.frame(
  colname = c("HeLa_ETP", 
              "HeLa_LTP_RepA", "HeLa_LTP_RepB", "HeLa_LTP_RepC"),
  samplename = c("HeLa_ETP", "HeLa_LTP", "HeLa_LTP", "HeLa_LTP"),
  replicate = c("ETP", "RepA", "RepB", "RepC")
)

Input <- gemini_create_input(counts.matrix = parrishHeLA ,
                             sample.replicate.annotation = sample.replicate.annotation_parrish,
                             guide.annotation = guide.annotation_parrish,
                             ETP.column = ETP, 
                             LTP.column = LTP,
                             gene.column.names = c("gene1", "gene2"),
                             sample.column.name = "samplename",
                             verbose = TRUE)  


Input %<>% gemini_calculate_lfc(normalize = TRUE, 
                                CONSTANT = 32)

Model_HeLa <- gemini_initialize(Input = Input, 
                           nc_gene = c("NT"), 
                           pattern_join = ':',
                           pattern_split = ':', 
                           cores = 1,
                           verbose = TRUE)

Model_HeLa %<>% gemini_inference(cores = 1,
                            verbose = TRUE, force_results = TRUE)

gemini_plot_mae(Model_HeLa)
Score <- gemini_score(Model = Model_HeLa, pc_threshold = -Inf)

write.csv(file = "Gemini Scripts/GeminiOutput/Gemini_Parrish_HeLa_Strong.csv", Score$strong)
write.csv(file = "Gemini Scripts/GeminiOutput/Gemini_Parrish_HeLa_Sensitive.csv", Score$sensitive_lethality)

Score <- gemini_score(Model = Model_HeLa, pc_gene = pc_genes)

write.csv(file = "Gemini Scripts/GeminiOutput/Filtered/Gemini_Parrish_HeLa_Strong.csv", Score$strong)
write.csv(file = "Gemini Scripts/GeminiOutput/Filtered/Gemini_Parrish_HeLa_Sensitive.csv", Score$sensitive_lethality)


