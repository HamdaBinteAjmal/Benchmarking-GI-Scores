# LICENSE INFORMATION
The MIT license applies to the entire repo except for subfolders that have their own license file.
# Benchmarking Genetic Interaction Scoring Methods for Identifying Synthetic Lethality from Combinatorial CRISPR Screen Data
 
Data processing and analysis code for the paper of the same name, published in : 

Cite as: []

Supplementary data is available at: https://figshare.com/s/183520dde2e683ea9053 DOI: 10.6084/m9.figshare.27868350

# Input Data Sources
Following are the details of input datasets needed to run all the scripts:

| Subfolder                     | Filename                                | Description                                                      | Data Source                                                                 |
|-------------------------------|-----------------------------------------|------------------------------------------------------------------|-----------------------------------------------------------------------------|
| InputData                      | genenames.txt                           | Entrez ID to HGNC mapping                                        | [HUGO](https://www.genenames.org/download/custom/)                     |
| InputData                      | TPM                                     | Gene expression TPM values of all genes for DepMap cell lines.   | [DepMap Public 24Q2](https://plus.figshare.com/articles/dataset/DepMap_24Q2_Public/25880521/1) |
| InputData                      | SLKB_calculated_scores                  | Downloaded GI scores from SLKB                                   | [SLKB](https://slkb.osubmi.org/)                                             |
| InputData/Benchmarks           | deKegel_output.csv                      | Used as benchmark (DepMap Hits)                                   | [Source](https://www.cell.com/cell-systems/fulltext/S2405-4712(21)00329-X)                          |
| InputData/Benchmarks           | Koferle.xlsx                            | Used as benchmark (Köferle List)                                  | [Source](https://doi.org/10.1016/j.celrep.2022.110636)                         |
| InputData/Chymera              | Chymera.csv                             | Contains raw read counts from the CHyMErA study                    | [Source](https://doi.org/10.1038/s41587-020-0437-z)                             |
| InputData/Chymera              | Chymera_essential.csv                   | List of essential genes used in the CHyMErA study                 | [Source](https://doi.org/10.1038/s41587-020-0437-z)                             |
| InputData/Dede                 | counts.txt                              | Contains raw read counts from Dede et al. study                    | [Source](https://doi.org/10.1186/s13059-020-02173-2)                             |
| InputData/Dede                 | pan-species-control-essentials-50genes  | List of essential genes used in Dede et al. study                 | [Source](https://doi.org/10.1186/s13059-020-02173-2)                             |
| InputData/Dede                 | pan-species-control-nonessentials-50genes| List of non-essential genes used in Dede et al. study             | [Source](https://doi.org/10.1186/s13059-020-02173-2)                             |
| InputData/Ito                  | Count_data_ParalogV1.xlsx               | Raw read counts from the Ito et al. study                          | [Source](https://doi.org/10.1038/s41588-021-00967-z)                           |
| InputData/Ito                  | CEGv2                                   | Control essential genes used in Ito et al. study                  | [Source](https://doi.org/10.1038/s41588-021-00967-z)                           |
| InputData/Ito                  | NEGv1                                   | Non-essential genes used in Ito et al. study                      | [Source](https://doi.org/10.1038/s41588-021-00967-z)                           |
| InputData/Parrish              | GSE178179_pgPEN_counts_PC9.txt          | Contains raw read counts from Parrish et al. study                 | [Source](https://10.1016/j.celrep.2021.109597)                                  |
| InputData/Parrish              | GSE120703_d.expr                        | Gene expression TPM values from Parrish et al. study              | [Source](https://10.1016/j.celrep.2021.109597)                                  |
| InputData/Parrish              | AchillesCommonEssentialControls         | List of essential genes used in the Parrish et al. study          | [Source](https://plus.figshare.com/articles/dataset/DepMap_24Q2_Public/25880521/1) |
| InputData/Thompson             | raw_counts                              | Contains raw read counts from Thompson et al. study                | [Source](https://doi.org/10.1038/s41467-021-21478-9)                             |
| InputData/Thompson             | Guide_sequences.xlsx                    | Guide sequences used in Thompson et al. study                     | [Source](https://doi.org/10.1038/s41467-021-21478-9)                             |

# Running the Scripts
Once the input data is in place, run the scripts in the following folders:

- zdLFC Scripts
- Gemini Scripts
- Orthrus Scripts

Code for the Parrish score was obtained from the author through private communication. Once all the scripts have run, output folders will be automatically populated.

# Data Analysis
To analyse the output of each GI scoring method on each study, run the following python jupyter notebooks in the Analysis folder in the following order:

- Chymera Analysis
- DeDe Analysis
- ITO-Analysis
- Parrish-Analysis
- Thompson-Analysis

Once each notebook has run, run all cells in the following notebooks:

- Combine_all_results
- Combine_all_results-Filtered

These scripts will calculated ROCs and PR curves of all GI scores on each study/cell line.

# Figures
Run the scripts inside the Figures folder to reproduce the figures in the paper. 


| Filename                     | Figure in Manuscript               |
|------------------------------|-----------------------------------|
| Figure2.R                    | Figure 2                         |
| Figure3ab-S2ab.R             | Figure 3a, 3b, S2a, S2b         |
| Figure4-5-S3-S4.R            | Figure 4, 5, S3, S4             |
| Figure6ab.R                  | Figure 6a and 6b                |
| Figure7ab.R                  | Figure 7a, 7b                   |
| Figure7cd.R                  | Figure 7c, 7d                   |
