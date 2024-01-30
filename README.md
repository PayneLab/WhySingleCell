# WhySingleCell

This repository contains the code and files for the calculations and figures from "Are we there yet? Assessing the readiness of single-cell proteomics to answer biological hypotheses"

## File Information

- ***Preprocessing_Figure_1_Step1.R***: R script that produces *tissue_specific_features.csv* and *tissue_enriched_features.csv* for next preprocessing step. Requires Table S2 from ["A Quantitative Proteome Map of the Human Body"](https://doi.org/10.1016/j.cell.2020.08.036) as input (*1-s2.0-S0092867420310783-mmc3.xlsx*)

- ***Preprocessing_Figure_1_Step2.py***: python script that produces *ensembl_to_uniprot_TS.tsv* and *ensembl_to_uniprot_TE.tsv* for next step

- ***Figure_1.R***: R script that produces *Figure_1.png*. Requires *proteins.tsv* from [PayneLab/singlecell_wwa](https://github.com/PayneLab/singlecell_wwa/tree/main/data) as input

- ***Figure_2ab.R***: R script that produces *Figure _2a.png* and *Figure_2b.png*

- ***Classification - Number of Cells.ipynb***: python notebook that produces the calculated numbers in the Classification - Number of Cells section of the paper

## Recreating the Figures (without preprocessing)

1. Run *Figure_1.R*
2. Run *Figure_2ab.R*

## Recreating the Figures (with preprocessing)

1. Download Table S2 from ["A Quantitative Proteome Map of the Human Body"](https://doi.org/10.1016/j.cell.2020.08.036) (*1-s2.0-S0092867420310783-mmc3.xlsx*) and add it to the folder containing the files from this repository
2. Run *Preprocessing_Figure_1_Step1.R*
3. Run *Preprocessing_Figure_1_Step2.py* - NOTE: because this code queries UniProt's API, changes to the API and/or database may result in different output files than those found in this repository. For guaranteed exact recreation of Figure 1, skip this step.
4. Run *Figure_1.R*
5. Run *Figure_2ab.R*

## Recreating the Calculations

1. **Unadjusted for Dropout**: Run all cells in *Classification - Number of Cells.ipynb*
2. **Adjusted for Dropout**: In the first cell in *Classification - Number of Cells.ipynb*, set d = 1 and run all cells

