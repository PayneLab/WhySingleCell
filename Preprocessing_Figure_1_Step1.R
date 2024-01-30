library(tidyverse)
library(readxl)


## Import the data from the correct excel sheet
data_1 <- read_excel("1-s2.0-S0092867420310783-mmc3.xlsx", "H comparison to other studies")


## Clean up the dataframe
data_1 <- data_1[-1,]
colnames(data_1) <- data_1[1,]
data_1 <- data_1[-1,]

data_1 <- data_1 %>%
  select(ensembl_id, entrez_id, hgnc_name, hgnc_symbol, Our_enrichment_category, `Our_enriched tissue`)


## Select the two sets of important features
cleaned_data_1 <- data_1 %>% 
  filter(!is.na(Our_enrichment_category))

tissue_specific <- cleaned_data_1 %>%  
  filter(Our_enrichment_category == "prt_specific")
tissue_enriched <- cleaned_data_1 %>%
  filter(Our_enrichment_category == "prt_enriched_not_spec")
house_keeping <- cleaned_data_1 %>%
  filter(Our_enrichment_category == "prt_hk")

important_features_TS <- tissue_specific %>%
  select(ensembl_id)
important_features_TE <- tissue_enriched %>%
  select(ensembl_id)


## Write out the ensembl IDs to be used in a UniProt query
write.table(important_features_TS, "tissue_specific_features.csv", row.names=FALSE, sep=",")
write.table(important_features_TE, "tissue_enriched_features.csv", row.names=FALSE, sep=",")
