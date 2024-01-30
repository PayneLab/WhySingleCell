library(tidyverse)
library(ragg)
library(viridis)



## Read in the data
### (1) list of TS proteins
### (2) list of TE proteins
### (3) protein information table from Hannah and Caleb and add TS/TE info
TS_ids <- read_tsv("ensembl_to_uniprot_TS.tsv") %>%
  filter(Reviewed == "reviewed") %>%
  filter(Organism == "Homo sapiens (Human)") %>%
  pull(Entry) %>%
  unique() %>%
  sort() %>%
  unlist()

TE_ids <- read_tsv("ensembl_to_uniprot_TE.tsv") %>%
  filter(Reviewed == "reviewed") %>%
  filter(Organism == "Homo sapiens (Human)") %>%
  pull(Entry) %>%
  unique() %>%
  sort() %>%
  unlist()

proteins <- read_tsv("proteins.tsv") %>%
  mutate(present_sc_new = select(., new_single_cell_F1:new_single_cell_F19) %>% rowSums(na.rm = TRUE)) %>%
  mutate(present_sc_old = select(., old_single_cell_F3:old_single_cell_F5) %>% rowSums(na.rm = TRUE)) %>%
  mutate(logscale = log10(bulk_abundance)) %>%
  mutate(tissue_specific = uniprot_id %in% TS_ids) %>%
  mutate(tissue_enriched = uniprot_id %in% TE_ids)


## Create bins for the x-axis of the plot
bins <- seq(1.1, 7.9, 0.1)
labels <- bins[-69] %>% paste()


## Create filtered data frames
new_sc = proteins %>%
  filter(present_sc_new > 0) %>%
  mutate(bins = factor(cut(logscale, breaks = bins, labels = labels), levels = labels))

old_sc <- proteins %>%
  filter(present_sc_old > 0) %>%
  mutate(bins = factor(cut(logscale, breaks = bins, labels = labels), levels = labels))

bulk <- proteins %>%
  filter(!is.na(bulk_abundance)) %>%
  mutate(bins = factor(cut(logscale, breaks = bins, labels = labels), levels = labels))

TS <- proteins %>%
  filter(tissue_specific == TRUE) %>%
  mutate(bins = factor(cut(logscale, breaks = bins, labels = labels), levels = labels))

TE <- proteins %>%
  filter(tissue_enriched == TRUE) %>%
  mutate(bins = factor(cut(logscale, breaks = bins, labels = labels), levels = labels))


## Value counts
new_sc_counts <- new_sc %>%
  group_by(bins, .drop = FALSE) %>%
  summarize(Count_New = n()) %>%
  arrange(desc(Count_New)) %>%
  filter(!is.na(bins))

old_sc_counts <- old_sc %>%
  group_by(bins, .drop = FALSE) %>%
  summarize(Count_Old = n()) %>%
  arrange(desc(Count_Old)) %>%
  filter(!is.na(bins))

bulk_counts <- bulk %>%
  group_by(bins, .drop = FALSE) %>%
  summarize(Count_Bulk = n()) %>%
  arrange(desc(Count_Bulk)) %>%
  filter(!is.na(bins))

TS_counts <- TS %>% 
  group_by(bins, .drop = FALSE) %>%
  summarize(Count_TS = n()) %>%
  arrange(desc(Count_TS)) %>%
  filter(!is.na(bins))

TE_counts <- TE %>% 
  group_by(bins, .drop = FALSE) %>%
  summarize(Count_TE = n()) %>%
  arrange(desc(Count_TE)) %>%
  filter(!is.na(bins))


## Combine counts tables into two: counts for the line distributions and 
##  counts for the features in the bar chart
combined_distributions <- new_sc_counts %>%
  full_join(old_sc_counts) %>%
  full_join(bulk_counts) %>%
  mutate(bins = as.numeric(as.character(bins))) %>%
  arrange(bins) %>%
  pivot_longer(Count_New:Count_Bulk, names_to = "dataset", values_to = "counts")

combined_features <- TS_counts %>%
  full_join(TE_counts) %>%
  mutate(bins = as.numeric(as.character(bins))) %>%
  arrange(bins) %>%
  mutate(fill = "filler")


## Create a line+bar plot
ggplot() +
  geom_bar(combined_features, mapping = aes(x = bins, y = Count_TE, fill = "Tissue-Enriched"), stat = "identity") +
  geom_bar(combined_features, mapping = aes(x = bins, y = Count_TS, fill = "Tissue-Specific"), stat = "identity") +
  geom_line(combined_distributions, mapping = aes(x = bins, y = counts, color = dataset), size = 1) +
  theme_bw() +
  labs(x = "Copy Number (log10)", y = "Number of Proteins", color = "Datasets", fill = "Featuresets") +
  scale_x_continuous(breaks = seq(1, 8, 1)) +
  scale_y_continuous(breaks = seq(0, 500, 100)) +
  scale_fill_manual(name = "Featuresets", breaks = c("Tissue-Enriched", "Tissue-Specific"), values=c("Tissue-Enriched" = "gray45", "Tissue-Specific" = "black")) +
  scale_color_viridis(discrete = TRUE, labels = c("Bulk Proteomics", "SCP - 2023", "SCP - 2021")) + 
  guides(linewidth = "none",
         fill = guide_legend(order = 2),
         color = guide_legend(order = 1))


## Save the plot
ggsave("Depth Classification Distributions.png", dpi = 600, units = "in", height = 5, width = 7)


