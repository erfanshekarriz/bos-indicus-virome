library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)


dfcontig <- readxl::read_xlsx("data/classification_summary_vOTUs.xlsx")
dfmeta <- readxl::read_xlsx("data/sample_list.xlsx") |>
  mutate(sample_source = str_extract(sample_title, "(?<=_)[^_]+$"))
dfgc <- readxl::read_xlsx("data/GC_content_vOTUs_tmp.xlsx")
dftax <- readxl::read_xlsx("data/merged_taxonomy.xlsx")
dfphatyp <- readxl::read_xlsx("data/phatyp_prediction.xlsx")
dfhost <- readxl::read_xlsx("data/merged_host.xlsx")


base <- dfcontig |>
  mutate(
    Length        = if_else(checkv_provirus == "No", genomad_length, checkv_proviral_length),
    Length        = as.numeric(Length),
    virlength     = if_else(!is.na(checkv_proviral_length), checkv_proviral_length, checkv_contig_length),
    virlengthkb   = virlength / 1000,
    virallength   = if_else(str_detect(type, "Provirus"), checkv_proviral_length, genomad_length),
    virallengthkb = virallength / 1000
  ) |>
  left_join(dfmeta, by = "sample_id") |>
  left_join(dfgc,   by = "contig_id") |>
  merge(dftax, by.x = "contig_id", by.y = "id") |> 
  merge(dfphatyp, by.x = "contig_id", by.y = "Accession") |> 
  merge(dfhost, by.x = "contig_id", by.y = "id") |> 
  dplyr::mutate(
    completeness_key = case_when(
      is.na(checkv_completeness) ~ "unknown",
      checkv_completeness == 100 ~ "complete",
      checkv_completeness >  90  ~ "90_100",
      checkv_completeness >  50  ~ "50_90",
      checkv_completeness >=  0  ~ "under_50",
      TRUE                       ~ "unknown"
    ), 
    tax_label = if_else(phagcn_Genus != "-", 
                        paste("Genus:", phagcn_Genus), 
                        paste("Family:", genomad_family)), 
    tax_label = if_else(!str_detect(tax_label, "NA"), tax_label, 
                       paste("Order:", genomad_order)), 
    tax_label = if_else(!str_detect(tax_label, "NA"), tax_label, 
                        paste("Class:", genomad_class)), 
    tax_label = if_else(!str_detect(tax_label, "NA"), tax_label, 
                        paste("Phylum:", genomad_phylum)),
    tax_label = if_else(!str_detect(tax_label, "NA"), tax_label, 
                        paste("Kingdom:", genomad_kingdom)), 
    tax_label = if_else(!str_detect(tax_label, "NA"), tax_label, 
                        paste("Realm:", genomad_realm)), 
    tax_label = if_else(!str_detect(tax_label, "NA"), tax_label, 
                        paste("Virus:", genomad_virus)))


base$tax_label |> count() 

counts <- base |> dplyr::count(completeness_key)
fmt    <- function(n) formatC(n, format = "d", big.mark = ",")
n_of   <- function(k) fmt(counts$n[counts$completeness_key == k])

label_map <- c(
  unknown  = sprintf("Unknown completeness\n(n=%s)", n_of("unknown")),
  under_50 = sprintf("<50%% complete\n(n=%s)",       n_of("under_50")),
  `50_90`  = sprintf("50-90%% complete\n(n=%s)",     n_of("50_90")),
  `90_100` = sprintf(">90%% complete\n(n=%s)",       n_of("90_100")),
  complete = sprintf("Complete\n(n=%s)",             n_of("complete"))
)

level_order <- c("complete", "90_100", "50_90", "under_50", "unknown")

dfwrite <- base |>
  mutate(
    completeness_cat = recode(completeness_key, !!!as.list(label_map)),
    completeness_cat = factor(completeness_cat, levels = unname(label_map[level_order]))
  )

dfwrite$completeness_cat |> count()


write.csv(dfwrite, "data/merged_contig_summary_all.csv", row.names = FALSE)

