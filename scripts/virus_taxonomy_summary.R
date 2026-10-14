library(tidyverse)
library(readxl)
library(ggrepel)
library(forcats)
source("scripts/utility.R")

#### Prepare Data ####
dftpm <- readxl::read_xlsx("data/vOTU_table_tpm.xlsx")
dfcontig <- read.csv("data/merged_contig_summary_all.csv") 
dfmeta <- readxl::read_xlsx("data/sample_list.xlsx") |> 
  mutate(sample_source = str_extract(sample_title, "(?<=_)[^_]+$"))
sample_cols <- colnames(dftpm[,-1])

dfrel <- dftpm |> 
  mutate_at(vars(-vOTU_id), funs(.*100/sum(.))) 
dfmerge <- dfrel |> 
  merge(dfcontig, by.x = "vOTU_id", by.y = "contig_id")
dfrellong <- dfrel |>
  pivot_longer(cols = all_of(sample_cols),
               names_to  = "sample",
               values_to = "rel_abund")


#### Prepare prevalence abundance df per sample source ####
overall <- dfrellong |>
  dplyr::group_by(vOTU_id) |>
  dplyr::summarise(
    n_samples          = n_distinct(sample),
    n_detected         = sum(rel_abund > 0),
    prevalence         = 100 * n_detected / n_samples,
    mean_abund         = mean(rel_abund, na.rm = TRUE),
    median_abund       = median(rel_abund, na.rm = TRUE),
    max_abund          = max(rel_abund, na.rm = TRUE),
    .groups            = "drop"
  )
per_source <- dfrellong |>
  dplyr::left_join(dfmeta |> select(sample_id, sample_source),
            by = c("sample" = "sample_id")) |>
  dplyr::group_by(vOTU_id, sample_source) |>
  dplyr::summarise(
    n_samples  = n_distinct(sample),
    n_detected = sum(rel_abund > 0),
    prevalence = 100 * n_detected / n_samples,
    mean_abund = mean(rel_abund),
    median_abund = median(rel_abund, na.rm = TRUE),
    max_abund = max(rel_abund, na.rm = TRUE),
    .groups    = "drop"
  )
per_source_wide <- per_source |>
  pivot_wider(
    names_from  = sample_source,
    values_from = c(n_samples, n_detected, prevalence,
                    mean_abund, median_abund, max_abund),
    names_glue  = "{.value}_{sample_source}"
  )
prev_threshold <- 95
abund_threshold <- 0.04
prev_abund <- overall |>
  left_join(per_source_wide, by = "vOTU_id") |>
  left_join(
    dfcontig,
    by = c("vOTU_id" = "contig_id")
  ) |>
  mutate(
    category = case_when(
      prevalence >= prev_threshold & mean_abund >= abund_threshold ~ "Core",
      prevalence >= prev_threshold & mean_abund <  abund_threshold ~ "Prevalent, Low Abundance",
      prevalence <  prev_threshold & mean_abund >= abund_threshold ~ "Sample-specific, Abundant",
      TRUE                                  ~ "Rare"
    ),
    category = factor(category,
                      levels = c("Core",
                                 "Prevalent, Low Abundance",
                                 "Sample-specific, Abundant",
                                 "Rare"))
  )
prev_abund <- prev_abund |>
  dplyr::mutate(
    core_Rumen = prevalence_Rumen >= prev_threshold & mean_abund_Rumen >= abund_threshold,
    core_Feces = prevalence_Feces >= prev_threshold & mean_abund_Feces >= abund_threshold,
    core_Both  = core_Rumen & core_Feces,
    core_only_Rumen = core_Rumen & !core_Feces,
    core_only_Feces = core_Feces & !core_Rumen, 
    core_category = if_else(core_Both, "Both", NA), 
    core_category = if_else(is.na(core_category) & core_Rumen, "Rumen", core_category),
    core_category = if_else(is.na(core_category) & core_Feces, "Feces", core_category),
  )
prev_abund |> dplyr::count(core_Rumen, core_Feces)
prev_abund |> filter(core_Both) |> nrow()

write.csv(prev_abund, "data/prevalence_abundance.csv")

prev_abund_hit <- prev_abund |> 
  filter(core_Both == TRUE | core_Feces == TRUE | core_Rumen == TRUE)

write.csv(prev_abund_hit, "data/prevalence_abundance_top_prev90_abund4.csv")

#### Plot prevalence abundance ####
labels_df <- prev_abund |>
  dplyr::mutate(tax_label = if_else(phagcn_Genus == "-" & is.na(genomad_family), "", tax_label), 
                tax_label = str_remove(tax_label, "^.+\\:\\s")) |>
  dplyr::filter(category %in% c("Core", "Sample-specific, Abundant")) |>
  dplyr::group_by(sample_source, category) |>
  dplyr::arrange(dplyr::desc(prevalence * mean_abund), .by_group = TRUE) |>
  dplyr::slice_head(n = 10) |>
  dplyr::ungroup()

cat_colors <- c(
  "Core"                      = "royalblue3",
  "Prevalent, Low Abundance"  = "cadetblue4",
  "Sample-specific, Abundant" = "tan4",
  "Rare"                      = "grey80"
)

prev_abund |>
  ggplot(aes(x = mean_abund*100, y = prevalence)) +
  geom_hline(yintercept = prev_threshold,
             linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  geom_vline(xintercept = abund_threshold*100,
             linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  geom_point(data = dplyr::filter(prev_abund, category == "Rare"),
             colour = cat_colors["Rare"], size = 0.1, alpha = 0.5) +
  geom_point(data = dplyr::filter(prev_abund, category != "Rare"),
             aes(colour = category, shape = core_category), size = 1, alpha = 0.85) +
  geom_label_repel(
    data = labels_df,
    aes(label = tax_label, colour = category),
    size = 2.5,
    box.padding    = 0.6,      # more space around each label box
    point.padding  = 0.3,      # more space between label and its point
    min.segment.length = 0,    # always draw the connector line
    segment.size   = 0.25,
    segment.colour = "grey50",
    max.overlaps   = Inf,      # never drop a label
    force          = 2,        # stronger repulsion
    force_pull     = 0.5,      # weaker pull toward the point
    max.time       = 2,        # more iterations to resolve overlaps
    max.iter       = 100000,
    direction      = "both",   # let labels move in any direction
    seed           = 42,       # reproducible layout
    show.legend    = FALSE
  ) +
  scale_x_log10(
    labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  ) +
  scale_colour_manual(values = cat_colors, name = NULL) +
  scale_shape_discrete(na.translate = FALSE) +
  facet_wrap(~ sample_source, ncol=1) +
  labs(
    x = "Mean Relative Abundance (%)",
    y = "Prevalence (%)"
  ) +
  theme_bw() + 
  theme8 +
  theme(legend.position = "right") + 
  guides(colour = guide_legend(title = "Core Category", override.aes = list(size = 3)), 
         shape = guide_legend(title = "Core in...", override.aes = list(size = 2)))


#### Taxonomy assigned percentage ####
tax_long <- dfcontig |>
  dplyr::transmute(
    contig_id,
    Genus   = dplyr::if_else(is.na(phagcn_Genus) | phagcn_Genus == "", "-", phagcn_Genus),
    Family  = dplyr::if_else(is.na(genomad_family)  | genomad_family  == "", "-", genomad_family),
    Order   = dplyr::if_else(is.na(genomad_order)   | genomad_order   == "", "-", genomad_order),
    Class   = dplyr::if_else(is.na(genomad_class)   | genomad_class   == "", "-", genomad_class),
    Phylum  = dplyr::if_else(is.na(genomad_phylum)  | genomad_phylum  == "", "-", genomad_phylum),
    Realm   = dplyr::if_else(is.na(genomad_realm)   | genomad_realm   == "", "-", genomad_realm),
    Kingdom = dplyr::if_else(is.na(genomad_kingdom) | genomad_kingdom == "", "-", genomad_kingdom)
  ) |>
  tidyr::pivot_longer(cols = -contig_id, names_to = "level", values_to = "taxon") |>
  dplyr::mutate(
    level  = factor(level, levels = c("Realm", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus")),
    status = dplyr::if_else(taxon == "-", "Unassigned", "Assigned")
  )

tax_counts <- tax_long |>
  dplyr::count(level, status) |>
  dplyr::mutate(status = factor(status, levels = c("Assigned", "Unassigned")))

tax_counts |>
  ggplot(aes(x = level, y = n, fill = status)) +
  geom_col(width = 0.7, colour = "black", linewidth = 0.2, position = "stack") +
  scale_fill_manual(values = c("Assigned" = "#2C7FB8", "Unassigned" = "grey75")) +
  labs(x = "Taxonomic level", y = "Number of vOTUs", fill = NULL) +
  theme10 +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "right",
    plot.margin = margin(c(rep(20, 4)))
  )

### Taxonomy classification per level ####
prev_threshold  <- 60
abund_threshold <- 1

class_summary <- prev_abund |>
  dplyr::group_by(genomad_class) |>
  dplyr::summarise(
    mean_abund_Feces = sum(mean_abund_Feces, na.rm = TRUE),
    mean_abund_Rumen = sum(mean_abund_Rumen, na.rm = TRUE),
    prevalence_Feces = mean(prevalence_Feces, na.rm = TRUE),
    prevalence_Rumen = mean(prevalence_Rumen, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(
    cols      = -c(genomad_class),
    names_to  = c(".value", "source"),
    names_pattern = "(.+)_(Feces|Rumen)"
  ) |>
  dplyr::filter(!is.na(source))

class_summary <- class_summary |>
  dplyr::mutate(
    genomad_class = if_else(is.na(genomad_class), "Unknown", genomad_class), 
    is_highlighted = mean_abund > abund_threshold | prevalence > prev_threshold,
    class_group    = dplyr::if_else(is_highlighted, genomad_class, "Other")
  )

highlight_set <- class_summary |>
  dplyr::filter(is_highlighted) |>
  dplyr::pull(genomad_class) |>
  unique() |>
  sort()

class_summary <- class_summary |>
  dplyr::mutate(
    class_group = factor(class_group, levels = c(highlight_set, "Other"))  ) 

class_summary_top    <- class_summary |> dplyr::filter(is_highlighted)
class_summary_nottop <- class_summary |> dplyr::filter(!is_highlighted)

write.csv(class_summary, "data/class_summary.csv")

class_summary |>
  ggplot(aes(x = mean_abund, y = prevalence)) +
  geom_hline(yintercept = prev_threshold, linetype = "dashed",
             colour = "grey40", linewidth = 0.3) +
  geom_vline(xintercept = abund_threshold, linetype = "dashed",
             colour = "grey40", linewidth = 0.3) +
  geom_point(data = class_summary_nottop,
             colour = "grey30", size = 1.5, alpha = 0.6) +
  geom_point(data = class_summary_top,
             aes(colour = class_group), size = 2.5, alpha = 0.9) +
  geom_label_repel(
    data = class_summary_top,
    aes(label = class_group, colour = class_group),
    size = 2.5,
    box.padding        = 0.6,
    point.padding      = 0.3,
    min.segment.length = 0,
    segment.size       = 0.25,
    segment.colour     = "grey50",
    max.overlaps       = Inf,
    force              = 2,
    force_pull         = 0.5,
    max.time           = 2,
    max.iter           = 100000,
    direction          = "both",
    seed               = 42,
    show.legend        = FALSE
  ) +
  scale_x_log10(
    breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 80),
    labels = function(x) formatC(signif(x, 2), format = "fg", big.mark = ",")
  ) +
  scale_y_continuous(
    breaks = c(0, 25, 50, 60, 75, 100),
    limits = c(0, 110)
  ) +
  scale_colour_manual(
    values = colorRampPalette(pal_aaas("default")(10))(length(highlight_set)),
    name   = "geNomad class"
  ) +
  facet_wrap(~ source, nrow = 1) +
  labs(
    x = "Mean Relative Abundance (%)",
    y = "Prevalence (%)"
  ) +
  theme11 +
  theme(
    legend.position = "right",
    plot.margin     = margin(rep(20, 4)),
    panel.spacing = unit(0.2, "cm"), 
    strip.background = element_blank(), 
  ) +
  guides(colour = guide_legend(title = "geNomad class",
                               override.aes = list(size = 3)))

# Lifestyle temparate vs lytic
# Core, Rumen Core, Feces Core, Host-specific
# Venn Diagram of shared vOTUs between rumen and feces