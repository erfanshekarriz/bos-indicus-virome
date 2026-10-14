library(tidyverse)
library(ggsci)
library(readxl)
library(ggpubr)
library(scales)
library(ggvenn)
library(ggridges)
source("scripts/utility.R")

# CLEANUP DATA ####
library(dplyr)
library(stringr)
library(readxl)

dfplot <- read.csv("data/merged_contig_summary_all.csv") 

# GC vs. Length

dfplot |>
  ggplot(aes(x = GC_percent, y = virallength, colour = sample_source, fill = sample_source)) +
  geom_point(size = 1) + 
  scale_y_log10(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)) +
  scale_fill_aaas() +
  scale_colour_aaas() +
  xlab("GC Content (%)") +
  ylab("Viral Contig Length (bps)") +
  theme11 +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = "Sample Source", override.aes = list(size = 2)),
         fill = "none")


# Rumen vs. Feasces Violin Plot

dfplot |>
  ggplot(aes(x = sample_source, y = virallength, fill = completeness_cat)) +
  geom_boxplot(outlier.shape = 1, outlier.size = 0.4,
               outlier.colour = "grey80",
               linewidth = 0.3,
               position = position_dodge(width = 0.75),
               width = 0.7) +
  scale_y_log10(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)) +
  scale_fill_uchicago() +
  xlab("Sample source") +
  ylab("Viral contig length (bps)") +
  theme11 +
  guides(fill = guide_legend(title = "Completeness category"))

# Completeness Box Plot ####
y_order <- unname(label_map[level_order])   # e.g. "Unknown…", "<50%…", "50-90%…", ">90%…", "Complete…"

dfplot %>%
  mutate(completeness_cat = factor(completeness_cat, levels = y_order)) %>%
  ggplot(aes(y = completeness_cat, x = Length / 1000)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(aes(fill = completeness_cat),
               outlier.shape = 1,
               outlier.size = 0.5,
               lwd = 0.5,
               alpha = 0.8,
               outlier.color = "grey90") +
  scale_x_log10(limits = c(1, NA)) +
  scale_y_discrete(limits = rev(y_order)) +
  xlab("vOTU\nGenome Size (kbps)") +
  scale_fill_uchicago() +
  theme5 +
  ggtitle("Viral genome retrieval summary\norganised according to CheckV genome completeness") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(face = "plain", size = 9),
    axis.title.x = element_text(face = "plain", size = 9, vjust = -1),
    plot.title = element_text(size = 10, hjust = 0.5, color = "grey20")
  )

# viral length boxplots ####
dfplot %>%
  mutate(completeness_cat = factor(completeness_cat, levels = y_order)) %>%
  filter(checkv_completeness > 50,
         type %in% c("Virus", "Provirus (geNomad+CheckV)")) %>%
  mutate(type = if_else(type == "Virus", "Free-living Virus", type)) %>%
  mutate(type = factor(type,
                       levels = c("Free-living Virus",
                                  "Provirus (geNomad+CheckV)"))) %>%
  ggplot(aes(y = type, x = virallengthkb)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(aes(fill = type),
               outlier.shape = 1,
               outlier.size  = 0.5,
               lwd           = 0.5,
               outlier.color = "grey90") +
  stat_compare_means(
    comparisons     = list(c("Free-living Virus", "Provirus (geNomad+CheckV)")),
    method          = "wilcox.test",
    p.adjust.method = "BH",
    label           = "p.signif",
    hide.ns         = FALSE,          
    size            = 4, 
    vjust = 2,
  ) +
  scale_x_log10(limits = c(1, NA)) +
  scale_fill_nejm() +
  facet_wrap(~completeness_cat, nrow = 1, strip.position = "top") +
  xlab("Viral contig length (kb)") +
  ggtitle("Viral contig length of pro-viruses\n(host removed) vs. free-living viruses\n") +
  theme5 +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 10, hjust = 0.5, color = "grey20")
  ) +
  guides(fill = guide_legend(title = "", size = 10, 
                             nrow = 2))


# genomad topology
complete_df <- dfplot %>%
  filter(str_detect(completeness_cat, "^Complete")) %>%
  mutate(topology = str_split(genomad_topology, ",\\s*")) %>%
  unnest(topology) %>%
  mutate(topology = str_squish(topology)) %>%
  filter(!is.na(topology) & topology != "")

topo_summary <- complete_df %>%
  dplyr::count(sample_source, topology, name = "n") %>%
  dplyr::group_by(sample_source) %>%
  dplyr::mutate(
    frac        = n / sum(n),
    label_short = paste0(round(100 * frac, 1), "%") # using round to check exact decimal
  ) %>%
  ungroup()

topo_summary %>%
  ggplot(aes(x = "", y = frac, fill = topology)) +
  geom_col(width = 1, linewidth = 0.4, position = "stack") +
  coord_polar(theta = "y", start = 0, clip = "off") +
  scale_fill_simpsons() +
  geom_text(aes(label = label_short),
            position = position_stack(vjust = 0.5),
            size = 2.6, colour = "black", fontface = "bold") +
  # scales = "free" ensures each pie chart independently scales to a full 100% circle
  facet_wrap(~ sample_source, ncol = 3, drop = FALSE, scales = "free") +
  theme_void() +
  theme(
    plot.title      = element_text(size = 10, hjust = 0.5, color = "grey20"),
    strip.text      = element_text(size = 9, face = "bold"),
    legend.title    = element_text(size = 9, face = "bold"),
    legend.text     = element_text(size = 8.5),
    legend.position = "bottom",
    plot.margin     = unit(c(1.2, 1.2, 1.2, 1.2), "cm")
  ) + 
  guides(fill = guide_legend(title="Topology"))


# Stacked bar-plot
topo_summary |> 
  ggplot(aes(x=frac, y=sample_source, fill=topology)) + 
  geom_col() +
  geom_text(aes(label = label_short),
            position = position_stack(vjust = 0.5),
            size = 2.6, colour = "black", fontface = "bold") +
  theme9 + 
  theme(plot.margin = margin(rep(1, 4), unit = "cm"), 
        axis.text.y = element_text(face = "bold", size = 8), 
        legend.position = "right") + 
  guides(fill = guide_legend(title="Topology")) + 
  scale_fill_flatui() 
  


# n-genes
dfplot |> 
  mutate(completeness_cat = factor(completeness_cat, levels = y_order)) %>%
  ggplot(aes(x=genomad_n_genes, fill=completeness_cat)) + 
  geom_density(alpha = 0.8) + 
  scale_x_log10(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)) +
  theme10 + 
  theme(plot.margin = margin(rep(1, 4), unit = "cm"), 
        axis.text.y = element_blank(), 
        legend.position = "right") + 
  guides(fill = guide_legend(title="Topology")) + 
  scale_fill_uchicago() + 
  xlab("Log Number of Genes") + 
  ylab("")
