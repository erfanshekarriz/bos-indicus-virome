# THEMES ####
theme1 <-
  theme_bw() + 
  theme(axis.title.x=element_text(face="italic", size=9, vjust=-3), 
        axis.title.y=element_text(face="italic", size=9, vjust=4), 
        axis.text=element_text(size = 6, face ="bold"), 
        panel.border = element_rect(linewidth = 0),
        legend.text=element_text(size = 8, face="bold"), 
        legend.background=element_rect(fill= ggplot2::alpha('grey', 0.2), 
                                       color = "grey90", linetype = 2, linewidth = 0),
        legend.key = element_rect(fill = "transparent"), 
        legend.key.size = unit(0.2, "cm"),
        legend.title=element_blank(),
        legend.position = "inside", 
        legend.position.inside = c(0.5, 0.2),
        plot.margin=unit(c(0.5,1,1,1), "cm"),
        plot.background = element_blank(),
        panel.grid.minor=element_line(color="grey92"), 
        panel.grid.major=element_line(color="grey92"))


theme2 <- 
  theme_bw() + 
  theme(axis.title.x=element_text(face="italic", size=8, vjust=0.5), 
        axis.title.y=element_text(face="italic", size=8, vjust=0.5), 
        axis.text.y = element_blank(), 
        axis.text= element_blank(), 
        panel.border = element_rect(linewidth = 0.5),
        panel.background = element_blank(), 
        legend.text=element_text(size = 8, face="bold"), 
        legend.key.size = unit(0.2, "cm"),
        legend.title=element_blank(),
        legend.position = "none", 
        plot.background = element_blank(),
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(), 
        plot.margin = margin(1, 1, 1, 1), 
        plot.title = element_text(hjust = 0.5, vjust=5, face="italic", color="grey40", size=10), 
  ) 

theme3 <- 
  theme_bw() + 
  theme(axis.title.x=element_text(face="italic", size=8, vjust=0.5), 
        axis.title.y=element_text(face="italic", size=8, vjust=0.5), 
        axis.text.y=element_text(size = 8, face ="bold.italic", colour = "grey40"), 
        axis.text.x=element_text(size = 8, face ="italic", colour = "black"), 
        panel.border = element_rect(colour = "grey", linewidth = 1),
        panel.background = element_blank(), 
        legend.text= element_text(size = 10, face="plain"), 
        legend.spacing.y = unit(0.1, 'cm'), 
        legend.key.size = unit(0.5, "cm"),
        legend.margin = margin(0,0,0,0, unit="cm"),
        legend.title=element_blank(),
        strip.text = element_blank(),
        legend.position = "right", 
        legend.background = element_blank(), 
        plot.background = element_blank(),
        panel.grid.minor=element_blank(), 
        panel.grid.major.x=element_blank(), 
        panel.grid.major.y=element_line(color="grey92"),
        plot.title = element_text(hjust = 0.5, vjust=5, face="italic", color="grey40", size=10), 
        plot.margin = margin(1, 1, 1, 1))

theme4 <- 
  theme_classic() +
  theme(axis.title.x=element_text(face="plain", size=12, vjust=0), 
        axis.title.y=element_text(face="plain", size=12, vjust=0),
        axis.text.y=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), 
        # axis.text.y=element_text(size = 10, face ="italic", colour = "black"), 
        # axis.text.x=element_text(size = 10, face ="italic", colour = "black"), 
        panel.background = element_blank(), 
        legend.text=element_text(size = 8, face="bold"), 
        legend.key.size = unit(0.2, "cm"),
        legend.title=element_blank(),
        legend.position = "none", 
        legend.background = element_blank(), 
        plot.background = element_blank(),
        panel.grid.minor=element_blank(), 
        panel.grid.major.x=element_blank(), 
        panel.grid.major.y=element_line(color="grey92"), 
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, unit = "cm"))


theme5 <- 
  theme_bw() + 
  theme(strip.background = element_blank(), 
        strip.text.y =element_text(face = "italic", size = 9), 
        strip.text.x =element_text(face = "italic", size = 9), 
        axis.title.y =element_blank(), 
        axis.ticks = element_line(linewidth = 0.3), 
        axis.title.x =element_blank(),
        # axis.title.x =element_text(size = 10, face ="italic", vjust = 4), 
        axis.text.x = element_text(size = 8, face ="italic"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.border = element_rect(colour="grey20", linewidth = 1), 
        panel.background = element_blank(), 
        plot.background = element_blank(), 
        plot.margin = unit(rep(0.5, 4), "cm"),
        panel.grid = element_blank(), 
        panel.spacing.x = unit(0.20, "lines"), 
        panel.spacing.y = unit(0.05, "lines")) 


theme6 <- 
  theme_bw() + 
  theme(axis.title.x=element_text(face="italic", size=8, vjust=0.5), 
        axis.title.y=element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text=element_text(size = 6, face ="italic"), 
        panel.border = element_rect(linewidth = 1, colour = "grey"),
        panel.background = element_blank(), 
        legend.text=element_text(size = 8, face="bold"), 
        legend.key.size = unit(0.2, "cm"),
        legend.title=element_blank(),
        legend.position = "none", 
        plot.background = element_blank(),
        panel.grid.minor=element_blank(), 
        panel.grid.major.y=element_line(colour = "grey80"),
        panel.grid.major.x = element_blank(), 
        plot.margin = margin(1, 1, 1, 1)) 

theme7 <- 
  theme_bw() + 
  theme(axis.title.x=element_text(face="italic", size=8, vjust=0.5), 
        axis.title.y=element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text=element_text(size = 6, face ="italic"), 
        panel.border = element_rect(linewidth = 0, colour = "grey"),
        panel.background = element_blank(), 
        legend.text=element_text(size = 10, face="plain"), 
        legend.spacing.y = unit(0.1, 'cm'), 
        legend.background = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        legend.margin = margin(0,0,0,0, unit="cm"),
        legend.title=element_blank(),
        legend.position = "none", 
        plot.background = element_blank(),
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_line(colour = "grey80"), 
        plot.margin = margin(1, 1, 1, 1))

theme8 <- theme_bw() + theme(axis.title.x=element_text(face="bold", size=8, vjust=-3), 
        axis.title.y=element_text(face="bold", size=8, vjust=5), 
        axis.text.y=element_text(size = 6, face ="bold.italic", color="black"), 
        axis.text.x=element_text(size = 6, face ="bold", color="black"), 
        legend.position = "top", 
        # legend.position.inside = c(0.5,0.8), 
        legend.text=element_text(size = 8, face="bold"), 
        legend.box.background = element_rect(fill = "white", 
                                             linewidth = 0.5, colour = "black"), 
        legend.background=element_blank(),
        legend.title=element_blank(),
        legend.key.size = unit(0.01, "cm"),      # Reduce the size of legend keys
        legend.spacing.y = unit(0.01, "cm"),
        panel.border=element_rect(colour="black", linewidth = 1), 
        panel.background=element_blank(), 
        plot.background=element_blank(), 
        plot.margin=unit(c(1,1,1,1), "cm"),
        panel.spacing.y = unit(0.4, "lines"), 
        panel.spacing.x = unit(0.05, "lines"))

theme9 <- 
  theme6 +
  theme(
    axis.text.y = element_text(size = 6, face ="bold", hjust = 1), 
    legend.position = "top", 
    axis.title.y = element_blank(), 
    plot.margin = margin(1,0,0,0, unit="cm"), 
    strip.background = element_blank(), 
    strip.text.y = element_blank(), 
    panel.spacing = unit(0, "lines"), 
    strip.clip = "off")

theme10 <- 
  theme2 + 
  theme(axis.text.x = element_text(face="plain", size=8),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = margin(1,0,0,0, unit="cm"), 
        strip.background = element_blank(), 
        strip.text.y = element_text(size = 6, face = "bold"), 
        panel.spacing = unit(0, "lines"), 
        strip.clip = "off")


theme11 <-   theme_minimal() +
  theme(plot.title = element_text(size = 9,  hjust = 0.5, color = "grey20"), 
        axis.title.x = element_text(face = "bold", size = 9, vjust = -2), 
        axis.title.y = element_text(face = "bold", size = 9, vjust = 3), 
        legend.key.size = unit(0.2, "cm"), 
        legend.title = element_text(size = 9, face = "plain"), 
        legend.text = element_text(size = 8),
        legend.background = element_blank(), 
        plot.margin = margin(rep(1, 4), unit = "cm"), 
        plot.background = element_blank(), 
        panel.background = element_blank()) + 
  theme(
    panel.background  = element_rect(colour = "black", linetype = "solid",
                                     fill = "grey97", linewidth = 1),
    panel.grid.major  = element_line(colour = "grey85", linewidth = 0.3),
    panel.grid.minor  = element_line(colour = "grey85", linewidth = 0.15),
    strip.background  = element_rect(colour = "black", linetype = "solid",
                                     fill = "grey97", linewidth = 0.5),
    strip.text        = element_text(face = "plain", size = 8, vjust = 0)
  )


# Functions



GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

gettaxbarplot <- function(physeq, highertax, lowertax, group, 
                          highertaxthresh, lowertaxthresh){
  
  # physeq = readRDS(paste0("./data/rds/", input))
  # highertax = "Class"
  # lowertax = "Genus"
  # group = "DataSource"
  # highertaxthresh = 0.01
  # lowertaxthresh = 0.05
  # 
  physeqGROUP <- merge_samples(physeq, group)
  # otuGROUP <- data.frame(otu_table(physeqGROUP)) 
  # colnames(otuGROUP) <- gsub("^X", "", colnames(otuGROUP))
  
  otu <- data.frame(otu_table(physeq)) 
  colnames(otu) <- gsub("^X", "", colnames(otu))
  
  tax <- data.frame(tax_table(physeq)) %>%
    mutate_all(na_if,"") %>% 
    mutate(lowertax = .data[[lowertax]], 
           lowertax = if_else(is.na(lowertax), "Unknown", lowertax),
           lowertax = str_replace_all(lowertax, "unknown", "Unknown"), 
           highertax = .data[[highertax]])
  
  
  # Rename low abundance HIGHERtaxa per GROUP
  physeqHIGHER <- tax_glom(physeqGROUP, taxrank= highertax)
  taxHIGHER <- data.frame(tax_table(physeqHIGHER))
  otuHIGHER <- data.frame(otu_table(physeqHIGHER) %>% t()) %>%
    t() %>% 
    apply(1, function(x) x/sum(x)) %>% data.frame() %>% 
    rownames_to_column(var = "id") %>% 
    mutate(tax = taxHIGHER[id, highertax], 
           tax = if_else(is.na(tax), "Unkown", tax)) %>% 
    dplyr::select(-id) %>% 
    group_by(tax) %>% 
    summarise(across(everything(), sum)) %>% 
    column_to_rownames(var = "tax")
  
  avgAbundHIGH <- otuHIGHER %>% 
    mutate(maxAvgAbund =  apply(., 1, max )) %>%  # pick the max between all group
    dplyr::select(maxAvgAbund) %>% 
    rowMeans(na.rm = TRUE) %>% 
    data.frame(avgAbundHIGH = .) %>% 
    cbind(taxHIGH = row.names(.)) %>% 
    mutate(taxHIGH = if_else(avgAbundHIGH > highertaxthresh, 
                             taxHIGH, 
                             paste0("Low Abundance (<", highertaxthresh, ")")))
  
  # Make a mapping df to map higher tax to lower tax 
  colorkeygroup <- tax %>% dplyr::select(lowertax,highertax) %>% 
    distinct(lowertax, .keep_all = TRUE) %>% 
    rownames_to_column(var = "id") %>% 
    column_to_rownames(var = "lowertax") %>% 
    mutate(highertax = if_else(is.na(highertax), "Unknown", highertax), 
           highertaxNAME = avgAbundHIGH[highertax,"taxHIGH"],
           highertaxNAME = if_else(is.na(highertaxNAME), "Unknown", highertaxNAME))
  
  rownames(tax) <- str_replace_all(rownames(tax), "-", "\\.")
  
  metadata <- data.frame(sample_data(physeq)) %>% 
    mutate(Group = .data[[group]])
  sample_data(physeq) <- metadata
  # colorkeygroup["Azolla", 2]
  # avgAbundHIGH[colorkeygroup["Azolla", 2], 1]
  # 1.900114e-04/avgAbundHIGH[colorkeygroup["Azolla", 2], 1]
  
  # tax$higher %>% table()
  # tax$lowertax %>% table()
  otutaxglom <- otu %>% 
    apply(1, function(x) x/sum(x)) %>%
    merge(tax, by = "row.names", all.x = TRUE) %>%
    mutate_at(rownames(otu), ~replace(., is.nan(.), 0)) %>% 
    column_to_rownames(var = "Row.names") %>% 
    group_by(lowertax) %>%
    summarise(across(where(is.numeric), sum)) %>% 
    column_to_rownames(var = "lowertax") %>%
    replace(is.na(.), 0) %>% 
    mutate(avgAbund = rowMeans(.)) %>%
    rownames_to_column(var = "lowertax") %>% 
    mutate(lowertaxNAME = if_else((avgAbund/avgAbundHIGH[colorkeygroup[lowertax, 2], 1] > lowertaxthresh) | 
                                    colorkeygroup[lowertax, 2] == "Unknown", 
                                  lowertax, 
                                  paste0("Other ", colorkeygroup[lowertax, 2])),
           lowertaxNAME = if_else(str_detect(colorkeygroup[lowertax, 3], "Low Abund"), 
                                  colorkeygroup[lowertax, 3], 
                                  lowertaxNAME), 
           lowertaxNAME = if_else(str_detect(colorkeygroup[lowertax, 3], "Unknown"), 
                                  "Unknown", 
                                  lowertaxNAME)) %>% 
    #paste0("Low Abundance (<", lowertaxthresh, ")"))) %>%
    group_by(lowertaxNAME) %>%
    summarise(across(where(is.numeric), sum)) %>%
    column_to_rownames(var = "lowertaxNAME")
  
  # colSums(otutaxglom) # check if columns add to 1 
  
  
  plotdf <- otutaxglom %>%
    t() %>% as.data.frame() %>%
    merge(metadata, by = "row.names") %>%
    pivot_longer(rownames(otutaxglom), 
                 names_to = "lowertax", 
                 values_to = "relAbundance") %>% 
    mutate(highertax = colorkeygroup[lowertax, 3], 
           highertax = if_else(is.na(highertax) & str_detect(lowertax, "Other"), 
                               str_remove(lowertax, "Other\\s"),
                               highertax),
           highertax = if_else(str_detect(lowertax, "Low Abund"), lowertax, highertax),
           highertax = if_else(is.na(highertax), "Unknown", highertax)) %>% 
    mutate(#lowertax = str_to_title(lowertax), 
      lowertax = if_else(lowertax == "Unknown" & highertax!="Unknown" & !str_detect(lowertax, "Low Abund"), 
                         paste("Unknown", highertax), 
                         lowertax)) %>% 
    mutate(lowertax = str_replace_all(lowertax, "#N/A", "Unknown")) %>% 
    mutate(lowertax = factor(lowertax, 
                             levels = c(setdiff(lowertax, paste0("Low Abundance (<", lowertaxthresh, ")")) %>% 
                                          setdiff(., "Unknown"),
                                        paste0("Low Abundance (<", lowertaxthresh, ")"), 
                                        "Unknown")),
           highertax = factor(highertax, 
                              levels = c(setdiff(highertax, paste0("Low Abundance (<", highertaxthresh, ")")) %>% 
                                           setdiff(., "Unknown"),
                                         paste0("Low Abundance (<", highertaxthresh, ")"), 
                                         "Unknown")))
  
  return(plotdf)
}

colorRamp <- function(color, n, strength) {
  colorlight = lighten(color, amount = strength)
  colordark = darken(color, amount = strength)
  
  colors <- colorRampPalette(c(colorlight, colordark))(n)
  return(colors)
  # plot(rep(1, n),col = colors, pch = 19, cex = 3)
  
}



