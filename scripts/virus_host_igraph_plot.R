library(visNetwork)
library(igraph)
library(tnameyverse)
library(ggsci)
library(ggalluvial)
source("scripts/utility.R")

g <- readRDS("data/virus_host_igraph.rds")

# ============================================================
# SHARED PREP
# ============================================================
nodes_all <- read.csv("./data/virus_cherry_network_node_taxonomy.tsv") |>
  dplyr::rename(TYPE = TYPE.x)

edges_all <- igraph::as_data_frame(g, what = "edges") |>
  dplyr::rename(from = from, to = to)


# ============================================================
# 1. Keystone species analysis
# ============================================================
btw  <- igraph::betweenness(g, directed = FALSE, normalized = TRUE)
deg  <- igraph::degree(g, mode = "all")

node_stats <- tibble::tibble(
  name        = igraph::V(g)$name,
  degree      = deg,
  betweenness = btw
) |>
  dplyr::left_join(nodes_all, by = "name", suffix = c("", ".dup")) |>
  dplyr::select(-dplyr::ends_with(".dup"))

deg_cut <- 400
btw_cut <- 0.025

node_plot <- node_stats |>
  dplyr::mutate(
    quadrant = dplyr::case_when(
      degree >= deg_cut & betweenness >= btw_cut ~ "High connectivity, High betweenness",
      degree >= deg_cut & betweenness <  btw_cut ~ "High connectivity, Low betweenness",
      degree <  deg_cut & betweenness >= btw_cut ~ "Low connectivity, High betweenness",
      TRUE                                       ~ "Low connectivity, Low betweenness"
    ),
    quadrant = factor(quadrant, levels = c(
      "High connectivity, High betweenness",
      "High connectivity, Low betweenness",
      "Low connectivity, High betweenness",
      "Low connectivity, Low betweenness"
    )),
    prevalence = tidyr::replace_na(prevalence, 0),
    size_class = dplyr::case_when(
      prevalence >= stats::quantile(prevalence, 0.75, na.rm = TRUE) ~ "Q4",
      prevalence >= stats::quantile(prevalence, 0.50, na.rm = TRUE) ~ "Q3",
      prevalence >= stats::quantile(prevalence, 0.25, na.rm = TRUE) ~ "Q2",
      TRUE                                                          ~ "Q1"
    ),
    size = dplyr::case_when(
      quadrant == "Low connectivity, Low betweenness" ~
        scales::rescale(prevalence, to = c(0.5, 2.5)),
      TRUE ~
        scales::rescale(prevalence, to = c(1.5, 6))
    ),
    colour = dplyr::case_when(
      quadrant == "Low connectivity, Low betweenness"  ~ "grey70",
      quadrant == "High connectivity, High betweenness" ~ "#C0392B",
      quadrant == "High connectivity, Low betweenness"  ~ "#2C7FB8",
      quadrant == "Low connectivity, High betweenness"  ~ "#F39C12"
    ),
    shape = dplyr::if_else(TYPE == "Query", "circle", "triangle"),
    label_show = quadrant != "Low connectivity, Low betweenness" &
      (degree >= stats::quantile(degree, 0.90, na.rm = TRUE) |
         betweenness >= stats::quantile(betweenness, 0.90, na.rm = TRUE)), 
    tax_label = if_else(is.na(tax_label), str_remove(name, "^.+\\:"), tax_label)
  ) |> 
  filter(TYPE != "Ref")

ggplot(node_plot, aes(x = degree, y = betweenness)) +
  geom_vline(xintercept = deg_cut, linetype = "dashed", colour = "grey30", linewidth = 0.4) +
  geom_hline(yintercept = btw_cut, linetype = "dashed", colour = "grey30", linewidth = 0.4) +
  geom_point(aes(size = size, colour = quadrant, shape = shape),
             alpha = 0.85, stroke = 0.3) +
  ggrepel::geom_label_repel(
    data = dplyr::filter(node_plot, label_show),
    aes(label = stringr::str_remove(tax_label, "^.+\\:\\s"), 
        color = quadrant),
    size = 2, fontface = "italic", 
    box.padding = 0.4, max.overlaps = 30, segment.size = 0.3
  ) +
  scale_colour_uchicago() + 
  scale_shape_manual(
    values = c("circle" = 16, "triangle" = 17),
    breaks = c("circle", "triangle"),
    labels = c("Virus", "Host"),
    name   = "Node type"
  ) +
  scale_size_identity(guide = "none") +
  scale_x_log10(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)) + 
  labs(x = "Node degree", y = "Normalized Betweeness Centrality") +
  theme8 + 
  theme(
    axis.title       = element_text(face = "bold"),
    axis.text        = element_text(colour = "grey20"),
    legend.position  = "top",
    legend.key       = element_blank(),
    legend.text      = element_text(size = 9),
    panel.grid.major = element_line(colour = "grey92", linewidth = 0.2)
  ) + 
  guides(colour = guide_legend(ncol = 1, position = "top"), 
         shape = guide_legend(position = "right"), 
         size = guide_legend())

top_hits <- node_plot |> 
  filter(label_show != FALSE)

write.csv(top_hits, "./data/network_keystone_analysis.csv", row.names = FALSE)



# TEST SANKEY ####
host_names <- nodes_all$name[nodes_all$TYPE != "Query"]

core_membership <- nodes_all |>
  dplyr::filter(TYPE == "Query") |>
  dplyr::select(name, tax_label, core_Both, core_only_Rumen, core_only_Feces) |>
  tidyr::pivot_longer(
    cols      = c(core_Both, core_only_Rumen, core_only_Feces),
    names_to  = "core_group",
    values_to = "in_core"
  ) |>
  dplyr::filter(in_core) |>
  dplyr::select(name, tax_label, core_group) |>
  dplyr::mutate(
    core_group = factor(core_group,
                        levels = c("core_Both", "core_only_Rumen", "core_only_Feces"),
                        labels = c("Core (both)", "Core (rumen only)", "Core (feces only)")),
    virus_taxon = stringr::str_remove(tax_label, "^.+\\:\\s")
  )

links <- core_membership |>
  dplyr::mutate(host = purrr::map(name, ~ intersect(igraph::neighbors(g, .x)$name, host_names))) |>
  tidyr::unnest(host) |>
  dplyr::distinct(name, core_group, virus_taxon, host)

host_labels <- nodes_all |>
  dplyr::filter(name %in% links$host) |>
  dplyr::distinct(name, tax_label) |>
  dplyr::mutate(
    tax_label = if_else(is.na(tax_label), name, tax_label), 
    host_label = stringr::str_remove(tax_label, "^.+\\:"),
    host_label = stringr::str_remove(host_label, "^.+\\:\\[?"),
    host_label = stringr::str_remove(host_label, "\\]$")
  )

links_new <- links |>
  dplyr::left_join(host_labels, by = c("host" = "name"), suffix = c("", "dup")) |> 
  dplyr::select(-dplyr::ends_with(".dup"))

alluvial_df <- links_new |>
  dplyr::count(core_group, virus_taxon, host_label, name = "n")

ggplot(
  alluvial_df,
  aes(axis1 = core_group, axis2 = virus_taxon, axis3 = host_label, y = n)
) +
  geom_alluvium(aes(fill = core_group), width = 0.25, alpha = 0.7) +
  geom_stratum(width = 0.6, fill = "grey92", colour = "grey30") +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum), 
        colour = core_group),
    size = 2.8, 
    fontface = "bold.italic"
  ) +
  scale_x_discrete(
    limits = c("Core group", "Viral Taxonomy", "Host"),
    expand = c(0.05, 0.05)
  ) +
  scale_fill_manual(
    values = c("Core (both)"       = "#C0392B",
               "Core (rumen only)" = "#F39C12",
               "Core (feces only)" = "#2C7FB8"),
    name = NULL
  ) +
  scale_colour_manual(
    values = c("Core (both)"       = "#C0392B",
               "Core (rumen only)" = "#F39C12",
               "Core (feces only)" = "#2C7FB8"),
    name = NULL
  ) +
  theme_void(base_size = 12) +
  theme(
    axis.title       = element_blank(),
    axis.text.x      = element_text(size = 12, face = "bold", colour = "grey20"),
    legend.position  = "none",
    plot.margin      = margin(10, 10, 10, 10)
  )



# ============================================================
# Rumen vs. 
# ============================================================
host_names <- nodes_all$name[nodes_all$TYPE != "Query"]

core_membership <- nodes_all |>
  dplyr::filter(TYPE == "Query") |>
  dplyr::select(name, core_Both, core_only_Rumen, core_only_Feces) |>
  tidyr::pivot_longer(
    cols      = -name,
    names_to  = "core_group",
    values_to = "in_core"
  ) |>
  dplyr::filter(in_core) |>
  dplyr::select(name, core_group)

core_hosts <- core_membership |>
  dplyr::mutate(host = purrr::map(name, ~ intersect(igraph::neighbors(g, .x)$name, host_names))) |>
  tidyr::unnest(host) |>
  dplyr::distinct(core_group, host) |>
  dplyr::mutate(
    core_group = factor(core_group,
                        levels = c("core_Both", "core_only_Rumen", "core_only_Feces"),
                        labels = c("Core (both)", "Core (rumen only)", "Core (feces only)"))
  )

host_labels <- nodes_all |>
  dplyr::filter(name %in% core_hosts$host) |>
  dplyr::distinct(name, tax_label) |>
  dplyr::mutate(
    label = stringr::str_remove(tax_label, "^.+\\:\\s"),
    label = stringr::str_remove(label, "^species:\\[?"),
    label = stringr::str_remove(label, "\\]$")
  )

edge_df <- core_hosts |>
  dplyr::transmute(from = core_group, to = host)

nodes_df <- tibble::tibble(name = unique(c(edge_df$from, edge_df$to))) |>
  dplyr::mutate(
    type = dplyr::if_else(name %in% edge_df$from, "Core group", "Host")
  ) |>
  dplyr::left_join(host_labels, by = "name") |>
  dplyr::mutate(label = dplyr::coalesce(label, name))

bg <- tidygraph::tbl_graph(nodes = nodes_df, edges = edge_df, directed = FALSE)

bg_ig <- tidygraph::as.igraph(bg)
igraph::V(bg_ig)$type <- igraph::V(bg_ig)$type == "Host"
lay <- igraph::layout_as_bipartite(bg_ig)

ggraph(bg, layout = "manual", x = lay[, 1], y = lay[, 2]) +
  geom_edge_link(colour = "grey70", alpha = 0.4, linewidth = 0.4) +
  geom_node_point(aes(colour = type, shape = type, size = type)) +
  geom_node_text(
    aes(label = label, colour = type),
    repel = TRUE, size = 3, fontface = "italic",
    max.overlaps = 40, box.padding = 0.3,
    segment.size = 0.2, segment.colour = "grey60"
  ) +
  scale_colour_manual(values = c("Core group" = "#C0392B", "Host" = "#1B5E20")) +
  scale_shape_manual(values = c("Core group" = 17, "Host" = 16)) +
  scale_size_manual(values = c("Core group" = 5, "Host" = 2)) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title    = element_blank(),
    legend.text     = element_text(size = 11)
  )

# ============================================================
# Network
# ============================================================
vir_dig_filt <- 20
host_dig_filt <- 0

keep_virus <- nodes_all |> 
  dplyr::filter(degree >= vir_dig_filt & TYPE == "Query") |> 
  dplyr::pull(name)
# keep_host  <- nodese_all  |> 
#   dplyr::filter(degree >= host_dig_filt & TYPE %in% c("Host", "Ref")) |> 
#   dplyr::pull(name)
keep_host_edges  <- edges_all  |>
  dplyr::filter(to %in% keep_virus | from %in% keep_virus)
keep_host <- unique(c(keep_host_edges$to, keep_host_edges$from))

keep <- intersect(c(keep_virus, keep_host), nodes_all$name)
g1   <- igraph::delete_vertices(g, !(V(g)$name %in% keep)) 
g1 <- delete_vertices(g1, degree(g1)==0)

plot(g1,
     vertex.size  = 1,
     vertex.label = NA,
     vertex.color = "grey40",
     edge.color   = "grey80",
     edge.width   = 0.2,
     layout       = layout_with_fr(g1))


nodes1 <- nodes_all |>
  dplyr::filter(name %in% V(g1)$name) |>
  dplyr::mutate(
    id    = name,
    group = dplyr::if_else(TYPE != "Query", "Host", "Virus"),
    shape = dplyr::if_else(group == "Host", "triangle", "dot"),
    size  = dplyr::case_when(
      group == "Host" ~ 30,
      TRUE ~ scales::rescale(prevalence, to = c(6, 30))
    ),
    color = dplyr::case_when(
      group == "Host" ~ "#1B5E20",
      TRUE ~ {
        pal <- scales::hue_pal()(dplyr::n_distinct(tax_label[TYPE == "Query"]))
        pal[as.integer(factor(tax_label, levels = sort(unique(tax_label[TYPE == "Query"]))))]
      }
    ),
    label = dplyr::if_else(TYPE == "Query", tax_label, name),
    label = stringr::str_remove(label, "^.+\\:\\s"),
    label = dplyr::if_else(
      group == "Host",
      stringr::str_remove(stringr::str_remove(label, "^species:\\[?"), "\\]$"),
      label
    ),
    label = dplyr::if_else(
      group == "Virus" & nchar(label) > 12,
      paste0(substr(label, 1, 12), "..."),
      label
    ),
    selector = dplyr::case_when(
      is.na(TYPE)          ~ "Unknown",
      is.na(genomad_class) ~ TYPE,
      TRUE                 ~ paste0(TYPE, " — ", genomad_class)
    ),
    title = paste0(
      "<b>", label, "</b><br>",
      "Type: ", TYPE, "<br>",
      "geNomad class: ", ifelse(is.na(genomad_class), "NA", genomad_class), "<br>",
      "Degree: ", degree, "<br>",
      "Prevalence: ", round(prevalence, 1), "%<br>",
      "Mean abundance: ", signif(mean_abund, 3)
    )
  ) |>
  dplyr::select(id, label, group, shape, size, color, selector, title, dplyr::everything())

edges1 <- igraph::as_data_frame(g1, what = "edges") |>
  dplyr::mutate(
    value  = if ("Weight" %in% list.edge.attributes(g1)) Weight else 1,
    color  = "rgba(150, 150, 150, 0.4)",
    smooth = FALSE
  )

virus_taxa  <- sort(unique(nodes1$tax_label[nodes1$group == "Virus"]))
virus_cols  <- scales::hue_pal()(length(virus_taxa))

legend_nodes <- rbind(
  data.frame(label = "Host", color = "#1B5E20", shape = "triangle",
             size = 15, font.size = 13, stringsAsFactors = FALSE),
  data.frame(label = virus_taxa, color = virus_cols, shape = "dot",
             size = 8, font.size = 12, stringsAsFactors = FALSE)
)

net <- visNetwork(nodes1, edges1, width = "100%", height = "800px") |>
  visNodes(
    font = list(size = 12, face = "bold", color = "white",
                strokeWidth = 2, strokeColor = "#333")
  ) |>
  visEdges(
    color   = list(color = "rgba(150, 150, 150, 0.3)", highlight = "#C0392B"),
    smooth  = FALSE,
    scaling = list(min = 0.5, max = 3)
  ) |>
  visLegend(
    useGroups = FALSE,
    addNodes  = legend_nodes,
    ncol      = 1,
    position  = "right"
  ) |>
  visOptions(
    highlightNearest  = list(enabled = TRUE, degree = 1, hover = TRUE),
    nodesIdSelection  = list(enabled = TRUE, style = "width: 200px; height: 26px;"),
    selectedBy        = list(variable = "selector",
                             style = "width: 220px; height: 26px;")
  ) |>
  visPhysics(
    solver = "forceAtlas2Based",
    forceAtlas2Based = list(
      gravitationalConstant = -80,
      centralGravity        = 0.005,
      springLength          = 120,
      springConstant        = 0.06,
      damping               = 0.6
    ),
    stabilization = list(
      enabled        = TRUE,
      iterations     = 1500,
      fit            = TRUE,
      updateInterval = 50
    ),
    maxVelocity = 20,
    minVelocity = 0.75,
    timestep    = 0.35
  ) |>
  visEvents(
    stabilizationIterationsDone = "function() { this.setOptions({ physics: { enabled: false } }); }"
  ) |>
  visLayout(randomSeed = 1234) |>
  visInteraction(
    dragNodes         = TRUE,
    dragView          = TRUE,
    zoomView          = TRUE,
    navigationButtons = TRUE,
    tooltipDelay      = 100
  ) |>
  visExport(type = "png", name = "virus_host_network", float = "right")

net

visSave(net, "./data/net_1_bipartite.html",
        selfcontained = TRUE, background = "white")

igraph::write_graph(
  g1,
  file   = "./data/net_1_bipartite.graphml",
  format = "graphml"
)
# ============================================================
# 2. HOST-CENTRIC — hosts with many viral links
# ============================================================
keep_host2 <- host_deg |> dplyr::filter(deg >= 5) |> dplyr::pull(name)
keep2 <- intersect(c(keep_host2, edges_all$from[edges_all$to %in% keep_host2]), nodes_all$name)
g2 <- induced_subgraph(g, vnames = keep2)

nodes2 <- data.frame(
  name    = V(g2)$name,
  label = V(g2)$name,
  group = ifelse(V(g2)$name %in% keep_host2, "Hub host", "Virus"),
  shape = ifelse(V(g2)$name %in% keep_host2, "square", "dot"),
  size  = ifelse(V(g2)$name %in% keep_host2, 30, 6),
  color = ifelse(V(g2)$name %in% keep_host2, "#2C7FB8", "#C0392B"),
  stringsAsFactors = FALSE
)

edges2 <- as_data_frame(g2, what = "edges") |>
  dplyr::mutate(color = "grey70", wnameth = 0.4)

visNetwork(nodes2, edges2, wnameth = "100%") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visGroups(groupname = "Hub host", color = "#2C7FB8", shape = "square") %>%
  visGroups(groupname = "Virus",    color = "#C0392B", shape = "dot") %>%
  visLegend() %>%
  visLayout(randomSeed = 1234) %>%
  visPhysics(stabilization = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visSave("./data/net_2_host_centric.html", selfcontained = TRUE, background = "white")

# ============================================================
# 3. VIRUS-CENTRIC — viruses with broad host range
# ============================================================
virus_deg3 <- edges_all |> dplyr::count(from) |> dplyr::rename(name = from, deg = n)
keep_virus3 <- virus_deg3 |> dplyr::filter(deg >= 3) |> dplyr::pull(name)
keep3 <- intersect(c(keep_virus3, edges_all$to[edges_all$from %in% keep_virus3]), nodes_all$name)
g3 <- induced_subgraph(g, vnames = keep3)

nodes3 <- data.frame(
  name    = V(g3)$name,
  label = V(g3)$name,
  group = ifelse(V(g3)$name %in% keep_virus3, "Hub virus", "Host"),
  shape = ifelse(V(g3)$name %in% keep_virus3, "dot", "square"),
  size  = ifelse(V(g3)$name %in% keep_virus3, 25, 6),
  color = ifelse(V(g3)$name %in% keep_virus3, "#C0392B", "#2C7FB8"),
  stringsAsFactors = FALSE
)

edges3 <- as_data_frame(g3, what = "edges") |>
  dplyr::mutate(color = "grey70", wnameth = 0.4)

visNetwork(nodes3, edges3, wnameth = "100%") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visGroups(groupname = "Hub virus", color = "#C0392B", shape = "dot") %>%
  visGroups(groupname = "Host",      color = "#2C7FB8", shape = "square") %>%
  visLegend() %>%
  visLayout(randomSeed = 1234) %>%
  visPhysics(stabilization = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visSave("./data/net_3_virus_centric.html", selfcontained = TRUE, background = "white")

# ============================================================
# 4. VIRAL CLASS × HOST PHYLUM — taxonomic aggregation
# ============================================================
nodes4_all <- nodes_all |>
  dplyr::filter(is_host | !is.na(class), !is.na(host), host != "")

virus_class <- nodes4_all |>
  dplyr::filter(role == "Virus") |>
  dplyr::select(name, class) |>
  dplyr::distinct()

host_phylum <- data.frame(
  host = unique(nodes4_all$host[nodes4_all$is_host])
) |>
  dplyr::mutate(phylum = host)  # replace with real phylum mapping if available

edges4 <- edges_all |>
  dplyr::left_join(virus_class, by = c("from" = "name")) |>
  dplyr::left_join(host_phylum, by = c("to" = "host")) |>
  dplyr::filter(!is.na(class), !is.na(phylum)) |>
  dplyr::count(class, phylum, name = "weight") |>
  dplyr::filter(weight >= 2)  # drop rare combinations

nodes4 <- dplyr::bind_rows(
  data.frame(name = unique(edges4$class),  group = "Viral class",
             shape = "dot",    size = 20, color = "#C0392B"),
  data.frame(name = unique(edges4$phylum), group = "Host phylum",
             shape = "square", size = 20, color = "#2C7FB8")
) |>
  dplyr::distinct(name, .keep_all = TRUE) |>
  dplyr::mutate(label = name)

edges4_vis <- edges4 |>
  dplyr::transmute(from = class, to = phylum,
                   wnameth = scales::rescale(weight, to = c(1, 6)),
                   color = "grey70")

visNetwork(nodes4, edges4_vis, wnameth = "100%") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visGroups(groupname = "Viral class",  color = "#C0392B", shape = "dot") %>%
  visGroups(groupname = "Host phylum",  color = "#2C7FB8", shape = "square") %>%
  visLegend() %>%
  visLayout(randomSeed = 1234) %>%
  visPhysics(stabilization = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visSave("./data/net_4_class_phylum.html", selfcontained = TRUE, background = "white")

# ============================================================
# 5. GEOGRAPHIC / SOURCE — viruses and hosts coloured by source
# ============================================================
if (!is.null(nodes_all$source) && any(!is.na(nodes_all$source))) {
  
  edges5 <- edges_all |>
    dplyr::left_join(nodes_all |> dplyr::select(name, source), by = c("from" = "name")) |>
    dplyr::filter(!is.na(source))
  
  keep5 <- unique(c(edges5$from, edges5$to))
  g5 <- induced_subgraph(g, vnames = keep5)
  
  nodes5 <- data.frame(
    name    = V(g5)$name,
    label = V(g5)$name,
    group = V(g5)$source,
    shape = ifelse(!is.na(V(g5)$cherry_Host), "square", "dot"),
    size  = 8,
    stringsAsFactors = FALSE
  )
  
  edges5_vis <- as_data_frame(g5, what = "edges") |>
    dplyr::mutate(color = "grey70", wnameth = 0.5)
  
  source_colors <- pal_d3("category10")(length(unique(nodes5$group)))
  
  net5 <- visNetwork(nodes5, edges5_vis, wnameth = "100%") %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visLegend() %>%
    visLayout(randomSeed = 1234) %>%
    visPhysics(stabilization = TRUE) %>%
    visInteraction(navigationButtons = TRUE)
  
  for (i in seq_along(unique(nodes5$group))) {
    grp <- unique(nodes5$group)[i]
    net5 <- net5 %>%
      visGroups(groupname = grp, color = source_colors[i])
  }
  
  net5 %>% visSave("./data/net_5_source.html", selfcontained = TRUE, background = "white")
}