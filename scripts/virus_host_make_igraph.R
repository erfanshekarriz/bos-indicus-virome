library(visNetwork)
library(igraph)
library(tidyverse)
library(readxl)

nodes <- read_xlsx("data/cherry_network_nodes.xlsx") |>
  dplyr::rename(name = Accession)
edges <- read_xlsx("data/cherry_network_edges.xlsx") |>
  dplyr::rename(from = Source, to = Target)

tax <- read.csv("data/prevalence_abundance.csv")

nodetax <- nodes |>
  merge(tax, by.x = "name", by.y = "vOTU_id", all.x = TRUE) |>
  dplyr::filter(!(name == "filtered" | Host == "filtered" | Host == "-")) |>
  select(name, everything())

nodes_unique <- unique(nodetax$name)

edgefilt <- edges |>
  dplyr::filter(from %in% nodes_unique, 
                to %in% nodes_unique, 
                from != to)

g <- graph_from_data_frame(edgefilt, directed = FALSE, vertices = nodetax)
g_small <- induced_subgraph(g, sample(V(g)$name, 5000))
plot(g_small,
     vertex.size  = 1,
     vertex.label = NA,
     vertex.color = "grey40",
     edge.color   = "grey80",
     edge.width   = 0.2,
     layout       = layout_with_fr(g_small))


# get node attributes
attributes <- data.frame(degree=degree(g, normalized = F))
#betweenness = betweenness(g, directed = F, normalized = T), 
#eigen_centrality = eigen_centrality(g, directed = F), 
# authority_score = authority_score(g, weights = NULL),
# page_rank = page_rank(g, directed = F, weights = NULL),
#transitivity = transitivity(g, type = "undirected"), 
#coreness = coreness(g, mode = "all"))

nodesdf <- nodetax %>%
  merge(attributes, by.x = "name", by.y = "row.names", all.x = TRUE)

g_attr <- graph_from_data_frame(edgefilt, directed=FALSE, vertices=nodesdf)



saveRDS(g_attr, "./data/virus_host_igraph.rds")
write.csv(nodesdf, "./data/virus_cherry_network_node_taxonomy.tsv", row.names = FALSE)

rm(list=ls())

