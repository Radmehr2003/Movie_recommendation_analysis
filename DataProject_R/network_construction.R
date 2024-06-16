# Load required libraries
library(igraph)
library(ggplot2)
library(RColorBrewer)
library(data.table)

ratings <- fread("ml-latest-small/ratings.csv")

# Create a bipartite graph
B <- graph_from_data_frame(d = ratings, vertices = NULL, directed = FALSE)

# Add node attributes for bipartite graph
V(B)$type <- bipartite_mapping(B)$type

cat("Number of Nodes:", vcount(B), "\n")

degree_sequence <- degree(B)
degree_counts <- table(degree_sequence)

ggplot(data.frame(degree = as.numeric(names(degree_counts)), count = as.numeric(degree_counts)),
       aes(x = degree, y = count)) +
  geom_point(color = "black", size = 2) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Degree Distribution of the Bipartite Network",
       x = "Degree (Number of Connections)",
       y = "Frequency (Number of Users/Movies)") +
  theme_minimal(base_size = 16)

# Calculate positions for the nodes in the network
pos <- layout_with_fr(B)


clustering_coeff <- transitivity(B, type = "average")
cat("Clustering Coefficient:", clustering_coeff, "\n")

# Community detection using Louvain method
communities <- cluster_louvain(B)
cat("Detected Communities:", membership(communities), "\n")

# Color map for communities
colors <- brewer.pal(max(membership(communities)), "Set3")

# Enhanced plot of the communities
plot(communities, B, layout = pos, vertex.size = 5, vertex.label = NA,
     vertex.color = colors[membership(communities)], edge.color = "grey",
     main = "Community Structure in the Network", sub = "Detected using Louvain method")

# Custom legend
legend("topright", legend = paste("Community", 0:(length(colors) - 1)), col = colors, pch = 19, pt.cex = 2)
