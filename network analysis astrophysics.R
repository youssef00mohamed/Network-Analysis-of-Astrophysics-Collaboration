# Install and load igraph package
library(igraph)

# The path to our text file
file_path <- "CA-AstroPh.txt"

# Read the data from the file
edge_data <- read.table(file_path, header = FALSE)

# Create a graph from the edge data
my_graph <- graph_from_data_frame(edge_data, directed = FALSE)

# Print basic information about the graph
print(summary(my_graph))

# Plot the graph
plot(my_graph, layout = layout_nicely(my_graph), vertex.label.dist = 1, vertex.label.cex = 1)

# Compute degree centrality
degree_centrality <- degree(my_graph)

# Identify the node with the highest degree (most important node)
most_important_node <- which.max(degree_centrality)
most_important_node_degree_centrality <- max(degree_centrality)

# Print the results
cat("Degree Centrality:", degree_centrality, "\n")
cat("Node with the Highest Degree (Most Important):", most_important_node, "\n")
cat("Degree Centrality of the Node with the Highest Degree (Most Important):", most_important_node_degree_centrality, "\n")

# Find connected components
components <- clusters(my_graph)
components

# Identify the largest connected component
largest_component_index <- which.max(components$csize)
largest_component <- components$membership == largest_component_index

# Count the number of nodes in the largest connected component
num_nodes_in_largest <- components$csize[largest_component_index]
# Print the results
cat("Number of nodes in the largest connected component:", num_nodes_in_largest, "\n")
cat("Largest Component Index:", largest_component_index, "\n")

# Calculate the average shortest path
avg_shortest_path <- average.path.length(my_graph, directed = FALSE)

# Print the result
cat("Average Shortest Path Length:", avg_shortest_path, "\n")

# Calculate the average clustering coefficient (Global)
avg_clustering_coefficient <- transitivity(my_graph)

# Print the result
cat("Average Clustering Coefficient:", avg_clustering_coefficient, "\n")

# Calculate the local clustering coefficient (Local)
local_clustering <- transitivity(my_graph, type = "local")

# Calculate the average local clustering coefficient
average_local_clustering <- mean(local_clustering, na.rm = TRUE)

# Print the results
cat("Average Local Clustering Coefficient:", average_local_clustering, "\n")

# Calculate the betweenness centrality
betweenness_centrality <- betweenness(my_graph)

# Plot a histogram of betweenness centrality
hist(betweenness_centrality, main = "Betweenness Centrality Histogram", xlab = "Betweenness Centrality", col = "lightblue", border = "black")

# Find the node with the highest betweenness centrality
highest_betweenness_centrality <- which.max(betweenness_centrality)
highest_betweenness_centrality_value <- max(betweenness_centrality)

# Print the results
cat("Node with the Highest Betweenness Centrality:", highest_betweenness_centrality, "\n")
cat("Node with the Highest Betweenness Centrality value:", highest_betweenness_centrality_value, "\n")

# Calculate the closeness centrality
closeness_centrality <- closeness(my_graph, mode = "all") # or "in" for in-closeness or "out" for out-closeness 
# We will go with "all" cuz the data is undirected

# Plot a histogram of closeness centrality
hist(closeness_centrality, main = "Closeness Centrality Histogram", xlab = "Closeness Centrality", col = "blue", border = "black")

# Find the node with the highest closeness centrality
most_closeness_node <- which.max(closeness_centrality)
most_closeness_value <- closeness_centrality[most_closeness_node]

# Print the results
cat("Node with the Highest Out-Closeness Centrality:", most_closeness_node, "\n")
cat("Closeness Centrality Value:", most_closeness_value, "\n")

# Replace 'source_node' and 'target_node' with the nodes you are interested in
source_node <- 1
target_node <- 3

# Find the shortest path
shortest_path <- shortest_paths(my_graph, from = source_node, to = target_node, mode = "all")

# Extract the path
path <- as.vector(shortest_path$vpath[[1]])

# Print the result
cat("Shortest Path from Node", source_node, "to Node", target_node, ":", path, "\n")

# Calculate eigenvector centrality
eigenvector_centrality <- eigen_centrality(my_graph)$vector

# Find the node with the highest eigenvector centrality
most_centrality_node <- which.max(eigenvector_centrality)
most_centrality_value <- max(eigenvector_centrality)

# Print the results
cat("Node with the Highest Eigenvector Centrality:", most_centrality_node, "\n")
cat("Eigenvector Centrality Value:", most_centrality_value, "\n")

# Get the adjacency matrix
adjacency_matrix <- get.adjacency(my_graph)

# Print the adjacency matrix
cat("Adjacency Matrix:\n")
print(adjacency_matrix)