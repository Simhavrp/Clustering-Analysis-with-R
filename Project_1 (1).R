# Load libraries
library(dplyr)
library(tidyr)
library(factoextra)
library(readxl)

# Read the Excel file into a data frame
file_path <- "/Users/Simha/Downloads/Mine_Dataset (1).xls"
mine_data <- readxl::read_excel(file_path, sheet = 2)

# Encode the categorical variable 'S' as a factor
mine_data$S <- as.factor(mine_data$S)

# Check the structure of the data to confirm the encoding
str(mine_data)

# Select relevant columns for clustering
selected_columns <- mine_data %>%
  select(V, H, S)

# Convert non-numeric columns to numeric if needed
selected_columns <- as.data.frame(sapply(selected_columns, as.numeric))

# Perform k-means clustering
set.seed(123)
kmeans_result <- kmeans(selected_columns, centers = 5, nstart = 25)

# Principal Component Analysis (PCA)
pca_result <- prcomp(selected_columns, scale. = TRUE)

# Extract principal components
pc1 <- pca_result$x[, 1]
pc2 <- pca_result$x[, 2]

# Scatter plot with different colors for each cluster:
plot(pc1, pc2, col = kmeans_result$cluster, pch = 16, main = "2D Scatter Plot of K-Means Clustering Results",
     xlab = "Principal Component 1", ylab = "Principal Component 2")
legend("topright", legend = unique(kmeans_result$cluster), col = 1:5, pch = 16, title = "Cluster")

# Hierarchical Clustering:
hclust_result <- hclust(dist(selected_columns), method = "complete")
k_clusters <- cutree(hclust_result, k = 5)
plot(pc1, pc2, col = k_clusters, pch = 16, main = "2D Scatter Plot with Hierarchical Clustering",
     xlab = "Principal Component 1", ylab = "Principal Component 2")
legend("topright", legend = unique(k_clusters), col = 1:max(k_clusters), pch = 16, title = "Cluster")
plot(hclust_result, main = "Hierarchical Clustering Dendrogram", xlab = "", sub = "")

# Evaluation Metrics:
library(cluster)  

# K-means Clustering
set.seed(123)
kmeans_result <- kmeans(selected_columns, centers = 5, nstart = 25)

# Silhouette Score for K-means
silhouette_kmeans <- silhouette(kmeans_result$cluster, dist(selected_columns))

# Hierarchical Clustering
hclust_result <- hclust(dist(selected_columns), method = "complete")
k_clusters <- cutree(hclust_result, k = 5)

# Silhouette Score for Hierarchical Clustering
silhouette_hierarchical <- silhouette(k_clusters, dist(selected_columns))

# Print evaluation metrics
cat("K-means Clustering Evaluation:\n")
cat("Silhouette Score:", mean(silhouette_kmeans[, 3]), "\n")
cat("Hierarchical Clustering Evaluation:\n")
cat("Silhouette Score:", mean(silhouette_hierarchical[, 3]), "\n")

# Visualize Silhouette Plots
par(mfrow = c(1, 2))
plot(silhouette_kmeans, main = "Silhouette Plot for K-means", col = 1:5)
plot(silhouette_hierarchical, main = "Silhouette Plot for Hierarchical Clustering", col = 1:5)
