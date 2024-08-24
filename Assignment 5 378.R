getwd()
ppm.data <- read.table("HW5Data.txt")
ppm.data
summary(ppm.data)

data = c(107.2, 48.0, 75.8, 46.0, 131.6, 57.0, 25.0, 127.4, 133.1, 33.8, 58.2, 94.8, 36.2, 104.3, 85.9)
install.packages("cluster")
library(cluster)


diss_matrix <- dist(data)

#single linkage clustering #
agn.tox <- agnes(diss_matrix, diss = FALSE, stand = FALSE, method = "single")
dend.ang.tox <- as.dendrogram(agn.tox)
plot(dend.ang.tox, xlab = "Data Points", main = "Single-linkage clustering")


diss_matrix <- dist(data)

#complete linkage clustering
agn.completetox <- agnes(diss_matrix, diss = FALSE, stand = FALSE, method = "complete")
dend.ang.completetox <- as.dendrogram(agn.completetox)
plot(dend.ang.completetox, xlab = "Data Points", main = "Complete-linkage clustering")






# Q2 #
# adding 'kmeans' in the dataset
kmeans_result <- kmeans(ppm.data[, c("x", "y")], centers = 3)
kmeans_result

# Extract cluster centers
cluster_centers <- kmeans_result$centers
cluster_centers

# cluster assignments for 12
cluster_assignment <- kmeans_result$cluster[12]
cluster_assignment

# number of individuals in each cluster
table(kmeans_result$cluster)

# Plot of data points with cluster assignments
plot(ppm.data$x, ppm.data$y, col = kmeans_result$cluster, pch = 19)

# cluster centers to the plot
points(cluster_centers[, 1], cluster_centers[, 2], col = 1:3, pch = 3)


# sum of squares (tot.withinss) from kmeans result
betweenss <- kmeans_result$between
betweenss

#pseudo F stat

k.mean.data = kmeans(ppm.data[, c("x", "y")], centers = 3)
k.mean.data


k.mean.data$totss
k.mean.data$withinss
k.mean.data$tot.withinss
k.mean.data$between

MSB_k3 <- betweenss / (3 - 1)
MSE_k3 <- kmeans_result$tot.withinss / (20 - 3)
pseudo_F_k3 <- MSB_k3 / MSE_k3
print(pseudo_F_k3)


# Implement k-means clustering with k=4

kmeans_k4 <- kmeans(ppm.data[, c("x", "y")], centers = 4)
kmeans_k4

# Calculate pseudo-F statistic for k=4
MSB_k4 <- kmeans_result_k4$betweenss / (4 - 1)
MSE_k4 <- kmeans_result_k4$tot.withinss / (20 - 4)
pseudo_F_k4 <- MSB_k4 / MSE_k4
print(pseudo_F_k4)







