#chapter 12
#3a
library(ISLR)
library(ISLR2)
library(glmnet)
library(keras)
library(cluster)

x1 = c(1, 1, 0, 5, 6, 4)
x2 = c(4, 3, 4, 1, 2, 0)
x = cbind(x1, x2)
plot(x[,1], x[,2],
     xlab = expression(X[1]),
     ylab = expression(X[2]),
     bty = "l",
     
     pch = 16)
text(x[,1]+0.15, x[,2], 1:6)

#b
set.seed(2100)
labels = sample(2, nrow(x), replace = T)
x = cbind(x, labels)
x

plot(x[,1], x[,2],
     xlab = expression(X[1]),
     ylab = expression(X[2]),
     bty = "l",
     col = labels+2,
     pch = 16)
text(x[,1]+0.15, x[,2], 1:6)
legend("topright", legend = c("Cluster 1", "Cluster 2"),
       col = c(1,2)+2, pch = 16, bty = "n")

#c
# Cluster one
centroid_one = colMeans(x[x[,3] == 1,, drop = FALSE])[1:2]
centroid_one
# Cluster two
centroid_two = colMeans(x[x[,3] == 2,, drop = FALSE])[1:2]
centroid_two
plot(x[,1], x[,2],
     xlab = expression(X[1]),
     ylab = expression(X[2]),
     col = labels+2,
     bty = "l",
     pch = 16)
text(x[,1]+0.15, x[,2], 1:6)
legend("topright", legend = c("Cluster 1", "Cluster 2"),
       col = c(1,2)+2, pch = 16, bty = "n")
points(centroid_one[1], centroid_one[2], pch = 3, col = 3, cex = 2)
points(centroid_two[1], centroid_two[2], pch = 3, col = 4, cex = 2)

#d
labels = c(2, 2, 2, 1, 1, 1)
x[,3] = labels
plot(x[, 1], x[, 2],
     xlab = expression(X[1]),
     ylab = expression(X[2]),
     bty = "l",
     col = labels + 2,
     pch = 16)
text(x[,1]+0.15, x[,2], 1:6)
legend("topright", legend = c("Cluster 1", "Cluster 2"),
       col = c(1,2)+2, pch = 16, bty = "n")
points(centroid_one[1], centroid_one[2], pch = 3, col = 3, cex = 2)
points(centroid_two[1], centroid_two[2], pch = 3, col = 4, cex = 2)

distance <- function (x, y){
  return(sqrt((x[1] - y[1])^2 + (x[2] - y[2])^2))
}
# For observation 1:
distance(obs[1,], centroid1)
distance(obs[1,], centroid2)
# This observation is closer to centroid 1.

clust.label[1] <- 1

# For observation 2:
distance(obs[2,], centroid1)
distance(obs[2,], centroid2)
# This observation is closer to centroid 1.

clust.label[2] <- 1

# For observation 3:
distance(obs[3,], centroid1)
distance(obs[3,], centroid2)
# This observation is closer to centroid 1.

clust.label[3] <- 1

# For observation 4:
distance(obs[4,], centroid1)
distance(obs[4,], centroid2)
# This observation is closer to centroid 2.

clust.label[4] <- 2

# For observation 5:

distance(obs[5,], centroid1)
distance(obs[5,], centroid2)
# This observation is closer to centroid 2.

clust.label[5] <- 2

# For observation 6:

distance(obs[6,], centroid1)
distance(obs[6,], centroid2)
#This observation is closer to centroid 1.

clust.label[6] <- 1






#e
plot(obs[clust.label == 1, 1], obs[clust.label == 1, 2], col = 4, pch = 20, cex = 3,
     xlim = c(0, 6))
points(obs[clust.label == 2, 1], obs[clust.label == 2, 2], 
       col = 3, pch = 20, cex = 3)

centroid1 <- c(mean(obs[clust.label == 1, 1]), mean(obs[clust.label == 1, 2]))
centroid2 <- c(mean(obs[clust.label == 2, 1]), mean(obs[clust.label == 2, 2]))

points(centroid1[1], centroid1[2], col = 4, pch = 4)
points(centroid2[1], centroid2[2], col = 3, pch = 4)

# For observation 1:
distance(obs[1,], centroid1)
distance(obs[1,], centroid2)
# This observation is closer to centroid 1.

clust.label[1] <- 1

# For observation 2:
distance(obs[2,], centroid1)
distance(obs[2,], centroid2)
# This observation is closer to centroid 1.

clust.label[2] <- 1

# For observation 3:
distance(obs[3,], centroid1)
distance(obs[3,], centroid2)
# This observation is closer to centroid 1.

clust.label[3] <- 1

# For observation 4:
distance(obs[4,], centroid1)
distance(obs[4,], centroid2)
# This observation is closer to centroid 2.

clust.label[4] <- 2

# For observation 5:

distance(obs[5,], centroid1)

distance(obs[5,], centroid2)
# This observation is closer to centroid 2.

clust.label[5] <- 2

# For observation 6:

distance(obs[6,], centroid1)

distance(obs[6,], centroid2)

#This observation is closer to centroid 2 - different from the first time!

clust.label[6] <- 2

# Plot new clusters and compute and plot new centroids:

plot(obs[clust.label == 1, 1], obs[clust.label == 1, 2], col = 4, pch = 20, cex = 3,
     xlim = c(0, 6), ylim = c(0, 4))
points(obs[clust.label == 2, 1], obs[clust.label == 2, 2], 
       col = 3, pch = 20, cex = 3)

centroid1 <- c(mean(obs[clust.label == 1, 1]), mean(obs[clust.label == 1, 2]))
centroid2 <- c(mean(obs[clust.label == 2, 1]), mean(obs[clust.label == 2, 2]))

points(centroid1[1], centroid1[2], col = 4, pch = 4)
points(centroid2[1], centroid2[2], col = 5, pch = 4)

# This looks more accurate than the first one. Repeat one more time to verify that
# this is correct and we're done!

# For observation 1:
distance(obs[1,], centroid1)
distance(obs[1,], centroid2)
# This observation is closer to centroid 1.

clust.label[1] <- 1

# For observation 2:
distance(obs[2,], centroid1)
distance(obs[2,], centroid2)
# This observation is closer to centroid 1.

clust.label[2] <- 1

# For observation 3:
distance(obs[3,], centroid1)
distance(obs[3,], centroid2)
# This observation is closer to centroid 1.

clust.label[3] <- 1

# For observation 4:
distance(obs[4,], centroid1)
distance(obs[4,], centroid2)
# This observation is closer to centroid 2.

clust.label[4] <- 2

# For observation 5:

distance(obs[5,], centroid1)
distance(obs[5,], centroid2)
# This observation is closer to centroid 2.

clust.label[5] <- 2

# For observation 6:

distance(obs[6,], centroid1)
distance(obs[6,], centroid2)
#This observation is closer to centroid 2 - different from the first time!

clust.label[6] <- 2

# Plot new clusters and compute and plot new centroids:

plot(obs[clust.label == 1, 1], obs[clust.label == 1, 2], col = 4, pch = 20, cex = 3,
     xlim = c(0, 6), ylim = c(0, 4))
points(obs[clust.label == 2, 1], obs[clust.label == 2, 2], 
       col = 3, pch = 20, cex = 3)

centroid1 <- c(mean(obs[clust.label == 1, 1]), mean(obs[clust.label == 1, 2]))
centroid2 <- c(mean(obs[clust.label == 2, 1]), mean(obs[clust.label == 2, 2]))

points(centroid1[1], centroid1[2], col = 4, pch = 4)
points(centroid2[1], centroid2[2], col = 5, pch = 4)

# Cluster one
centroid_one = colMeans(x[x[,3] == 1,, drop = FALSE])[1:2]
centroid_one
# Cluster two
centroid_two = colMeans(x[x[,3] == 2,, drop = FALSE])[1:2]
centroid_two
plot(x[,1], x[,2],
     xlab = expression(X[1]),
     ylab = expression(X[2]),
     col = labels+2,
     bty = "l",
     pch = 16)
text(x[,1]+0.15, x[,2], 1:6)
legend("topright", legend = c("Cluster 1", "Cluster 2"),
       col = c(1,2)+2, pch = 16, bty = "n")
points(centroid_one[1], centroid_one[2], pch = 3, col = 3, cex = 2)
points(centroid_two[1], centroid_two[2], pch = 3, col = 4, cex = 2)


#f
plot(x[, 1], x[, 2], col=(labels + 1), pch = 20, cex = 2)






###########################################################################
#9
data = USArrests
# Look a bit at the data
head(data)
summary(data)
# Set seed for reproducibility
set.seed(2100)
# Create the complete linkage cluster
# 'dist()' computes the Euclidean distance between the observations.
complete_cluster = hclust(dist(data), method="complete")
# Plot the complete linkage cluster
plot(complete_cluster)

#b
# Call the cutree function and specify the number of clusters.
clusters3 = cutree(complete_cluster, 3)
clusters3
table(clusters3)

#c
# Scale the data
data_scaled = scale(USArrests)
# Set seed for reproducibility
set.seed(2100)
# Create the complete linkage cluster based on scaled data.
# 'dist()' computes the Euclidean distance between the observations.
complete_cluster_scaled = hclust(dist(data_scaled), method="complete")
# Plot the complete linkage cluster with scaled data
plot(complete_cluster_scaled)

#d
# Call the cutree function and specify the number of clusters.
clusters3_scaled = cutree(complete_cluster_scaled, 3)
clusters3_scaled
plot(clusters3_scaled)
table(clusters3_scaled)
rbind(clusters3, clusters3_scaled)
# Before scaling
mean = apply(data, 2, mean)
sd = apply(data, 2, sd)
IQR = apply(data, 2, IQR)
show(round(rbind(mean, sd, IQR), 3))
# After scaling
mean = apply(data_scaled, 2, mean)
sd = apply(data_scaled, 2, sd)
IQR = apply(data_scaled, 2, IQR)
show(round(rbind(mean, sd, IQR), 3))


#e
# Cluster one
centroid_one = colMeans(x[x[,3] == 1,, drop = FALSE])[1:2]
centroid_one

# Cluster two
centroid_two = colMeans(x[x[,3] == 2,, drop = FALSE])[1:2]
centroid_two

plot(x[,1], x[,2],
     xlab = expression(X[1]),
     ylab = expression(X[2]),
     col = labels+2,
     bty = "l",
     pch = 16)
text(x[,1]+0.15, x[,2], 1:6)
legend("topright", legend = c("Cluster 1", "Cluster 2"),
       col = c(1,2)+2, pch = 16, bty = "n")
points(centroid_one[1], centroid_one[2], pch = 3, col = 3, cex = 2)
points(centroid_two[1], centroid_two[2], pch = 3, col = 4, cex = 2)




#9
#a
data = USArrests
# Look a bit at the data
head(data)
summary(data)
# Set seed for reproducibility
set.seed(2100)
# Create the complete linkage cluster
# 'dist()' computes the Euclidean distance between the observations.
complete_cluster = hclust(dist(data), method="complete")
# Plot the complete linkage cluster
plot(complete_cluster)


#b
# Call the cutree function and specify the number of clusters.
clusters3 = cutree(complete_cluster, 3)
clusters3
plot(clusters3)
table(clusters3)

#c
# Scale the data
data_scaled = scale(USArrests)
# Set seed for reproducibility
set.seed(2100)
# Create the complete linkage cluster based on scaled data.
# 'dist()' computes the Euclidean distance between the observations.
complete_cluster_scaled = hclust(dist(data_scaled), method="complete")
# Plot the complete linkage cluster with scaled data
plot(complete_cluster_scaled)


#d
# Call the cutree function and specify the number of clusters.
clusters3_scaled = cutree(complete_cluster_scaled, 3)
clusters3_scaled
plot(clusters3_scaled)
table(clusters3_scaled)
rbind(clusters3, clusters3_scaled)
# Before scaling
mean = apply(data, 2, mean)
sd = apply(data, 2, sd)
IQR = apply(data, 2, IQR)
show(round(rbind(mean, sd, IQR), 3))
# After scaling
mean = apply(data_scaled, 2, mean)
sd = apply(data_scaled, 2, sd)
IQR = apply(data_scaled, 2, IQR)
show(round(rbind(mean, sd, IQR), 3))

table(cutree(complete_cluster_scaled, k=3))
cutree(clusters3_scaled, k=3)














