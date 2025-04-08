#3. K-Means - How do health indicators and demographic factors differ across the identified clusters, and what insights can be drawn about high-risk or unique subgroups?

library(tidyverse)

names(clean_dataset)

# Select only numeric columns
numeric_dataset <- clean_dataset[, sapply(clean_dataset, is.numeric)]
names(numeric_dataset) <- make.unique(names(numeric_dataset))
head(numeric_dataset)
ggplot(data=numeric_dataset,aes(x=bmi,y=age))+geom_point()

k3 <- kmeans(numeric_dataset[, c(1:11)], centers=3)
k3
k3$centers
ggplot(data=numeric_dataset,aes(x=bmi,y=age)) +
  geom_point(aes(col=k3$cluster)) +
  geom_point(data=data.frame(k3$centers),col="red")

k7 <- kmeans(numeric_dataset[, c(1:11)], centers=7)
k7
k7$centers
ggplot(data=numeric_dataset,aes(x=bmi,y=age)) +
  geom_point(aes(col=k7$cluster)) +
  geom_point(data=data.frame(k7$centers),col="red")

k10 <- kmeans(numeric_dataset[, c(1:11)], centers=10)
k10
k10$centers
ggplot(data=numeric_dataset,aes(x=bmi,y=age)) +
  geom_point(aes(col=k10$cluster)) +
  geom_point(data=data.frame(k10$centers),col="red")

k13 <- kmeans(numeric_dataset[, c(1:11)], centers=13)
k13
k13$centers
ggplot(data=numeric_dataset,aes(x=bmi,y=age)) +
  geom_point(aes(col=k13$cluster)) +
  geom_point(data=data.frame(k13$centers),col="red")


# The elbow graph - use this to determine which k is best,then use that k to answer all the questions
Y=0;for(k in 1:25){Z=kmeans(numeric_dataset[, c(1:11)],centers=k);Y[k]=Z$betweenss/Z$totss}
ggplot(data=data.frame(x=1:25,y=Y),aes(x,y)) +
  geom_line()+
  geom_point()

# Initialize a vector to store WCSS for each k
WCSS <- numeric(25)

# Loop through k = 1 to 25
for(k in 1:25) {
  Z <- kmeans(numeric_dataset[, c(1:11)], centers = k)
  WCSS[k] <- Z$totss - Z$betweenss # Calculate WCSS as TSS - BSS
}

# Create a data frame for plotting
plot_data <- data.frame(x = 1:25, y = WCSS)

# Plot WCSS using ggplot
ggplot(data = plot_data, aes(x = x, y = y)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Within-Cluster Sum of Squares (WCSS) vs. Number of Clusters",
    x = "Number of Clusters (k)",
    y = "Within-Cluster Sum of Squares (WCSS)"
  )



#The best number of clusters is 4
k4 <- kmeans(numeric_dataset[, c(1:11)], centers=4)
k4
k4$centers
ggplot(data=numeric_dataset,aes(x=bmi,y=age)) +
  geom_point(aes(col=k4$cluster)) +
  geom_point(data=data.frame(k4$centers),col="red")

#############################################################################################
# Load necessary library
library(ggplot2)

# Create a data frame for the clusters
clusters <- data.frame(
  Cluster = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),
  BMI = c(25.04064, 30.13569, 27.98360, 29.64341)
)

# Plot the bar graph
ggplot(clusters, aes(x = Cluster, y = BMI, fill = Cluster)) +
  geom_bar(stat = "identity") +
  labs(
    title = "BMI Across Clusters",
    x = "Cluster",
    y = "BMI"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") # Optional: Adds a color palette for better visuals

#############################################################################################

