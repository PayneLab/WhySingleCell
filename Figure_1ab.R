library(tidyverse)
library(viridis)
library(ragg)


## Set seed for replication
set.seed(77)

#### PART A ####
## Generate random clusters in a bifurcation structure
points_per_cluster <- 50

### Step 1: generate cluster 1 (it will be the left-most cluster, centered on the y-axis)
x_values <- rnorm(points_per_cluster, mean = 10, sd = 2)
y_values <- rnorm(points_per_cluster, mean = 15, sd = 2)

cluster_1 <- tibble(x_values, y_values) %>%
  mutate(cluster = "Cluster 1")

### Step 2: generate cluster 2 (it will be to the right of cluster 1, centered on the y-axis)
x_values <- rnorm(points_per_cluster, mean = 16, sd = 2)
y_values <- rnorm(points_per_cluster, mean = 15, sd = 2)

cluster_2 <- tibble(x_values, y_values) %>%
  mutate(cluster = "Cluster 2")

### Step 3: generate cluster 3 (it will be to the right of cluster 2, on the top end of the y-axis)
x_values <- rnorm(points_per_cluster, mean = 22, sd = 2)
y_values <- rnorm(points_per_cluster, mean = 20, sd = 2)

cluster_3 <- tibble(x_values, y_values) %>%
  mutate(cluster = "Cluster 3")

### Step 4: generate cluster 4 (it will be to the right of cluster 2, on the bottom end of the y-axis)
x_values <- rnorm(points_per_cluster, mean = 22, sd = 2)
y_values <- rnorm(points_per_cluster, mean = 10, sd = 2)

cluster_4 <- tibble(x_values, y_values) %>%
  mutate(cluster = "Cluster 4")

### Step 5: combine the data points into one table
clusters <- bind_rows(cluster_1, cluster_2, cluster_3, cluster_4)

### Step 6: create data points for a line plot to represent a trajectory 
###   overlayed on top of the scatterplot
trajectory_points <- clusters %>%
  group_by(cluster) %>%
  summarize(x_values = mean(x_values), y_values = mean(y_values))

trajectory_line1 <- trajectory_points %>%
  filter(cluster != "Cluster 4")

trajectory_line2 <- trajectory_points %>%
  filter(cluster == "Cluster 2" | cluster == "Cluster 4")

### Step 7: plot the clusters
trajectory_plot <- ggplot() + 
  geom_point(clusters, mapping = aes(x = x_values, y = y_values, color = cluster)) +
  scale_color_viridis(discrete = TRUE) +
  # geom_point(clusters, mapping = aes(x = x_values, y = y_values, color = "red")) + 
  geom_point(trajectory_points, mapping = aes(x_values, y_values), color = "black", size = 2.5) + 
  geom_line(trajectory_line1, mapping = aes(x_values, y_values), color = "black") + 
  geom_line(trajectory_line2, mapping = aes(x_values, y_values), color = "black") +
  theme_bw() + 
  labs(x = "X", y = "Y", title = "a)") + 
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  # theme(legend.position = "none", 
  #       axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
  #       axis.text.y = element_blank(), axis.ticks.y = element_blank())



#### PART B ####
## Split full dataset above into replicates
### Step 1: Randomly assigne a replicate number to each row in the clusters dataframe
replicates <- sample(rep(1:4, each = points_per_cluster))

clusters <- clusters %>%
  mutate(replicate = replicates)

### Step 2: create data points for a line plot to represent a trajectory 
###   overlayed on top of the scatterplot
trajectory_points <- clusters %>%
  group_by(replicate, cluster) %>%
  summarize(x_values = mean(x_values), y_values = mean(y_values))

trajectory_line1 <- trajectory_points %>%
  filter(ifelse(replicate != 2, 
                cluster != "Cluster 4", 
                cluster != "Cluster 3"))

trajectory_line2 <- trajectory_points %>%
  filter(ifelse(replicate != 2, 
                cluster == "Cluster 2" | cluster == "Cluster 4", 
                cluster == "Cluster 3" | cluster == "Cluster 4"))

### Spet 3: Plot the replicate trajectories
replicate_plot <- ggplot() + 
  geom_point(clusters, mapping = aes(x = x_values, y = y_values, color = cluster)) + 
  scale_color_viridis(discrete = TRUE) +
  geom_point(trajectory_points, mapping = aes(x_values, y_values), color = "black", size = 2.5) +
  geom_line(trajectory_line1, mapping = aes(x_values, y_values), color = "black") +
  geom_line(trajectory_line2, mapping = aes(x_values, y_values), color = "black") +
  facet_wrap(~replicate) + 
  theme_bw() + 
  labs(x = "X", y = "Y", title = "b)") + 
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  # theme(legend.position = "none", 
  #       axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
  #       axis.text.y = element_blank(), axis.ticks.y = element_blank())




## Save the plots
agg_png("Figure_1a.png", width = 6, height = 6, units = "in", res = 600, scaling = 1.5)
trajectory_plot
dev.off()

agg_png("Figure_1b.png", width = 6, height = 6, units = "in", res = 600, scaling = 1.5)
replicate_plot
dev.off()


