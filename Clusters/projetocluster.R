# Clustering, also known as cluster analysis, is an unsupervised learning technique used to identify patterns and structures in datasets. The main goal of clustering is to group similar objects into clusters and different objects into distinct clusters based on some measure of similarity or dissimilarity between them. Clustering has various applications such as customer segmentation, anomaly detection, exploratory data analysis, among others. Consequently, cluster analysis has applications in different domains.

# The dataset used in this project refers to customers of a Portuguese wholesale distributor. It includes annual spending in monetary units (u.m) on various product categories.

# Observations refer to customers, and variables are divided as follows:
# FRESH: annual spending (u.m.) on fresh products (Continuous);
# MILK: annual spending (u.m.) on dairy products (Continuous);
# GROCERY: annual spending (u.m.) on grocery products (Continuous);
# FROZEN: annual spending (u.m.) on frozen products (Continuous);
# DETERGENTS_PAPER: annual spending (u.m.) on detergents and paper products (Continuous);
# DELICATESSEN: annual spending (u.m.) on deli products (Continuous);
# CHANNEL: customer channel - Horeca (Hotel/Restaurant/Cafe) or Retail channel (Nominal);
# REGION: customer region - Lisbon, Porto, or Other (Nominal).

# With "CHANNEL" and "REGION" as categorical variables and the rest being quantitative variables.

# In this project, we will perform a hierarchical clustering process and a "k-means" clustering process. In summary, we will conduct a cluster analysis where the number of clusters will be defined during the process (hierarchical method), and an analysis where the number of clusters will be defined a priori. This way, we can use a common practice among data scientists, which involves using the "output" of the hierarchical method as the "input" for the "k-means" method.

# Installation and loading of used packages

packages <- c("plotly", "fastDummies", # graphical platform
             "tidyverse", # load other R packages
             "ggrepel", # text and label geoms for 'ggplot2' to prevent text overlap
             "knitr", "kableExtra", # table formatting
             "reshape2", # 'melt' function
             "misc3d", # 3D plots
             "plot3D", # 3D plots
             "cluster", # 'agnes' function for hierarchical clustering
             "factoextra", # 'fviz_dend' function for dendrogram construction
             "ade4") # 'ade4' function for distance matrix in binary variables

if (sum(as.numeric(!packages %in% installed.packages())) != 0) {
  installer <- packages[!packages %in% installed.packages()]
  for (i in 1:length(installer)) {
    install.packages(installer, dependencies = TRUE)
    break()
  }
  sapply(packages, require, character = TRUE)
} else {
  sapply(packages, require, character = TRUE)
}

# Importing the dataset
customer_data <- read.csv("Wholesale customers data.csv")
save(customer_data, file = "customer_data.RData")

# Visualizing the dataset
View(customer_data)
## Display only the first 10 rows in markdown

# Counting categories per variable

map(customer_data[, c("Channel", "Region")], ~ summary(as.factor(.)))
# Where: Channel(1) = Hotel/Restaurant/Cafe; Channel(2) = Retail.
# Region(1) = Lisbon; Region(2) = Porto; Region(3) = Other Region.

# Checking the data types of our database:
glimpse(customer_data)

# Since categorical variables are encoded as numeric, let's change them to factors:
customer_data2 <- customer_data
customer_data2$Channel <- as.factor(customer_data$Channel)
customer_data2$Region <- as.factor(customer_data$Region)

# As we have both categorical and numerical variables in the database, let's separate the variables into two databases so that we can create two distance matrices. This procedure is necessary because we will use different distance calculation methods for numerical and categorical variables. After that, we will combine the matrices and perform clustering on the combined matrix.

# Separating variables into numerical and categorical:
numeric_data <- customer_data2[, c("Fresh", "Milk", "Grocery", "Frozen", "Detergents_Paper", "Delicassen")]
categorical_data <- customer_data2[, c("Channel", "Region")]

# Standardizing numerical variables
standardized_data <- as.data.frame(scale(numeric_data))
# Now, all numerical variables have a mean of 0 and a standard deviation of 1.

# Creating dummy variables for categorical variables
dummy_data <- dummy_columns(.data = categorical_data,
                            select_columns = "Channel",
                            remove_selected_columns = TRUE,
                            remove_most_frequent_dummy = TRUE)

dummy_data <- dummy_columns(.data = dummy_data,
                            select_columns = "Region",
                            remove_selected_columns = TRUE,
                            remove_most_frequent_dummy = TRUE)

# Creating our matrices
numeric_D_matrix <- standardized_data %>% dist(method = "euclidean")
categorical_D_matrix <- dummy_data %>% dist(method = "binary")

# Combining the matrices
total_distance_matrix <- numeric_D_matrix + categorical_D_matrix

# Visualizing the dissimilarity matrix
data.matrix(total_distance_matrix) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)
## Display only the first 10 rows in markdown
# Since our distances are relatively small, we will use the complete linkage method during hierarchical clustering.

# Hierarchical clustering
hierarchical_cluster <- agnes(x = total_distance_matrix, method = "complete")

# Constructing the dendrogram
dev.off()
dendrogram1 <- fviz_dend(x = hierarchical_cluster, show_labels = FALSE)
dendrogram1
## We can see the presence of some well-defined clusters and others not so much... Probably,
## there are some outliers in the database.

# After analyzing the dendrogram in a hierarchical clustering process, we can choose the number of clusters by observing the dendrogram structure and identifying the cuts that seem more significant or relevant for our objective.

# Dendrogram with cluster visualization
# Setting the height to 7 to define the dendrogram clusters.
dendrogram_clusters <- fviz_dend(x = hierarchical_cluster,
          h = 7,
          color_labels_by_k = FALSE,
          rect = TRUE,
          rect_fill = TRUE,
          lwd = 1,
          ggtheme = theme_bw(),
          show_labels = FALSE)

dendrogram_clusters
## We can see approximately 12 clusters

# Creating a database with all the data used in creating the matrices:
complete_data <- cbind(standardized_data, Channel_2=dummy_data$Channel_2, Region_1=dummy_data$Region_1, Region_2=dummy_data$Region_2)

# Creating a categorical variable to indicate the cluster in the database
## The 'k' argument indicates the number of clusters.

complete_data$cluster_hierarchical <- factor(cutree(tree = hierarchical_cluster, k = 12))
## _ps: 12 is the number of clusters created by cutting at height 7

# Next, let's check if all variables help in forming the groups

summary(anova_channel2 <- aov(formula = Channel_2 ~ cluster_hierarchical,
                                data = complete_data))

summary(anova_region1 <- aov(formula = Region_1 ~ cluster_hierarchical,
                              data = complete_data))

summary(anova_region2 <- aov(formula = Region_2 ~ cluster_hierarchical,
                             data = complete_data))

summary(anova_fresh <- aov(formula = Fresh ~ cluster_hierarchical,
                             data = complete_data))

summary(anova_milk <- aov(formula = Milk ~ cluster_hierarchical,
                           data = complete_data))

summary(anova_grocery <- aov(formula = Grocery ~ cluster_hierarchical,
                           data = complete_data))

summary(anova_frozen <- aov(formula = Frozen ~ cluster_hierarchical,
                             data = complete_data))

summary(anova_detergents <- aov(formula = Detergents_Paper ~ cluster_hierarchical,
                            data = complete_data))

summary(anova_delicassen <- aov(formula = Delicassen ~ cluster_hierarchical,
                            data = complete_data))

# For a 95% confidence level, only the variable "Region_1" cannot be considered significant
# for the formation of at least one cluster.

# Descriptive statistics of clusters by variable

# Descriptive statistics of the 'Fresh' variable
group_by(complete_data, cluster_hierarchical) %>%
  summarise(
    mean = mean(Fresh),
    sd = sd(Fresh),
    min = min(Fresh),
    max = max(Fresh))

# Descriptive statistics of the 'Milk' variable
group_by(complete_data, cluster_hierarchical) %>%
  summarise(
    mean = mean(Milk),
    sd = sd(Milk),
    min = min(Milk),
    max = max(Milk))

# Descriptive statistics of the 'Grocery' variable
group_by(complete_data, cluster_hierarchical) %>%
  summarise(
    mean = mean(Grocery),
    sd = sd(Grocery),
    min = min(Grocery),
    max = max(Grocery))

# Descriptive statistics of the 'Frozen' variable
group_by(complete_data, cluster_hierarchical) %>%
  summarise(
    mean = mean(Frozen),
    sd = sd(Frozen),
    min = min(Frozen),
    max = max(Frozen))

# Descriptive statistics of the 'Detergents_Paper' variable
group_by(complete_data, cluster_hierarchical) %>%
  summarise(
    mean = mean(Detergents_Paper),
    sd = sd(Detergents_Paper),
    min = min(Detergents_Paper),
    max = max(Detergents_Paper))

# Descriptive statistics of the 'Delicassen' variable
group_by(complete_data, cluster_hierarchical) %>%
  summarise(
    mean = mean(Delicassen),
    sd = sd(Delicassen),
    min = min(Delicassen),
    max = max(Delicassen))

# Descriptive statistics of the 'Channel_2' variable
group_by(complete_data, cluster_hierarchical) %>%
  summarise(
    mean = mean(Channel_2),
    sd = sd(Channel_2),
    min = min(Channel_2),
    max = max(Channel_2))

# Descriptive statistics of the 'Region_1' variable
group_by(complete_data, cluster_hierarchical) %>%
  summarise(
    mean = mean(Region_1),
    sd = sd(Region_1),
    min = min(Region_1),
    max = max(Region_1))

# Descriptive statistics of the 'Region_2' variable
group_by(complete_data, cluster_hierarchical) %>%
  summarise(
    mean = mean(Region_2),
    sd = sd(Region_2),
    min = min(Region_2),
    max = max(Region_2))

## Through the statistics, we can understand the characteristics of each cluster, and consequently,
## the retail network would know how to better allocate its resources to meet the demand of its customers.
## In this example, we can also notice the presence of outliers because clusters 7, 8, 9, 10, 11, and 12
## are formed by a single observation. We could remove the outlier observations and rerun the hierarchical
## clustering algorithm. However, since the outliers represent a very small fraction of the data (6/440)
## and are isolated in their clusters, we can consider them irrelevant for the formation of the other clusters.
## Therefore, we will proceed with the initial objective and now execute a "k-means" clustering with all
## observations in the database (including outlier observations) and 12 clusters determined a priori. In the end,
## we can compare the results of both procedures.

# Elbow Method to identify the optimal number of clusters
fviz_nbclust(complete_data[,1:9], kmeans, method = "wss", k.max = 30)
## The elbow method seems to indicate that the optimal number of clusters is around 11 and 12, as we analyzed from the dendrogram in the hierarchical procedure.

# Non-hierarchical k-means clustering
kmeans_cluster <- kmeans(select(complete_data, -cluster_hierarchical),
                         centers = 12)

# Creating a categorical variable to indicate the cluster in the database
complete_data2 <- complete_data
complete_data2$cluster_Kmeans <- factor(kmeans_cluster$cluster)

summary(anova_channel2 <- aov(formula = Channel_2 ~ cluster_Kmeans,
                              data = complete_data2))

summary(anova_region1 <- aov(formula = Region_1 ~ cluster_Kmeans,
                             data = complete_data2))

summary(anova_region2 <- aov(formula = Region_2 ~ cluster_Kmeans,
                             data = complete_data2))

summary(anova_fresh <- aov(formula = Fresh ~ cluster_Kmeans,
                             data = complete_data2))

summary(anova_milk <- aov(formula = Milk ~ cluster_Kmeans,
                           data = complete_data2))

summary(anova_grocery <- aov(formula = Grocery ~ cluster_Kmeans,
                           data = complete_data2))

summary(anova_frozen <- aov(formula = Frozen ~ cluster_Kmeans,
                             data = complete_data2))

summary(anova_detergents <- aov(formula = Detergents_Paper ~ cluster_Kmeans,
                            data = complete_data2))

summary(anova_delicassen <- aov(formula = Delicassen ~ cluster_Kmeans,
                            data = complete_data2))

## Unlike the hierarchical procedure, here we see that all variables are significant for
## the formation of at least one cluster at a 95% confidence level.

# Descriptive statistics of clusters by variable

# Descriptive statistics of the 'Fresh' variable
group_by(complete_data2, cluster_Kmeans) %>%
  summarise(
    mean = mean(Fresh),
    sd = sd(Fresh),
    min = min(Fresh),
    max = max(Fresh))

# Descriptive statistics of the 'Milk' variable
group_by(complete_data2, cluster_Kmeans) %>%
  summarise(
    mean = mean(Milk),
    sd = sd(Milk),
    min = min(Milk),
    max = max(Milk))

# Descriptive statistics of the 'Grocery' variable
group_by(complete_data2, cluster_Kmeans) %>%
  summarise(
    mean = mean(Grocery),
    sd = sd(Grocery),
    min = min(Grocery),
    max = max(Grocery))

# Descriptive statistics of the 'Frozen' variable
group_by(complete_data2, cluster_Kmeans) %>%
  summarise(
    mean = mean(Frozen),
    sd = sd(Frozen),
    min = min(Frozen),
    max = max(Frozen))

# Descriptive statistics of the 'Detergents_Paper' variable
group_by(complete_data2, cluster_Kmeans) %>%
  summarise(
    mean = mean(Detergents_Paper),
    sd = sd(Detergents_Paper),
    min = min(Detergents_Paper),
    max = max(Detergents_Paper))

# Descriptive statistics of the 'Delicassen' variable
group_by(complete_data2, cluster_Kmeans) %>%
  summarise(
    mean = mean(Delicassen),
    sd = sd(Delicassen),
    min = min(Delicassen),
    max = max(Delicassen))

# Descriptive statistics of the 'Channel_2' variable
group_by(complete_data2, cluster_Kmeans) %>%
  summarise(
    mean = mean(Channel_2),
    sd = sd(Channel_2),
    min = min(Channel_2),
    max = max(Channel_2))

# Descriptive statistics of the 'Region_1' variable
group_by(complete_data2, cluster_Kmeans) %>%
  summarise(
    mean = mean(Region_1),
    sd = sd(Region_1),
    min = min(Region_1),
    max = max(Region_1))

# Descriptive statistics of the 'Region_2' variable
group_by(complete_data2, cluster_Kmeans) %>%
  summarise(
    mean = mean(Region_2),
    sd = sd(Region_2),
    min = min(Region_2),
    max = max(Region_2))

# Comparing the results of the hierarchical and non-hierarchical approaches
complete_data2 %>%
  select(Channel_2, cluster_hierarchical, cluster_Kmeans) %>%
  arrange(Channel_2) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)
## Show only the first 10 rows
# We can see a significant difference in the distribution of observations for each cluster.

# Comparing through a confusion matrix
# Create the contingency table
contingency_table <- table(complete_data2$cluster_hierarchical, complete_data2$cluster_Kmeans)

# Display the confusion matrix
confusion_matrix <- prop.table(contingency_table, margin = 1)

# Display the formatted confusion matrix
print(confusion_matrix, digits = 2)

# Concluding both procedures and analyzing the confusion matrix, we can see that both procedures
# tended to group the observations in a similar way. The confusion matrix shows us the
# percentage of observations that were grouped in the same cluster during the hierarchical clustering
# and remain grouped together in the same cluster after the "k-means" clustering. It is possible
# to notice that there was no major dispersion of observations, and the two clusters in the hierarchical
# procedure that had the greatest dispersion of observations had approximately 50% and 70% of these observations
# grouped together again.

# Thus, the effectiveness of clustering algorithms for grouping observations is demonstrated,
# and the process of using the output of the hierarchical method as input for the "k-means" method can be
# a valid strategy for this type of analysis.


