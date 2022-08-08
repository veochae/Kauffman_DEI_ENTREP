#This script is written by Veo Chae
#Data acquired from US Census

#Utilized Package
install.packages("factoextra")
install.packages("cluster")
install.packages("magrittr")

library("cluster")
library("factoextra")
library("magrittr")

#Data Import
econ <- read.csv("D:/ACS Econ Final.csv")
social <- read.csv("D:/ACS Social Final.csv")

#Innerjoin by state
final <- merge(econ,social, by = "X")
econ <- econ[-52,]


#naming each row with its respective state
row_name <- final$X
row.names(final) <- row_name
row.names(econ) <- row_name
row.names(social) <- row_name

final$X <- NULL
econ$X <- NULL
social$X <- NULL

############################## FINAL DATASET #######################################


#Showing the distance between rows of data based on its correlation.
#Warmer the color, further the distance.
res.dist <- get_dist(final, stand = TRUE, method = "pearson")
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Looking at optimal numbers of clusters for this respective dataset
fviz_nbclust(final, kmeans, method = "gap_stat")
#The result shows that 3 is the most optimal
#However, after modeling with 3 clusters and increasing the number of clusters, I found that 8 gives the most promising results. 

#K-Means Clustering
set.seed(12345)
km.res <- kmeans(final, 5, nstart = 100)
# Visualize
fviz_cluster(km.res, data = final,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

#Hierarchial Clustering
res.hc <- final %>%
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2") 
#Visualize
fviz_dend(res.hc, k = 5, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

############################# ECON DATASET ########################################

#Showing the distance between rows of data based on its correlation.
#Warmer the color, further the distance.
res.dist <- get_dist(econ, stand = TRUE, method = "pearson")
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Looking at optimal numbers of clusters for this respective dataset
fviz_nbclust(econ, kmeans, method = "gap_stat")
#The result shows that 7 is the most optimal

#K-Means Clustering
set.seed(12345)
km.res <- kmeans(econ, 5, nstart = 100)
# Visualize
fviz_cluster(km.res, data = econ,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

#hierarchial clustering 
res.hc <- econ %>%
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2") 
#Visualize
fviz_dend(res.hc, k = 5, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


########################### SOCIAL DATASET #########################################3

#Showing the distance between rows of data based on its correlation.
#Warmer the color, further the distance.
res.dist <- get_dist(social, stand = TRUE, method = "pearson")
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Looking at optimal numbers of clusters for this respective dataset
fviz_nbclust(social, kmeans, method = "gap_stat")
#The result shows that 7 is the most optimal

#K-Means Clustering
set.seed(12345)
km.res <- kmeans(social, 5, nstart = 100)
# Visualize
fviz_cluster(km.res, data = econ,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())


#hierarchial clustering 
res.hc <- social %>%
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2") 
#Visualize
fviz_dend(res.hc, k = 5, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)
