#This script is written by Veo Chae
#Data acquired from US Census

#Utilized Package
install.packages("factoextra")
install.packages("cluster")
install.packages("magrittr")

library("cluster")
library("factoextra")
library("magrittr")
library("dplyr")

#Data Import
econ <- read.csv("C:/Users/dchae2/Desktop/ACS Econ Final.csv")
social <- read.csv("C:/Users/dchae2/Desktop/ACS Social Final.csv")

#Inner-join by state
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

#UDF for subsetting by wanted category of questions
index_subset <- function(names,dataset){
  index <- grepl(names, colnames(dataset))
  subset <- final[index]
  
  return(subset)
}

################################################################################
################################################################################

#FULL STACK CLUSTER GENERATOR
  #names: Input the alphabetic code of choice for questionaire
    #name should be inputted as following: 
        #"A|B|C"
  #dataset: for the purpose of this project, it will be "final" dataset.
  
  #SAMPLE INPUT: index_best_k("A|B|C", final)

index_best_k <- function(names, dataset){
  
  #Using the UDF from Line 35 to create a subset with only the questionaires to use
  subset <- index_subset(names,dataset)
  
  #Looking for the best value K using gap_stat. 
  #You can change to "silohouette" if seem apt.
  best_k_find <- fviz_nbclust(subset, kmeans, method = "gap_stat")
  
  #prints the line graph of best K fit.
  print(best_k_find)
  
  #creating a data table with highest SE.Sim value --> what determines K
  x <- as.data.frame(cbind(best_k_find$data$clusters,best_k_find$data$SE.sim))
  
  #If the best_k is 1, automatically switched to 2 because you need at least 2 for clustering
  best_k <- if(which.max(x[,2]) == 1){
    best_k <- 2 } else{
      which.max(x[,2])
    }
  
  set.seed(12345)
  #General K-Means visualization start
  km.res <- kmeans(subset, best_k, nstart = 100)
  # Visualize
  Cluster_viz <- fviz_cluster(km.res, data = subset,
               ellipse.type = "convex",
               palette = "jco",
               ggtheme = theme_minimal())
  
  #Hierarchial Clustering
  res.hc <- subset %>%
    dist(method = "euclidean") %>% # Compute dissimilarity matrix
    hclust(method = "ward.D2") 
  #Visualize
  dendiogram <- 
    fviz_dend(res.hc, k = best_k, # Cut in four groups
            cex = 0.5, # label size
            k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
            color_labels_by_k = TRUE, # color labels by groups
            rect = TRUE) # Add rectangle around groups
  
  print(Cluster_viz)
  print(dendiogram)
  
}

#Sample
index_best_k("A|G",final)




stri_rand_strings(n, 1, '[A-Z]')
