#This script has been written by Veo Chae
#This is a script continuation of Clustering.R file attached in the repository

#subsetting the cluster results from the clustering.R
overall <- as.data.frame(km.res$cluster)

#looking for the state names and adding $state for left join
overall$state <- row.names(overall)
econ$state <- row.names(econ)
social$state <- row.names(social)

#subsetting clusters for linear regression
cluster_1 <- overall[overall$`km.res$cluster` == 1,]
cluster_2 <- overall[overall$`km.res$cluster` == 2,]
cluster_3 <- overall[overall$`km.res$cluster` == 3,]
cluster_4 <- overall[overall$`km.res$cluster` == 4,]
cluster_5 <- overall[overall$`km.res$cluster` == 5,]

#left joining with econ dataset. Interchangeable by social or final
cluster_1 <- merge(cluster_1, econ, by = "state")
cluster_2 <- merge(cluster_2, econ, by = "state")
cluster_3 <- merge(cluster_3, econ, by = "state")
cluster_4 <- merge(cluster_4, econ, by = "state")
cluster_5 <- merge(cluster_5, econ, by = "state")

#loading the Kauffman Dataset and subsetting rne, per_capita and zindex for linear regression dependent variable
Kauffman <- read.csv("D:/Kauffman Dataset (2019).csv")
Kauffman$state <- Kauffman$ï..state
Kauffman$ï..state <- NULL
rne <- Kauffman[,c(3,8)]
per_capita <- Kauffman[,c(2,8)]
zindex <- Kauffman[,c(7,8)]

#left joining the clusters with the dependent variables above
cluster_1_per_capita <- merge(cluster_1, per_capita, by = "state")
cluster_2_per_capita <- merge(cluster_2, per_capita, by = "state")
cluster_3_per_capita <- merge(cluster_3, per_capita, by = "state")
cluster_3_per_capita$state <- NULL
cluster_4_per_capita <- merge(cluster_4, per_capita, by = "state")
cluster_5_per_capita <- merge(cluster_5, per_capita, by = "state")

#linear regression and summary
reg <- lm(bf_per_capita ~ ., data = cluster_3_per_capita)
summary(reg)

