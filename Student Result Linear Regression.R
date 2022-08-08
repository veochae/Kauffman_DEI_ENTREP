#This script has been written by Veo Chae
#The datasets can be found in
  #https://indicators.kauffman.org/data-tables
  #https://www.census.gov/programs-surveys/acs
  #https://www.census.gov/programs-surveys/acs
#This script analyzes the student driven responses of ACS Dataset
#For the 22 students analyzed, the Top 5 variables of importance has been utilized for Linear Regression

#import data
econ <- read.csv("D:/ACS Econ Final.csv")
social <- read.csv("D:/ACS Social Final.csv")
Kauffman <- read.csv("D:/Kauffman Dataset (2019).csv")

#Innerjoin by state
final <- merge(econ,social, by = "X")

#Utilizing only the 5 variables that the student result shows
student <- final[,c("E3","L9","L7","S5","U1")]

#adding state to the student dataset for inner join with Kauffman Dataset
student$state <- final$X



##############################################
############## Linear Regression #############
##############################################

#PLEASE RUN THE CODES LINE 9 to 30 and 67 to 101 in Clustering.R before beginning

#subsetting the cluster results from the clustering.R
overall <- as.data.frame(km.res$cluster)

#looking for the state names and adding $state for left join
overall$state <- row.names(overall)

#subsetting clusters for linear regression
cluster_1 <- overall[overall$`km.res$cluster` == 1,]
cluster_2 <- overall[overall$`km.res$cluster` == 2,]
cluster_3 <- overall[overall$`km.res$cluster` == 3,]
cluster_4 <- overall[overall$`km.res$cluster` == 4,]
cluster_5 <- overall[overall$`km.res$cluster` == 5,]

#left joining with econ dataset. Interchangeable by social or final
cluster_1 <- merge(cluster_1, student, by = "state")
cluster_2 <- merge(cluster_2, student, by = "state")
cluster_3 <- merge(cluster_3, student, by = "state")
cluster_4 <- merge(cluster_4, student, by = "state")
cluster_5 <- merge(cluster_5, student, by = "state")

#loading the Kauffman Dataset and subsetting rne, per_capita and zindex for linear regression dependent variable
Kauffman$state <- Kauffman$ï..state
Kauffman$ï..state <- NULL
rne <- Kauffman[,c(3,8)]
per_capita <- Kauffman[,c(2,8)]
zindex <- Kauffman[,c(7,8)]

#left joining the clusters with the dependent variables above
#Interchangeable with rne and zindex
cluster_1_per_capita <- merge(cluster_1, zindex, by = "state")
cluster_2_per_capita <- merge(cluster_2, zindex, by = "state")
cluster_3_per_capita <- merge(cluster_3, zindex, by = "state")
cluster_4_per_capita <- merge(cluster_4, zindex, by = "state")
cluster_5_per_capita <- merge(cluster_5, zindex, by = "state")

#deleting state since it is no longer used and is present as row name
cluster_1_per_capita$state <- NULL
cluster_2_per_capita$state <- NULL
cluster_3_per_capita$state <- NULL
cluster_4_per_capita$state <- NULL
cluster_5_per_capita$state <- NULL

#deleting cluster number count extracted from clustering
cluster_1_per_capita$`km.res$cluster` <- NULL
cluster_2_per_capita$`km.res$cluster` <- NULL
cluster_3_per_capita$`km.res$cluster` <- NULL
cluster_4_per_capita$`km.res$cluster` <- NULL
cluster_5_per_capita$`km.res$cluster` <- NULL

#linear regression and summary
reg_1 <- lm(zindex ~ ., data = cluster_1_per_capita)
reg_2 <- lm(zindex ~ ., data = cluster_2_per_capita)
reg_3 <- lm(zindex ~ ., data = cluster_3_per_capita)
reg_4 <- lm(zindex ~ ., data = cluster_4_per_capita)
reg_5 <- lm(zindex ~ ., data = cluster_5_per_capita)

#linear regression summary
summary(reg_1)
summary(reg_2)
summary(reg_3)
summary(reg_4)
summary(reg_5)












