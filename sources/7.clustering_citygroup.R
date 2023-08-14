setwd("C:/Users/Emilia Hoang/Desktop/p2_emilia_hoang/data")

#1.Load the dataset 
data <- read.csv("data_prepared.csv")
table(data$HQ_city)

#Select columns for rating analysis
selected_columns <- c("HQ_city","rating","company_age","avg_salary_estimate")
# Create a new dataframe with the selected columns
new_data <- data[selected_columns]

library(dplyr)
group_data <- new_data %>%
  group_by(HQ_city) %>%
  summarize(
    mean_rating = mean(rating),
    median_company_age = median(company_age),
    mean_avg_salary = mean(avg_salary_estimate)
  )
group_data_ <- as.data.frame(group_data)
summary(group_data)

#1.Standard scale the feature: 
vars_to_use <- colnames(group_data)[-1] 
pmatrix <- scale(group_data[, vars_to_use])
summary(pmatrix)

#2.Create K-means clustering
kbest_p <- 5 
pclusters <- kmeans(pmatrix, kbest_p, nstart = 100, iter.max = 100)
summary(pclusters)

#3.Interpret result
pclusters$centers
pclusters$size #number of rows in each cluster

#Print details of each cluster
#---->Create a print function
print_clusters <- function(data, groups, columns){
  groupedData <- split(group_data, groups) 
  lapply(groupedData, function (df) df[, columns]) 
}

cols_to_print <- c("HQ_city", "mean_rating","median_company_age","mean_avg_salary") 
groups <- pclusters$cluster 
print_clusters(group_data, groups, cols_to_print)

#4.Evaluate the result of K-means clustering by calculate WWS
#Calculate square distance
sqr_edist <- function(x,y){
  sum((x-y)^2)
}

#Calculate WSS for each cluster
wss_each_cluster <- function(clustermatrix){
  c0 <- colMeans(clustermatrix)   
  sum(apply(    
    clustermatrix, 1, FUN = function(row){   
      sqr_edist(row, c0)  
    }
  ))
}

#Calculate total WSS
wss_total <- function(dmatrix, labels){
  wsstotal <- 0 
  k <- length(unique(labels))
  for (i in 1:k)
    wsstotal <- wsstotal + wss_each_cluster(subset(dmatrix, labels == i))
  wsstotal
}

wss_total(pmatrix, groups)


#How can we set the number of K?
#---> Use the elbow method
get_wss<-function(dmatrix, max_clusters) { 
  wss = numeric(max_clusters) 
  wss[1] <- wss_each_cluster(dmatrix) 
  
  for(k in 2:max_clusters) { 
    pclusters1 <- kmeans(pmatrix, k, nstart = 100, iter.max = 100) 
    groups <- pclusters1$cluster
    wss[k] <- wss_total(dmatrix, groups) 
  }
  wss
}

#5.Run the function to get WSS for each K clusters
kmax<-10
cluster_measure<-data.frame(nclusters= 1:kmax, wss= get_wss(pmatrix, kmax))

#Plot the result to determine which K is the best
library(ggplot2)
breaks <-1:kmax
ggplot(cluster_measure, aes(x=nclusters, y = wss)) + 
  geom_point() + geom_line() +
  scale_x_continuous(breaks = breaks)


#7.Hierarchical clustering
dist_matrix <- dist(pmatrix, method = "euclidean")
pfit <- hclust(dist_matrix, method = "ward.D") 
plot(pfit, labels = group_data$HQ_city)
rect.hclust(pfit, k=5) 
#Extract the members of each cluster from the hclustobject
print_clusters(group_data, groups, cols_to_print)
