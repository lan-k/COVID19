####cluster weights
rm(list = ls())

library(tidyverse)
library(runner)
library(mclust)
library(cluster) 
library(fpc)

###age_adjust to Australian population profile

setwd('D:/work/SAHMRI/COVID-19/Data')

source('D:/work/SAHMRI/COVID-19/R code/age adjusted weights international.R')

length(t(pop_list))
###weights for all countries in WHO
weights_all <- calc_weights("Australia",as.vector(t(pop_list)))

summary(weights_all$weight_death)
wt_death <- scale(weights_all$weight_death)

##find k-means groups
wss <- (nrow(wt_death)-1)*sum(apply(wt_death,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(wt_death,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

#use 3 groups

##cluster into groups
set.seed(123)


wt_death <- weights_all$weight_death

km.res.2 <- kmeans(wt_death, 2, nstart = 25)
print(km.res.2)

km.res.3 <- kmeans(wt_death, 3, nstart = 25)
print(km.res.3)


aggregate(wt_death,by=list(km.res.3$cluster),FUN=mean)
aggregate(wt_death,by=list(km.res.3$cluster),FUN=min)
aggregate(wt_death,by=list(km.res.3$cluster),FUN=max)

weights_all <- data.frame(weights_all, km.res.3$cluster) 


# clusplot(scale(wt_death), km.res.3$cluster, color=TRUE, shade=TRUE,
#          labels=2, lines=0)

###dendrograms

# Ward Hierarchical Clustering
d <- dist(wt_death, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram
groups <- cutree(fit, k=3) # cut tree into 3 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=3, border="red") 

aggregate(wt_death,by=list(groups),FUN=mean)
aggregate(wt_death,by=list(groups),FUN=min)
aggregate(wt_death,by=list(groups),FUN=max)

weights_all <- data.frame(weights_all, groups) 


###model based clustering

fit <- Mclust(wt_death)
plot(fit) # plot results
summary(fit) # display the best model 

aggregate(wt_death,by=list(fit$classification),FUN=mean)
aggregate(wt_death,by=list(fit$classification),FUN=min)
aggregate(wt_death,by=list(fit$classification),FUN=max)

weights_all <- data.frame(weights_all, fit$classification) 

