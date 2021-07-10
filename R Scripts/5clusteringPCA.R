library(dplyr)
library(dummies)
library(rgl)
library(cluster)


set.seed(12345)


homeData <- read.table("HomeDataClean.csv", header=TRUE, row.names=1, sep=",",
                       comment.char="", colClasses=c("character", 
                       rep("factor",2), rep("numeric",4), rep("factor",3))) 


senicData <- read.table("SENIC.csv", header=TRUE, row.names=1, sep=",",
                        colClasses=c(rep("numeric",7),rep("factor",2),
                        rep("numeric",9), rep("factor",2)))


homeDF <- dummy.data.frame(homeData[,c("Bedrooms", "Baths", 
                                       "Sq..Ft.", "Price", "Realtor.Group")],
                           names="Realtor.Group")
head(homeDF)


homeDF <- scale(homeDF, center=TRUE, scale=TRUE)
head(homeDF)


homeKmeans <- kmeans(homeDF, centers=4)


homePCA <- prcomp(homeDF, retx=TRUE)
plot(homePCA$x[,1:2], col=homeKmeans$cluster, pch=homeKmeans$cluster)


homeDist <- dist(homeDF)
homeSil <- silhouette(homeKmeans$cluster, homeDist)
plot(homeSil)


summary(homePCA)


homePCA$rotation


plot(homePCA)


plot3d(homePCA$x[,1:3],col=homeKmeans$cluster)


