library(readr)
zoo1 <- read_csv("IST 707/zoo.csv")

#Remove names and type

zoo<- zoo1[,c(2:17)]
str(zoo)

install.packages("RWeka")
library(RWeka)
model_rweka <- SimpleKMeans(zoo, control = Weka_control(N=7, I=500, S=100))

model_r <- kmeans(zoo,7)

model_r$centers

cluster_assignment <- data.frame(zoo1, model_r$cluster)

plot(zoo1$type ~ jitter(model_r$cluster,1), pch=21, col=as.factor(zoo1$milk))

install.packages("cluster")
library(cluster)
clusplot(zoo, model_r$cluster,color=TRUE, shade=TRUE,labels=2, lines=0)

d=dist(as.matrix(zoo))
hc=hclust(d)
plot(hc)

clusters <- hclust(dist(zoo1))
plot(clusters)

clusterCut <- cutree(clusters, 5)

table(clusterCut, zoo1$type)
clusters <- hclust(dist(zoo1), method = 'average')
plot(clusters)

clusterCut <- cutree(clusters, 5)
table(clusterCut, zoo1$type)
