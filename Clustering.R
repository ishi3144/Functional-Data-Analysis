df <- USArrests
dim(df)
head(df)
df <- na.omit(df)
df <- scale(df)
d <- dist(df, method = "euclidean")
hc <- hclust(d)
sub_grp <- cutree(hc, k = 4)
library(factoextra)
fviz_cluster(list(data = df, cluster = sub_grp))
plot(hc, cex = 0.6)

df <- USArrests
df <- na.omit(df)
df <- scale(df)
# nstart is the number of random starting points
clusters <- kmeans(df, 4, nstart = 10)
library(factoextra)
fviz_cluster(clusters, df)
### Practical Applications
library(animation)
par(mfrow = c(2,2))
kmeans.ani(df, 4)
fviz_nbclust(df, kmeans, method = "wss")