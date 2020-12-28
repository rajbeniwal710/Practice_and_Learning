#reading the in-built iris dataset
df <- iris
head(df)
#reading the first four columns of the dataset
df <- df[,1:4]
#dimension of the dataset
dim(df)
#structure of dataframe
str(df)

# FIND THE NUMBERS OF OPTIMUM CLUSTERS
library(cluster)
library(factoextra)
fviz_nbclust(df, kmeans, method = 'wss') +
  geom_vline(xintercept = 3, linetype = 5, col = 'orange')
#so looking at the elbow curve.. its is wise to use three clusters

# K MEANS CLUSTERING
set.seed(123)
km.res= kmeans(df, 3, nstart = 40)

#USEFULL INFORMATION ABOUT CLUSTERS
print(km.res)
fviz_cluster(km.res, data = df,
             palette = c('#2E9FDF', '#E7B800', '#FC4E07'),
             ellipse = TRUE,
             ellipse.type = "convex",
             show.clust.cent = FALSE,
             ggtheme = theme_classic())
# PROFILE OF EACH CLISTER
# SEE MEANS OF VARIABLES IN ORIGINAL DATA, CLUSTER WISE
aggregate(df, by = list(cluster = km.res$cluster), mean)


#HIERARCHICAL CLUSTERING
res.dist = dist(df, method = 'euclidean')
res.hc= hclust(d = res.dist, method = 'ward.D2')

fviz_dend(res.hc, k = 3,
          cex = 0.5,
          k_colors = c('#2E9FDF', '#FC4E07', '#E7B800'),
          color_labels_by_k = TRUE,
          rect = TRUE)
