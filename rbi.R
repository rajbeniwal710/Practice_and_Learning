df1= read.csv("C:/Users/king/Desktop/rbi/RBI.csv")
dim(df1)
df1= na.omit(df1)
str(df1)

rownames(df1) <- df1$States_Union
View(df1)
df1 <- df1[, c(2:5)]
head(df1)
#set the states as rownames or indexed

#scalling so that the data points are in a prticular range
set.seed(123)
ss= sample(1:38, 15)
df1= df[ss, ]
head(df1, 3)
df1.scaled= scale(df1)
head(df1.scaled, 3)


#EUCLIDEAN DISTANCE
dist.eucl_15= get_dist(df1.scaled, method = 'euclidean')
head(dist.eucl_15, 3)
dist.eucl_15[is.na(dist.eucl_15)] <- 0 #make all the na value as 0
round(as.matrix(dist.eucl_15)[1:3, 1:3], 1)
fviz_dist(dist.eucl_15)

?fviz_dist

# FIND THE NUMBERS OF OPTIMUM CLUSTERS

fviz_nbclust(df, kmeans, method = 'wss') +
  geom_vline(xintercept = 3, linetype = 5, col = 'orange')
#so looking at the elbow curve.. its is wise to use three clusters


# K MEANS CLUSTERING

set.seed(123)
km.res= kmeans(df, 3, nstart = 40)
km.res

#USEFULL INFORMATION ABOUT CLUSTERS

km.res
km.res$totss
km.res$betweenss


# PROFILE OF EACH CLISTER
# SEE MEANS OF VARIABLES IN ORIGINAL DATA, CLUSTER WISE

aggregate(df, by = list(cluster = km.res$cluster), mean)

#ADDCLUSTER MEMBERSHIP IN DATA FILE 

df_m = cbind(df, cluster = km.res$cluster)
head(df_m)

View(df_m)

fviz_cluster(km.res, data = df,
             palette = c('#2E9FDF', '#00AFBB', '#E7B800'),
             ellipse.type = 'euclid',
             star.plot = TRUE,
             repel = TRUE,
             ggtheme = theme())

# DISSIMILARITY MATRIX

res.dist = dist(df, method = 'euclidean')
head(res.dist)
round(as.matrix(res.dist)[1:3, 1:3], 1)

fviz_dist(res.dist)
#HIERARCHICAL CLUSTERING

res.hc= hclust(d = res.dist, method = 'ward.D2')
fviz_dend(res.hc, cex = 0.5)

fviz_dend(res.hc, k = 4,
          cex = 0.5,
          k_colors = c('#2E9FDF', '#00AFBB', '#E7B800'),
          color_labels_by_k = TRUE,
          rect = TRUE)
# MORE BETTER DIAGRAMS
# CUT TREE
grp= cutree(res.hc, k = 3)
head(grp, n=3)
table(grp)
rownames(df)[grp ==1]

fviz_cluster(list(data = df, cluster = grp),
             palette = c('#2E9FDF', '#00AFBB', '#E7B800', '#FC4E07'),
             ellipse = TRUE,
             ellipse.type = "convex",
             repel = TRUE,
             show.clust.cent = FALSE,
             ggtheme = theme_classic())
