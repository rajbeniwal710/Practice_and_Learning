# ACCESSING THE DATA

data('USArrests')
View(USArrests)
str(USArrests)

# SCALLING THE DATA AND REMOVING THE NA'S

df= USArrests
dim(df)
df= na.omit(df)
str(df)
View(df)
head(df,3)

# DISTANCE MATRIX COMPUTATION

install.packages(c('cluster', 'factoextra'))
library(cluster)
library(factoextra)
set.seed(123)
ss= sample(1:50, 15)
df1= USArrests[ss, ]
head(df1, 3)
df1.scaled= scale(df1)
head(df1.scaled, 3)

#EUCLIDEAN DISTANCE

dist.eucl_15= dist(df1.scaled, method = 'euclidean')
head(dist.eucl_15, 3)

round(as.matrix(dist.eucl_15)[1:3, 1:3], 1)

fviz_dist(dist.eucl_15)

# FIND THE NUMBERS OF OPTIMUM CLUSTERS

fviz_nbclust(df, kmeans, method = 'wss') +
  geom_vline(xintercept = 4, linetype = 5, col = 'orange')



# K MEANS CLUSTERING

set.seed(123)
km.res= kmeans(df, 4, nstart = 25)
km.res

#USEFULL INFORMATION ABOUT CLUSTERS

km.res
km.res$totss
km.res$betweenss
139.5968/196

# PROFILE OF EACH CLISTER
# SEE MEANS OF VARIABLES IN ORIGINAL DATA, CLUSTER WISE

aggregate(USArrests, by = list(cluster = km.res$cluster), mean)

#ADDCLUSTER MEMBERSHIP IN DATA FILE 

df_m = cbind(USArrests, cluster = km.res$cluster)
head(df_m)

View(df_m)

#YOU CAN SAVE DATA FILE BY WRITE 

write.csv(df_m, 'D:/bsc data science notes/vinod sir/USArrests_m.csv')

fviz_cluster(km.res, data = df,
             palette = c('#2E9FDF', '#00AFBB', '#E7B800', '#FC4E07'),
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
          k_colors = c('#2E9FDF', '#00AFBB', '#E7B800', '#FC4E07'),
             color_labels_by_k = TRUE,
          rect = TRUE)


# MORE BETTER DIAGRAMS
# CUT TREE
grp= cutree(res.hc, k = 4)
head(grp, n=4)
table(grp)
rownames(df)[grp ==1]

fviz_cluster(list(data = df, cluster = grp),
             palette = c('#2E9FDF', '#00AFBB', '#E7B800', '#FC4E07'),
             ellipse = TRUE,
             ellipse.type = "convex",
             repel = TRUE,
             show.clust.cent = FALSE,
             ggtheme = theme_classic())
