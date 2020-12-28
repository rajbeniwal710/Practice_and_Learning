setwd("C:/Users/king/Desktop/project1")
library(ggplot2)
library(dplyr)
pcr <- read.csv("cereals_data.csv")
#change column names for better visualisation... and understanding
colnames(pcr) <- c("Product's Name","Manufacturer","Type","Calories","Protein","Fat",
                   "Sodium","Fiber","Carbohydrates","Sugars","Potass","Vitamins","Shelf","Weight",
                   "Cups","Rating")
#as mentioned in the cereal description vitamins and shelf are factor.. so we need to change its datatype
str(pcr)
pcr <- transform(pcr, Shelf = as.factor(Shelf),
                     Vitamins = as.factor(Vitamins))

#


#changing h and c in type column as hot and cold
library(plyr)
head(pcr)
pcr$Type <- revalue(pcr$Type, c("H"="Hot","C"="Cold"))
#changing the companies name to real name
pcr$Manufacturer <- revalue(pcr$Manufacturer,c("N"="Nabisco","Q"="Quaker Oats","K"="Kelloggs", "R"="Ralston Purina","G"="General Mills","P"="Post","A"="AHFP"))
sum(is.na(pcr))
library(DMwR) #missing value imputation
pcr <- knnImputation(pcr)
sum(is.na(pcr))


#principal component analysis
#lets start
library(PCAmixdata)
 
str(pcr)#something new...
# using this package for mixed data PCA
#lets store the data into 2 different quantitative and qualitativ matrix
?PCAmix
quanti <- pcr %>%
  select(Calories, Protein,Fat,Sodium,Fiber,Carbohydrates,Sugars,Potass,Weight,Cups,Rating)
head(quanti,3)
quali <- pcr %>%
  select(Vitamins,Shelf)
prca <- PCAmix(X.quanti = quanti, X.quali = quali, ndim = 5, graph = TRUE)
summary(prca)
sqd <- round(prca$sqload,3)
write.csv(sqd,"sqd.csv")


#looking at the the graph we can choose rating, calories, fiber, potass, weight as 
#our variable for cluster analysis
c_cluster <- pcr %>%
  select(Product.s.Name, Rating, Calories, Fiber, Potass,Weight)
rownames(c_cluster) <- c_cluster$Product.s.Name
c_cluster$Calories <- as.numeric(c_cluster$Calories)
View(c_cluster)
c_cluster <- c_cluster[ ,-1]

#scalling so that the data points are in a prticular range
set.seed(123)
ss= sample(1:77, 30)
df1= c_cluster[ss, ]
head(df1, 3)
df1.scaled= scale(df1)
head(df1.scaled, 3)

#EUCLIDEAN DISTANCE
library(factoextra)
dist.eucl_15= get_dist(df1.scaled, method = 'euclidean')
head(dist.eucl_15, 3)
dist.eucl_15[is.na(dist.eucl_15)] <- 0 #make all the na value as 0
round(as.matrix(dist.eucl_15)[1:3, 1:3], 1)
fviz_dist(dist.eucl_15)
View(c_cluster)
# FIND THE NUMBERS OF OPTIMUM CLUSTERS
class(df)
c_cluster <- as.matrix(c_cluster)

fviz_nbclust(c_cluster, kmeans, method = 'wss') +
  geom_vline(xintercept = 3, linetype = 5, col = 'orange')
#so looking at the elbow curve.. its is wise to use three clusters

# K MEANS CLUSTERING

set.seed(123)
km.res= kmeans(c_cluster, 3, nstart = 40)
km.res
km.res$cluster
# PROFILE OF EACH CLISTER
# SEE MEANS OF VARIABLES IN ORIGINAL DATA, CLUSTER WISE

aggregate(c_cluster, by = list(cluster = km.res$cluster), mean)

#ADDCLUSTER MEMBERSHIP IN DATA FILE 

df_m = cbind(c_cluster, cluster = km.res$cluster)
head(df_m)
df <- cbind(pcr, cluster = km.res$cluster)
head(df)
d <- df %>%
  select(Calories, cluster) %>%
  group_by(cluster) %>%
  summarise_each(funs(mean(Calories)))
d
df <- transform(df, cluster = as.factor(cluster))
ggplot(data = df, aes(x=Rating, y= Calories, color = cluster, fill = cluster, size = Fat)) + geom_point(alpha = 0.7) + theme_grey()
ggplot(data = df, aes(x=Calories, fill = cluster))+ geom_histogram(bins = 13, binwidth = 10)






write.csv(c,"no_mean_cluster.csv")


fviz_cluster(km.res, data = c_cluster,
             palette = c('#2E9FDF', '#E7B800','#FC4E07'),
             ellipse.type = 'convex',
             ellipse.alpha = 0.4,
             pointsize = 3,
             main = "Calories Cluster",
             star.plot = TRUE,
             geom = "point",
             repel = TRUE,
             ggtheme = theme_bw())
?palette
?fviz_cluster

#DISSIMILARITY MATRIX

res.dist = dist(c_cluster, method = 'euclidean')
head(res.dist)
round(as.matrix(res.dist)[1:3, 1:3], 1)

fviz_dist(res.dist)

res.hc= hclust(d = res.dist, method = 'ward.D2')
fviz_dend(res.hc, cex = 0.5)
group_by(df_m$cluster)

fviz_dend(res.hc, k = 3,
          cex = 0.5,
          k_colors = c('#2E9FDF', '#FC4E07', '#E7B800'),
          color_labels_by_k = TRUE,
          rect = TRUE)
# MORE BETTER DIAGRAMS
# CUT TREE
grp= cutree(res.hc, k = 3)
head(grp, n=3)
table(grp)
rownames(df)[grp ==1]

fviz_cluster(list(data = c_cluster, cluster = grp),
             palette = c('#00AFBB', '#E7B800', '#FC4E07'),
             ellipse = TRUE,
             ellipse.type = "convex",
             repel = TRUE,
             geom = "point",
             show.clust.cent = FALSE,
             main = "Cereals Cluster Plot",
             ggtheme = theme_bw())
?fviz_cluster

