df <- read.csv("cereals_data.csv")
?rename
df <- rename(df, manufacturer = mfr)
df <- transform(df, shelf = as.factor(shelf),
                     vitamins = as.factor(vitamins))
#clustering
str(df)
#since the df dataframe contains mixed datatypes, continuous and factor...
#so we cant use euclied distance since it is only used for conti dataset

#first we need to make name cplumn as index column
rownames(df) <- df$name
df <- df[,-1] #removing the first column since it remains in dataframe 
head(df,3)

#for mixed data type, "Gower Distance"metric is used
#it auto-selects the matric based on the column data-type
#it uses the metric as foloow
# continuous - euclidean
# interval - manhattan
# ordinal - first ranks it and then manhattan
# nominal - converts into k binary use dice coefficient
# gower distance is calculated using daisy function

nrow(df)
nrow(unique(df))
#so all the rows are unique in the dataset


sum(is.na(df))
#there are total 4 na in the dataset
df <- na.omit(df)
sum(is.na(df))
#so omitted all the records which had na values
nrow(df)
#so now there are 74 rows in the dataset

set.seed(150)

gower_dist <- daisy(df, metric = "gower")
summary(gower_dist)
#yeahhhhh it worked

round(as.matrix(gower_dist)[1:3, 1:3], 1)
fviz_dist(gower_dist)
#its going good...


#choosing algorithm for clustering
# partioning around medoids is used here
# simillar to k-means clusteribg
# in k-means cluster centers are defined by euclidean dist
# but here PAM cluster centres are restricted to the observations themeselves(medoids)

# using silhouette width to ensure optimum number of clusters
# choses clusters with higher value
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}
plot(1:10, sil_width,
     xlab = "No of Clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width) 
#yesssss finallyyy....


pam_fit_ <- pam(gower_dist, diss = TRUE, k = 5)
pam_fit_

pam_fit_$clustering
df_m <- cbind(df, cluster =pam_fit_$clustering)
head(df_m, 3)

tsne_obj <- Rtsne(gower_dist,is_distance = TRUE, perplexity = 7)
tsne_obj$Y

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X","Y")) %>%
  mutate(cluster = factor(pam_fit_$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) + 
  geom_point(aes(color = cluster), size = 4) +
  theme_dark() + labs(title = paste("Clustering of Cereals Dataset"), size = 18)
?geom_point
?geom_label
?labs
