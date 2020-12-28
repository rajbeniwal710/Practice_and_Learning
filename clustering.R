data(USArrests)
View(USArrests)
str(USArrests)


df <- USArrests
dim(df)           
df <- na.omit(df)
df <- scale(df)
str(df)
head(df , 3)


set.seed(123)
ss <- sample(1:50, 15)
df1 <- USArrests[ss, ]
head(df1)
df1.scaled <- scale(df1)
head(df1.scaled, 3)

dist.eucl_15 <- dist(df1.scaled, method = 'euclidean')
head(dist.eucl_15,3)
round(as.matrix(dist.eucl_15)[1:3, 1:3],1)

fviz_dist(dist.eucl_15)

