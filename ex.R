data <- read.csv("C:/Users/king/Documents/cs2m.csv")
data
str(data)
#checking the structure of the dataframe(task 2)

task1 <- data[ ,c(4:6,1:3)]
task1
#rearranging the columns.... categorical columns first and then the conti. columns
?heatmap

heat_ <- as.matrix(data)
heat_
heatmap(heat_)
colnames(heat_) <- NULL
heat_
heatmap(heat_,Colv = NA,Rowv = NA, labCol = colnames(data), cexCol = 1, scale = "column",col = heat.colors(30))
?terrain.colors
?palette
#task 3 heatmap

cnt <- data[ ,c(1:3)]
cnt
#task 4 storing conti variables in cnt


install.packages("PerformanceAnalytics")

chart.Correlation(cnt, histogram = TRUE, method = "pearson")
#task5



task7 <- cnt[1:10, c(1,3)]
task7
#10 rows of bo and age




task8 <- data[order(data$Age),]
task8



colnames(data)[1] = "Blood_Pressure"
data


t_ <- ifelse(data$Blood_Pressure < 125,"Low","High")
task15 <- data
task15$bpstatus <- t_
task15
#adding anew column to the esixsting dataframe with if else statement in it



grades <- read.csv("C:/Users/king/Documents/grades.csv")

task9 <- table(grades$ethnicity)
label_ <- c("Black", "Brown", "Australian", "Hispanic", "American")
pie3D(task9, labels = label_, labelcex = 1)
?pie3D
#3d pie chart 

?recode_factor
recode


table(grades$passfail)
str(grades$passfail)
#only one outstanding result

sample_ <- grades[sample(1:nrow(grades), 0.5 * nrow(grades), replace=FALSE),]
sample_
0.5 * nrow(grades)
ncol(grades)
grades[82,22] <- "P"
grades
sapply(split(grades$gpa,grades$gender),mean)
sapply(split(grades$gpa,grades$gender,grades$ethnicity),mean)
?mean
t <- read.csv("*C:/Users/king/Documents/t.csv,"