setwd("C:/Users/king/Desktop/project1")
library(ggplot2)
library(dplyr)
project <- read.csv("cereals_data.csv")

colnames(project)#column names in the cereals data file

ncol(project)
#total column in the datafile

str(project)
nrow(unique(project))
#checking if there are duplicate rows
#(unique(project$rating)

#as mentioned in the cereal description vitamins and shelf are factor.. so we need to change its datatype
project <- transform(project, shelf = as.factor(shelf),
                     vitamins = as.factor(vitamins))

 #change column names for better visualisation... and understanding
?colnames
colnames(project)
colnames(project) <- c("Product's Name","Manufacturer","Type","Calories","Protein","Fat",
                       "Sodium","Fiber","Carbohydrates","Sugars","Potass","Vitamins","Shelf","Weight",
                       "Cups","Rating")
#changing h and c in type column as hot and cold
library(plyr)
temp <- project
project$Type <- revalue(project$Type, c("H"="Hot","C"="Cold"))
#changing the companies name to real name
project$Manufacturer <- revalue(project$Manufacturer,c("N"="Nabisco","Q"="Quaker Oats","K"="Kelloggs", "R"="Ralston Purina","G"="General Mills","P"="Post","A"="AHFP"))
unique(project$Manufacturer)

#now lets deal with missing values
# first count the number of missing values
sum(is.na(project))
# so there are four missing values
#so now lets find out to which column they are...
colSums(is.na(project))

# looking at the results
# there are 2 missing value in Potass
# and one NAs in Sugars and Carbohydrates
# lets recheck the structure of project to ensure the datatype of each column
str(project)

# so, potas and sugars are integer whereas carbohydrates are numeric
#since the data contains factors as hot and cold
na_type <- project %>% select(Type,Carbohydrates,Potass,Sugars) %>%
  group_by(Type) %>% summarise_each(funs(sum(is.na(.))))
na_type
write.csv(na_type,"Missing_Value.csv")
#so there is only one missing value in COLD
# whereas 3 missing values in HOT
#plotting histogram/density plot to check its distribution
hist(project$Carbohydrates)
plot(density(project$Carbohydrates, na.rm = TRUE), main = "Density Plot of Carbohydrates")

# plotting density plot based on type group
ggplot(project, aes(x=Calories)) + 
  geom_density(size=2,alpha=.4, color = "blue", fill = "blue") + theme_bw()
?ggtitle
# since the distribution of carbo in hot doesnt follow normal distribution
# filtering the records so that it only contain required fields and Hot type
hot_ <- project %>% filter(Type == 'Hot')
hot_
#since there are only there 3 records for HOT type
# so we have insufficient data to make computation solely based on hot type
#thus using knn imputation for replacing missing values irrespective of its type
library(DMwR)
project <- knnImputation(project)
sum(is.na(project))
#so there are no missing value in the dataset


#boxplot for protein, fat, fiber, carbo,sugars
#because they have simmilar range
boxplot(project[ , c(5,6,8,9,10)], col = topo.colors(5))
#boxplot for sodium,potass and rating
boxplot(project[ , c(7,11,16)], col = terrain.colors(3))

ggplot(data = project) +
  geom_hex(mapping = aes(x = Vitamins, y=Calories), na.rm = TRUE) + theme_dark()



#distribution of calories with respective to different brands
# manufacturer and different brands
# histogram
#project %>% 
 # ggplot(aes(x = Calories, fill = Manufacturer)) + geom_histogram() +
  #scale_x_continuous(name = "Calories", expand = c(0,0))+
  #scale_y_continuous(expand = c(0,0), limits = c(0,10), breaks = seq(0,10,2))
#density plot
mfr_count <- project %>% 
  select(Manufacturer, Calories) %>%
  group_by(Manufacturer) %>%
  tally()
mfr_count
ggplot(project, aes(x=Calories)) +
  geom_histogram(aes(fill=Manufacturer))
# when we look at the no products from different manufacturer
# we notice that there is only product from AHFP

#correlation matrix between conti. data
library(GGally)
project %>%
  select(Calories, Protein, Fat, Sodium, Fiber, Carbo = Carbohydrates, Sugars, Potass, Weight, Rating) %>%
  ggcorr(label = TRUE)
?pairs
pairs(~ Calories + Protein + Fat + Sodium + Fiber +
        Carbohydrates + Sugars + Potass + Weight + Rating, data = project)
#scatter plot of weight and calories
#hex chart
project %>%
  ggplot(aes(x=Weight, y=Calories)) +
  geom_hex(size = 3) + theme_bw()
#hist type, calories
mtc <- project %>%
  select(Manufacturer,Type, Calories) %>%
  group_by(Type,Manufacturer) %>%
  summarise_each(funs(mean(.),frequency = n()))
write.csv(mtc, "mtc.csv")
ggplot(data = mtc) +
  geom_histogram(aes(x=mean, fill = Manufacturer, bins = 30))
#shelf
project %>%
  ggplot(aes(x=Weight, y=Calories, color = Shelf)) +
  geom_point(size = 3) + theme_bw()
#vitamins
project %>%
  ggplot(aes(x=Weight, y=Calories, color =Vitamins)) +
  geom_point(size = 3) + theme_bw()

# Scatter plot for calories and rating
#scatter plot
project %>%
  ggplot(aes(x=Rating, y=Calories, color = Manufacturer)) +
  geom_point(size = 5) + theme_bw()

#sodium
project %>%
  ggplot(aes(x=Sodium, y=Calories)) +
  geom_hex(size = 3) + theme_bw()
#sugars
project %>%
  ggplot(aes(x=Sugars, y=Calories)) +
  geom_hex(size = 3) + theme_bw()
#fat
project %>%
  ggplot(aes(x=Fat, y=Calories)) +
  geom_hex(size = 3) + theme_bw()


#boxplot for different categories
#manufacturer
project %>%
  ggplot(aes(y = Calories, x = Manufacturer, fill = Manufacturer)) +
  geom_boxplot() + geom_jitter(alpha = 0.6, width = 0.1)

project %>%
  ggplot(aes(y = Calories, x = Manufacturer, fill = Vitamins)) +
  geom_boxplot() + geom_jitter(alpha = 0.6, width = 0.1)
  
project %>%
  ggplot(aes(y = Calories, x = Manufacturer, fill = Type)) +
  geom_boxplot() + geom_jitter(alpha = 0.6, width = 0.1)

#shelf
project %>%
  ggplot(aes(y = Calories, x = Shelf, fill = Shelf)) +
  geom_boxplot() + geom_jitter(alpha = 0.6, width = 0.1)
sh <- project %>%
  select(Shelf, Calories) %>%
  group_by(Shelf) %>%
  summarise_each(funs(mean(.),frequency = n()))
sh
write.csv(sh,"shelf.csv")
#type
project %>%
  ggplot(aes(y = Calories, x = Type, fill = Type)) +
  geom_boxplot() + geom_jitter(alpha = 0.6, width = 0.1)
#vitamins
project %>%
  ggplot(aes(y = Calories, x = Vitamins, fill = Vitamins)) +
  geom_boxplot() + geom_jitter(alpha = 0.6, width = 0.1)

mrc <- project %>%
  select(Manufacturer,Calories,Rating) %>%
  group_by(Manufacturer) %>%
  summarise_each(funs(mean(.))) 
mrc <- cbind(mrc, frequency = mfr_count$n)  
mrc
write.csv(mrc,"mrc.csv")

ggplot(project, aes(x=Protein)) + 
  geom_density(size=2,alpha=.4, color = "blue", fill = "blue") + theme_bw()
#no normal distribution

ggplot(project, aes(x=Fat)) + 
  geom_density(size=2,alpha=.4, color = "blue", fill = "blue") + theme_bw()
#no normal distribution

ggplot(project, aes(x=Sodium)) + 
  geom_density(size=2,alpha=.4, color = "blue", fill = "blue") + theme_bw()
#normally distributed

ggplot(project, aes(x=Fiber)) + 
  geom_density(size=2,alpha=.4, color = "blue", fill = "blue") + theme_bw()
#not normally distributed

ggplot(project, aes(x=Sugars)) + 
  geom_density(size=2,alpha=.4, color = "blue", fill = "blue") + theme_bw()
#no normally distributed

ggplot(project, aes(x=Potass)) + 
  geom_density(size=2,alpha=.4, color = "blue", fill = "blue") + theme_bw()
#no normally distributed

ggplot(project, aes(x=Weight)) + 
  geom_density(size=2,alpha=.4, color = "blue", fill = "blue") + theme_bw()
#normally distributed

ggplot(project, aes(x=Carbohydrates)) + 
  geom_density(size=2,alpha=.4, color = "blue", fill = "blue") + theme_bw()
#normally distributed

ggplot(project, aes(x=Cups)) + 
  geom_density(size=2,alpha=.4, color = "blue", fill = "blue") + theme_bw()
#normally distributed

ggplot(project, aes(x=Rating)) + 
  geom_density(size=2,alpha=.4, color = "blue", fill = "blue") + theme_bw()
#not normally distributed

hist(project$Fat)


















