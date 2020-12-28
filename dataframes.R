city = data.frame(name=c("bangalore","mumbai","delhi"),
                  sales=c(1000,4000,3000),stringsAsFactors = FALSE)
movies=data.frame(name=c("bangalore","mumbai","delhi"),
                  title=c("super8","psycho","icy"),stringsAsFactors = FALSE)
m1 = merge(city,movies, by.x="name")
m1
