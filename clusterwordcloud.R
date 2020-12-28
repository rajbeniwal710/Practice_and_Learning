setwd("C:/Users/king/Desktop/project1")
readLines("calories.txt")
text_ <- readLines("calories.txt")
library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)



#creating a corpus
docs_ <- Corpus(VectorSource(text_))

#data cleaning
docs_ <- docs_ %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs_ <- tm_map(docs_, content_transformer(tolower))
docs_ <- tm_map(docs_,removeWords,c(stopwords("english"),"calorie","can","people","may"))

#creating a document termes marix
dtm <- TermDocumentMatrix(docs_)
mat <- as.matrix(dtm)
words <- sort(rowSums(mat))
df <- data.frame(word = names(words), freq = words)
df <- df[order(df$freq, decreasing = TRUE),]
head(df)
#creating wordcloud
library(wordcloud2)
set.seed(1234)

wordcloud2(data = df,color = "random-dark",backgroundColor = "black")

