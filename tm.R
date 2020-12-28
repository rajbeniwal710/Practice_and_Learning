getwd()
readLines("Acknowledgment.txt")
#reading the text document



a <- c("Vehli","Janta","Rocks")
a
paste(a, collapse = " ")


chunk2 <- readLines("Acknowledgment.txt") 
chunk_pasted_2 <- paste(chunk2, collapse = " ")
head(chunk_pasted_2)

#lower case
clean_data1 <- tolower(chunk_pasted_2)
clean_data1

#removing the punctuatuion and adding the text
clean_data2 <- gsub(pattern = "\\W", replacement = " ", clean_data1)
clean_data2
?gsub


#removing the digits
clean_data3 <- gsub(pattern = "\\d", replacement = " ", clean_data2)
head(clean_data3)

#preview stopwords
stopwords()

#removing stopword from the data
clean_data4 <- removeWords(clean_data3, stopwords())
head(clean_data4)



#remove single letters
clean_data5 <- gsub(pattern = "\\b[A-z]\\b{1}", replacement = " ",clean_data4)
head(clean_data5)


#remove whitespaces
clean_data6 <- stripWhitespace(clean_data5)
head(clean_data6)

#spliting the word
clean_data7 <- strsplit(clean_data6, " ")
clean_data7


word_freq <- table(clean_data7)
head(word_freq)
word_freq1 <- cbind(names(word_freq), as.integer(word_freq))
head(word_freq1)

write.csv(word_freq1,"Word_Frequency.csv")
