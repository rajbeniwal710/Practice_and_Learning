#vectors
#1.	Create numeric, integer, logical and character vectors


# Numeric vector stores all the real numbers
num = as.numeric(c(12,-12.07,45,78.33,0,-11,2.33))
num

# Integervector stores all the positive and negative whole numbers
int = as.integer(c(23,99,-123,23,12,56,1.2))
int

#The elements of a logical vector can have the values TRUE, FALSE, and NA
log = as.logical(c(TRUE, FALSE, FALSE, TRUE))
log

# Character vector store elements which is mainly text/string/character
# or a mixture of bot number and string
char = as.character(c("Raj","Kumar","Beniwal","Raipur", "X3date"))
char

# Access vectors using integer
# irrespective the type of vector(numeric, character, etc)
# it can accessed using a single integer of a series of integer

# in the example below, it prints the 1st element of vector num
num[1]

# in this example it prints from 2 to 4 elements of vector char
char[2:4]

# following code prints all the elements of char vector 
# except 1st element
char[-1]

# prints 1 and 3 element of the vector
char[c(1, 3)]

# accessing vector based on logical indexes
# elements corresponding to TRUE returns
char[c(TRUE,FALSE,TRUE,FALSE,FALSE)]


#	Perform arithmetic operations on vectors
# x and y are two numeric vectors
x= c(1,5,7,3,5,9,8,4)
y = c(1,4,6,8,4,3,8,9)

#addition
x + y
#substraction
x - y
#multiplication
x * y
#division
x / y

# naming vector indexes
char
names(char) <- c("First Name", "Middle Name", "Last Name", "Address", "Unique ID")
char

#accessing vector using character indexes
char["Last Name"]
#matrix
#1.	Create a matrix and access its elements.
matr = matrix(c(3:14), nrow = 4, byrow = TRUE)
matr[2,3]

# 2.	Modify matrix elements using relational operators.
matr[2,3] <- 9

# 3.	Perform addition of rows and columns in matrix
matr
matr = rbind(matr, c(22,12,21))
matr = cbind(matr, c(6,8,11,14,22))


# Importing Data Files
# 1.	Import a dataset 
df = read.csv("C:/Users/king/Desktop/test_AbJTz2l.csv")
x = df$Item_Fat_Content
rm(x)

#data frame and get its structure and summary functions
summary(df)
structure(df)


#creating a dataframe
producers <- data.frame(   
  surname =  c("Spielberg","Scorsese","Hitchcock","Tarantino","Polanski"),    
  nationality = c("US","US","UK","US","Poland"),    
  stringsAsFactors=FALSE)

# Create destination dataframe
movies <- data.frame(    
  surname = c("Spielberg",
              "Scorsese",
              "Hitchcock",
              "Hitchcock",
              "Spielberg",
              "Tarantino",
              "Polanski"),    
  title = c("Super 8",
            "Taxi Driver",
            "Psycho",
            "North by Northwest",
            "Catch Me If You Can",
            "Reservoir Dogs","Chinatown"),                
  stringsAsFactors=FALSE)

m1 <- merge(producers, movies, by.x = "surname") #full match

#partial match
# Create a new producer
add_producer <-  c('Lucas', 'US')
#  Append it to the ` producer` dataframe
producers <- rbind(producers, add_producer)
# Use a partial merge 
m3 <-merge(producers, movies, by.x = "surname", by.y = "surname", all.x = TRUE)


#Perform spreading, gathering, separate and unite functions





