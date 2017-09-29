##Intro!

##Atomic Vector
vec <- c(1,2,3,4)

class(vec) #class of obeject
typeof(vec) #R's internal type (? What is this for?)
str(vec) #structure

vec2 <- vec

vec <- c('a','b','c')
vec[1]

#MATRIX
mat <- matrix(c(1, 2, 3, 4), 2, 2)
mat[1,1] #R always expect row first, column second 

#LIST
test <- list(c(1,2), 'cat', mtcars) #this is a mixed list 
test[1]
class(test[2]) #single bracket: this refers to the class it represented 
class(test[[1]]) #double bracket: this refers to the actual object

