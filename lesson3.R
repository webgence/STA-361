#Charcter Object
(myName <- "Ivan") 
str(myName)
is(myName)

cc <- "hello"

#Numerical Object
num_obj <- 7


pval <- .0000000007
options(scipen = 10)

5 > 1
10 > 20
4 = 4
4 == 'char' #Matching operator
'char' == "char"


#Atomic Structures

vec1 <- c(1, 2, 3)
vec2 <- c(4, 5, 6)
vec3 <- c(7, 8, 9)
square <- rbind(vec1, vec2, vec3)
str(square)
is.matrix(square)


vec2 <- c("a", "b", "c", "d")


(mat1 <- matrix(0, nrow = 3, ncol = 4))
(mat2 <- matrix(1:9, nrow=3, byrow=T))
rownames(mat2) <- c("r1", "r2", "r3")
colnames(mat2) <- c("c1", "c2", "c3")


## subsetting matrices
row1 <- mat2[1, 1:3]
str(row1)
is.vector(row1)


mat2[1,]


dim(mat2)
dim(vec1)

length(vec1)




