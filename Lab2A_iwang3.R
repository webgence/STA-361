# Question 1
(TRUE & TRUE)
(TRUE | TRUE)

(TRUE & FALSE)
(TRUE | FALSE)

(TRUE & NA)
(TRUE | NA)

(FALSE & TRUE)
(FALSE | TRUE)

(FALSE & FALSE)
(FALSE | FALSE)

(FALSE & NA)
(FALSE | NA)

(NA & TRUE)
(NA | TRUE)

(NA & FALSE)
(NA | FALSE)

(NA & NA)
(NA | NA)


#2a
vec1 <- c(rep(1, 4))
vec2 <- c(rep(2, 4))
vec3 <- c(rep(3, 4))
m <- rbind(vec1, vec2, vec3)
str(m)
is.matrix(m)
m

#2b
subsetting <- m[3, 3]
#2c
subsetting2 <- m[,1]

#2d
subsetting3 <- m[2:3, 2:3]






