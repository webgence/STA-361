# for Question 1
col_1 <- c(rep(TRUE, 3), rep(FALSE, 3), rep(NA, 3))
col_2 <- c(rep(c(TRUE, FALSE, NA), 3))
co1_3 <- col_1 & col_2
col_4 <- col_1 | col_2

ddd <- data.frame(col_1, col_2, co1_3, col_4)
names(ddd) <- c("A", "B", "A and B", "A or B")
ddd

# for Question 2
obj_1 <- 1
obj_2 <- TRUE
obj_3 <- "true"

class(obj_1)
str(obj_1)

class(obj_2)
str(obj_2)

class(obj_3)
str(obj_3)


obj1_vec <- c(obj_1, obj_2)
obj1_vec #Numerical value has higher Coercion then Logical value
obj2_vec <- c(obj_2, obj_3)
obj2_vec #Character value has higher Coercion then Logical value
obj3_vec <- c(obj_3, obj_1)
obj3_vec #Character value has higher Coercion then numerical value


#Therefore, in order of highest coercion of data types: Character > Numerical > Logical
# for Question 3
drink_size <- c(2, 2, 1, 3, 2, 2, 3, 3, 1, 3)

drink_size <- factor(drink_size,
                     levels = c(1, 2, 3),
                     labels = c('small', 'medium', 'large'))

drink_size
