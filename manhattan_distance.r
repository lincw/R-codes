
# function to calculate Manhattan distance

manhattan_dist <- function(a, b) {
    dist <- abs(a - b)
    dist <- sum(dist)
    return(dist)
}

# example 1
# define two vectors

a <- c(1, 2, 3, 4)
b <- c(2, 2, 1, 10)

# calculate Manhattan distance between a and b vectors

message("Manhattan distance between a and b is: ", manhattan_dist(a, b))

# example 2
# calculate Manhattan distance in a matrix

c <- c(9, 9, 10, 1)
d <- c(1, 2, 3, 3)

# generate a matrix from a, b, c and d vectors

mat <- rbind(a, b, c, d)
mat

# calculate Manhattan distance between each vectors in the matrix

dist(mat, method = "manhattan")
