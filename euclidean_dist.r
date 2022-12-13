# ref: https://www.statology.org/euclidean-distance-in-r/
# define the euclidean formula

euclidean_dist <- function(x, y) {
    edist <- sqrt(sum(x - y)^2)
    return(edist)
    }

# example 1, the euclidean distance between two vectors

a <- c(2, 6, 7, 7, 5, 12, 3, 12, 9)
b <- c(1, 9, 0, 3, 17, 2, 4)

message("The Euclidean distance between a and b: ")
euclidean_dist(a, b)
