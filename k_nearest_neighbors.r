# simple explaination
# https://towardsdatascience.com/k-nearest-neighbors-algorithm-with-examples-in-r-simply-explained-knn-1f2c88da405c
# diagram ref: https://cambridgecoding.files.wordpress.com/2016/01/knn2.jpg
# ML in R: https://ourcodingclub.github.io/tutorials/machine-learning/

# library
library(class)

# custom function, normalize data into range 0 and 1
norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
}

# generate a random number that is 90% of the total number of rows in IRIS
rand <- sample(1:nrow(iris), 0.9 * nrow(iris))

# normalize the first 4 columns of IRIS
iris_norm <- as.data.frame(lapply(iris[, c(1:4)], norm))

# extract training and testing set
iris_train <- iris_norm[rand, ]
iris_test <- iris_norm[-rand, ]

# extract 5th column from IRIS as cluster, which will be used in `knn` function
iris_target_cl <- iris[rand, 5]
iris_test_cl <- iris[-rand, 5]

# run `knn` function
pr <- knn(iris_train, iris_test, cl = iris_target_cl, k = 13)

# create confusion matrix
iris_tab <- table(pr, iris_test_cl)

# this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(iris_tab)
