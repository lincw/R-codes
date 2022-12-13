# Uniform manifold approximation and projection (UMAP) in R
# ref: https://cran.r-project.org/web/packages/umap/vignettes/umap.html

# library
library(umap)

# custon plot function
plotIris <- function(x, labels,
    main="A UMAP visualization of the Iris dataset",
    colors=c("#ff7f00", "#e377c2", "#17becf"),
    pad=0.1, cex=0.6, pch=19, add=FALSE, legend.suffix="",
    cex.main=1, cex.legend=0.85) {

    layout <- x
    if (is(x, "umap")) {
        layout <- x$layout
    } 

    xylim <- range(layout)
    xylim <- xylim + ((xylim[2]-xylim[1])*pad)*c(-0.5, 0.5)
    if (!add) {
        par(mar=c(0.2,0.7,1.2,0.7), ps=10)
        plot(xylim, xylim, type="n", axes=F, frame=F)
        rect(xylim[1], xylim[1], xylim[2], xylim[2], border="#aaaaaa", lwd=0.25)  
    }
    points(layout[,1], layout[,2], col=colors[as.integer(labels)],
            cex=cex, pch=pch)
    mtext(side=3, main, cex=cex.main)

    labels.u <- unique(labels)
    legend.pos <- "topleft"
    legend.text <- as.character(labels.u)
    if (add) {
        legend.pos <- "bottomleft"
        legend.text <- paste(as.character(labels.u), legend.suffix)
    }

    legend(legend.pos, legend=legend.text, inset=0.03,
            col=colors[as.integer(labels.u)],
            bty="n", pch=pch, cex=cex.legend)
}

# separate IRIS columns into data and label
iris_data <- iris[, grep("Sepal|Petal", colnames(iris))]
iris_labels <- iris[, "Species"]

# apply the UMAP transformation
iris_umap <- umap(iris_data)

# Visualize the UMAP object
plotIris(iris_umap, iris_labels)

# projecting new data
# add some noise to the original IRIS
iris_wnoise <- iris_data + matrix(rnorm(150 * 40, 0, 0.1), ncol = 4)
colnames(iris_wnoise) <- colnames(iris_data)

# arrange the perturbed observations onto the same layout as IRIS
iris_wnoise_umap <- predict(iris_umap, iris_wnoise)

# Visualize the perturbed observations with the original
plotIris(iris_umap, iris_labels)
plotIris(iris_wnoise_umap, iris_labels, add = T, pch = 4, legend.suffix = " (with noise)")