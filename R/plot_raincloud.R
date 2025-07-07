suppressMessages({
    if (!require(ggplot2)) install.packages("ggplot2")
    if (!require(ggtext)) install.packages("ggtext")
    if (!require(ggsignif)) install.packages("ggsignif")
    if (!require(grid)) install.packages("grid")
    if (!require(common)) install.packages("common") 
    if (!require(signs)) install.packages("signs")
})

x <- runif(5)
ggplot2::resolution(x)

x1 <- outer(x, x, "-")
x1 <- min(abs(x1[upper.tri(x1)]))
x1
