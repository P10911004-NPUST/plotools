heatmap_gradient <- function(
        n_breaks = 30,
        color_set = c("#1d3557", "#f1faee", "#e63946")
){
    colorRampPalette(color_set)(n_breaks)
}

discrete_4 <- list(
    d1 = c("#7ED7C1", "#F0DBAF", "#DC8686", "#B06161"),
    d2 = c("#E8C872", "#FFF3CF", "#C9D7DD", "#637A9F")
)

continuous_4 <- list(
    c("#DCF2F1", "#7FC7D9", "#365486", "#0F1035"),
    c("#B9EDDD", "#87CBB9", "#569DAA", "#577D86"),
    c("#1F2544", "#474F7A", "#81689D", "#FFD0EC"),
    c("#092635", "#1B4242", "#5C8374", "#9EC8B9")
)



show_palette <- function(palette){
    if (!is.null(dim(palette)) & !is.vector(palette))
        print("Input should be a vector")
    
    ncols <- length(palette)
    x.left <- 0:(ncols - 1) / ncols
    x.right <- 1:ncols / ncols
    y.bottom = rep(0.3, times = ncols)
    y.top = rep(0.7, times = ncols)
    
    par(mar = c(0, 0, 0, 0))
    plot(
        x = 0, 
        y = 0, 
        type = "n", 
        xlim = c(0, 1), 
        ylim = c(0, 1), 
        axes = FALSE, 
        xlab = "", 
        ylab = ""
    )
    
    rect(
        xleft = x.left,
        xright = x.right,
        ybottom = y.bottom,
        ytop = y.top,
        col = palette,
        border = "NA"
    )
    
    if (length(palette) < 7){
        text(
            x = (x.left + x.right) / 2,
            y = (y.bottom + y.top) / 2,
            labels = palette
        )
    }else if (length(palette) < 15){
        text(
            x = (x.left + x.right) / 2,
            y = (y.bottom + y.top) / 2,
            labels = palette,
            srt = 90
        )
    }
    
}



