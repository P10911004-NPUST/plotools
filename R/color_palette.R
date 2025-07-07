rgb_to_hexadecimal <- function(R, G, B, alpha = NULL)
{
    R <- R / 255
    G <- G / 255
    B <- B / 255
    ret <- grDevices::rgb(R, G, B, alpha)
    return(ret)
}

heatmap_gradient <- function(
        n_breaks = 30,
        color_set = c("#1d3557", "#f1faee", "#e63946")
){
    colorRampPalette(color_set)(n_breaks)
}

discrete_3 <- list(
    type01 = c("#FF8C00", "#A034F0", "#159090")
)

discrete_4 <- list(
    type01 = c("black", "#FF8C00", "#A034F0", "#159090"),
    type02 = c("#7ED7C1", "#F0DBAF", "#DC8686", "#B06161"),
    type03 = c("#E8C872", "#FFF3CF", "#C9D7DD", "#637A9F")
)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Color palettes for color blindness ====
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
color_blindness_IBM <- c("#648FFF", "#785EF0", "#DC267F", "#FE6100", "#FFB000") # IBM Design Library
color_blindness_Wong <- c(
    # Wong, B. Points of view: Color blindness. Nat Methods 8, 441 (2011). 
    # https://doi.org/10.1038/nmeth.1618
    "Black"           = "#000000", # rgb_to_hexadecimal(  0,   0,   0)
    "Orange"          = "#E69F00", # rgb_to_hexadecimal(230, 159,   0)
    "Sky-blue"        = "#56B4E9", # rgb_to_hexadecimal( 86, 180, 233)
    "Bluish-green"    = "#009E73", # rgb_to_hexadecimal(  0, 158, 115)
    "Yellow"          = "#F0E442", # rgb_to_hexadecimal(240, 228,  66)
    "Blue"            = "#0072B2", # rgb_to_hexadecimal(  0, 114, 178)
    "Vermillion"      = "#D55E00", # rgb_to_hexadecimal(213,  94,   0)
    "#Reddish-purple" = "#CC79A7"  # rgb_to_hexadecimal(204, 121, 167)
)
color_blindness_Tol <- c("#332288", "#117733", "#44AA99", "#88CCEE", "#DDCC77", "#CC6677", "#AA4499", "#882255") # Paul Tol



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



