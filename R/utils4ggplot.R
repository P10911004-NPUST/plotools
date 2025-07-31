suppressMessages({
    if (!require(ggplot2)) install.packages("ggplot2")
    if (!require(ggtext)) install.packages("ggtext")
    if (!require(ggsignif)) install.packages("ggsignif")
    if (!require(grid)) install.packages("grid")
    if (!require(common)) install.packages("common") 
    if (!require(signs)) install.packages("signs") 
    windowsFonts(
        palatino = windowsFont("Palatino Linotype"),
        times = windowsFont("Times New Roman"),
        arial = windowsFont("Arial")
    )
})


# Self-defined function ====
asterisk <- function(pvalue){
    if (pvalue <= 0.001) return("\U002A\U002A\U002A")
    if (pvalue <= 0.01 & pvalue > 0.001) return("\U002A\U002A")
    if (pvalue <= 0.05 & pvalue > 0.01) return("\U002A")
    if (pvalue > 0.05) return(common::supsc("ns"))
}

num2asterisk <- function(
        x, 
        cutpoints   = c(0, 0.001, 0.01, 0.05, 1),
        symbols     = c("\U002A\U002A\U002A", "\U002A\U002A", "\U002A", "ns"),
        superscript = FALSE
){
    symbols <- as.character(symbols)
    # if (superscript) symbols <- c("***", "**", "*", common::supsc("ns"))
    if (superscript) symbols <- sapply(symbols, common::supsc)
    symnum(
        x,
        cutpoints = cutpoints,
        symbols = symbols
    )
}




# Customized geom function ====
geom_point <- function(
        size = 2,
        alpha = 0.5,
        show.legend = TRUE,
        position = position_jitter(width = 0.1),
        ...
){
    ggplot2::geom_point(
        position = position,
        size = size,
        alpha = alpha,
        show.legend = show.legend,
        ...
    )
}


geom_boxplot <- function(
        fill = "transparent",
        size = 1, 
        linewidth = 0.5,
        outliers = FALSE,
        outlier.colour = "transparent",
        outlier.size = 0,
        outlier.shape = NA,
        show.legend = TRUE,
        ...
){
    ggplot2::geom_boxplot(
        fill = fill,
        size = size, 
        linewidth = linewidth,
        outliers = outliers,
        outlier.colour = outlier.colour,
        outlier.size = outlier.size,
        outlier.shape = outlier.shape,
        show.legend = show.legend,
        ...
    )
}


geom_bar <- function(
        stat = "summary", 
        fun = "mean", 
        position = "dodge", 
        color = "transparent", 
        alpha = 0.5,
        ...
){
    geom_bar(
        stat = stat, 
        fun = fun, 
        position = position, 
        color = color, 
        alpha = alpha,
        ...
    )
}


geom_errorbar <- function(
        data,
        mapping,
        inherit.aes = FALSE,
        show.legend = FALSE,
        width = 0.3,
        linewidth = 0.6 
){
    geom_errorbar(
        data = data,
        mapping = mapping,
        inherit.aes = inherit.aes,
        show.legend = show.legend,
        width = width,
        linewidth = linewidth
    )
}


geom_signif <- function(
        family = "arial",
        size = 0.5,
        textsize = 8,
        color = "black",
        show.legend = FALSE,
        comparisons = list(
            c(),
            c()
        ),
        test = c("t.test", "wilcox.test"),
        test.args = list(
            alternative = "two.sided",  # c("two.sided", "less", "greater") 
            var.equal = FALSE, 
            paired = FALSE,
            exact = FALSE,
            mu = 0,
            conf.level = 0.95
        ),
        map_signif_level = x -> num2asterisk(x),
        # map_signif_level = function(x){
        #     if (x <= 0.001) return("\U002A\U002A\U002A")
        #     if (x <= 0.01 & x > 0.001) return("\U002A\U002A")
        #     if (x <= 0.05 & x > 0.01) return("\U002A")
        #     if (x > 0.05) return(common::supsc("ns"))
        # },
        ...
){
    ggsignif::geom_signif(
        family = family,
        size = size,
        textsize = textsize,
        color = color,
        show.legend = show.legend,
        comparisons = comparisons,
        test = match.arg(test),
        test.args = test.args,
        map_signif_level = map_signif_level,
        ...
    )
}


ggsave2 <- function(
        plot = ggplot2::last_plot(),
        filename,
        path = "./",
        device = "jpeg",
        dpi = 660,
        units = "cm",
        height = 12,
        width = 17,
        ...
){
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    
    ggplot2::ggsave(
        plot = plot,
        filename = filename,
        path = path,
        device = device,
        dpi = dpi,
        units = units,
        height = height,
        width = width,
        ...
    ) 
}


hline_grob <- function(xmin, xmax, y, linewidth = 1.5){
    ggplot2::annotation_custom(
        grob = grid::linesGrob(gp = grid::gpar(lwd = linewidth)),
        xmin = xmin, 
        xmax = xmax, 
        ymin = y, 
        ymax = y
    )
}

vline_grob <- function(x, ymin, ymax, linewidth = 1.5){
    ggplot2::annotation_custom(
        grob = grid::linesGrob(gp = grid::gpar(lwd = linewidth)),
        xmin = x, 
        xmax = x, 
        ymin = ymin, 
        ymax = ymax
    )
}


# Theme ====
theme_bw_01 <- theme_bw() +
    theme(
        text = element_text(family = "arial", face = "bold", size = 20),
        
        plot.title = element_markdown(family = "arial", face = "bold", size = 20),
        plot.subtitle = element_markdown(family = "arial", face = "bold", size = 20),
        
        legend.position = "top",
        legend.position.inside = c(0.5, 0.95),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.title = element_markdown(
            family = "arial", 
            face = "bold", 
            size = 20, 
            margin = ggplot2::margin(r = 7, l = 7)
        ),
        legend.text = element_markdown(
            family = "arial", 
            face = "plain", 
            size = 20, 
            margin = ggplot2::margin(r = 7, l = 7)
        ),
        legend.key.size = grid::unit(x = 1, units = "lines"),
        legend.key.spacing.x = grid::unit(x = 0.7, units = "lines"),
        legend.key.spacing.y = grid::unit(x = 0.5, units = "lines"),
        
        axis.title.x = element_markdown(margin = ggplot2::margin(t = 7)),
        axis.line.x = element_line(linewidth = 0.6, lineend = "round"),
        
        axis.title.y = element_markdown(margin = ggplot2::margin(r = 9)),
        axis.line.y = element_line(linewidth = 0.6, lineend = "round")
    )


theme_bw_02 <- theme_bw() +
    theme(
        text = element_text(family = "arial", face = "bold", size = 20),
        
        legend.position = "top",
        legend.position.inside = c(0.5, 0.95),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.title = element_markdown(
            family = "arial", 
            face = "bold", 
            size = 20, 
            margin = ggplot2::margin(r = 7, l = 7)
        ),
        legend.text = element_markdown(
            family = "arial", 
            face = "plain", 
            size = 20, 
            margin = ggplot2::margin(r = 7, l = 7)
        ),
        legend.key.size = grid::unit(x = 1, units = "lines"),
        legend.key.spacing.x = grid::unit(x = 0.7, units = "lines"),
        legend.key.spacing.y = grid::unit(x = 0.5, units = "lines"),
        
        axis.title.x = element_markdown(margin = ggplot2::margin(t = 7)),
        
        axis.title.y = element_markdown(margin = ggplot2::margin(r = 9))
    )

