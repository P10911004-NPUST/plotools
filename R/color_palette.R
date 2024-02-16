if (!require(ggplot2)) install.packages("ggplot2")


show_palette <- function(x) {
    par(mai = c(0.2, max(strwidth(x, "inch") + 0.4, na.rm = TRUE), 0.2, 0.4))
    barplot(rep(1, length(x)), col=x, space = 0.1, axes=FALSE, 
            names.arg=rev(x), cex.names=0.8, horiz = FALSE, las=0.5)
    return(invisible(NULL))
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


show_palette(discrete_4$d1)


par(mar=c(0,0,0,0))
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")

# Parameters
line <- 31
col <- 21

# Rectangles
rect( rep((0:(col - 1)/col),line) ,  sort(rep((0:(line - 1)/line),col),decreasing=T) , rep((1:col/col),line) , sort(rep((1:line/line),col),decreasing=T),  
      border = "light gray" , col=colors()[seq(1,651)])

# Text
text( rep((0:(col - 1)/col),line)+0.02 ,  sort(rep((0:(line - 1)/line),col),decreasing=T)+0.01 , seq(1,651)  , cex=0.5)