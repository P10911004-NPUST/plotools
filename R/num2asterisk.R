if ( ! require(common) ) install.packages("common")

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

