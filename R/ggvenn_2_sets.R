suppressMessages({
    rm(list = ls())
    if (!is.null(dev.list())) dev.off()
    
    if (!require(tidyverse)) install.packages("tidyverse")
    if (!require(ggvenn)) install.packages("ggvenn")
})

test_data <- readRDS("C:/jklai/project/Ath_heat/NextSeq2000/rcode/DEGs.rds")

dat <- list(
    set_A = test_data$SETS$set_A,
    set_B = test_data$SETS$set_B,
    set_C = test_data$SETS$set_C
)

ggvenn(dat) + theme_bw()

