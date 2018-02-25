#' Plotting Unc-GFP lengths
#' n = 30-60

png(filename="taxol.png", width=6, height=3,units="in", res=600)

# import data
df <- read.csv('taxol.csv', header=TRUE)
df <- cbind(df[,5], df[,6], df[,3], df[,4], df[,1], df[,2])
df[df < 0.5] <- NA
    
addNoise <- function(mtx) {
    if (!is.matrix(mtx)) mtx <- matrix(mtx, byrow = TRUE, nrow = 1)
    random.stuff <- matrix(runif(prod(dim(mtx)), min = -0.0001, max = 0.01), nrow = dim(mtx)[1])
    random.stuff + mtx
}

df <- addNoise(df)

library(RColorBrewer)

cols <- brewer.pal(9,"Set1")[c(9,4)]

par(bty="n",
    mar=c(2, 4, 3, 0),  # plot margins b-l-t-r
    las=1,              # horizontal labels
    tcl=-.25,           # tick length
    font.main=1,        # plain font
    mgp=c(2.5, 0.5, 0),  # axis spacings
    cex.lab=1.6,
    cex.axis=1.4,
    ps=12
)


xat <- c(1,
         2,
         3, 4, 5, 6)
b <- boxplot(df,
             xaxt="n",       # no x-axis
             yaxt="n",
             boxwex=0.45,    # boxwidth
             lty=1,          # line type, 1 = solid
             staplewex=0,    # no staples
             log="",         # no log scaling
             ylim=c(0, 3),
             at=xat,
             border=cols,
             lwd=2,
             outline=FALSE
)          # line weight

stripchart(apply(df, 2, as.list),
           vertical=TRUE,
           method="jitter",
           add=TRUE,
           pch=20,
           cex=0.4,
           col=cols,
           at=xat,
           jitter=0.05)

title("Cep290 lengths\n (late G2 phase)", font.main=1, cex.main=1.6, line=0)
title(ylab=expression(paste("Length (", "μ", "m)", sep="")), line=2.2)

axis(2, at=seq(0, 3, 0.5), labels=TRUE, col.ticks=1, lwd=1)

temp.labels = c("Control", "SigD")
legend("topleft", cex=1.3, col=cols, lty=1, lwd=2, temp.labels, bty="n")


dev.off()

