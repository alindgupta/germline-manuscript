#' Plotting Unc-GFP lengths
#' n = 30-60

png(filename="unc.png", width=3.5, height=6,units="in", res=600)

# import data
df <- read.csv('unc.csv', header=TRUE)
df <- cbind(df[,1], df[,3], df[,2])
    
addNoise <- function(mtx) {
    if (!is.matrix(mtx)) mtx <- matrix(mtx, byrow = TRUE, nrow = 1)
    random.stuff <- matrix(runif(prod(dim(mtx)), min = -0.0001, max = 0.01), nrow = dim(mtx)[1])
    random.stuff + mtx
}

df <- addNoise(df)

library(RColorBrewer)

cols <- brewer.pal(9,"Set1")[c(9,2,4)]

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
         3)
b <- boxplot(df,
             xaxt="n",       # no x-axis
             yaxt="n",
             boxwex=0.45,    # boxwidth
             lty=1,          # line type, 1 = solid
             staplewex=0,    # no staples
             log="",         # no log scaling
             ylim=c(1, 4),
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

title("Unc lengths\n (late G2 phase)", font.main=1, cex.main=1.6, line=0)
title(ylab=expression(paste("Length (", "μ", "m)", sep="")), line=2.2)

axis(2, at=seq(1, 4, 0.5), labels=TRUE, col.ticks=1, lwd=1)

temp.labels = c("Control", expression(italic("sktl")^{2.3}), "SigD")
legend("topleft", cex=1.3, col=cols, lty=1, lwd=2, temp.labels, bty="n")

offset <- 0.015
yat <- 0.9
lines(c(xat[4], xat[5]), c(yat, yat))
lines(c(xat[4], xat[4]), c(yat+offset, yat))
lines(c(xat[5], xat[5]), c(yat+offset, (yat)))
text((xat[4] + xat[5])/2, yat-3*offset, expression(italic('N.S.')), cex=1)

offset <- 0.05
yat <- 1.5
lines(c(xat[1], xat[2]), c(yat, yat))
lines(c(xat[1], xat[1]), c(yat+offset, yat))
lines(c(xat[2], xat[2]), c(yat+offset, yat))
text((xat[1] + xat[2])/2, yat-2*offset, expression(italic('p')<0.01), cex=1)

dev.off()

