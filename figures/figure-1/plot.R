png(filename="fig1.png", width=7, height=6,units="in", res=600)

library(RColorBrewer)
library(plotrix)

addNoise <- function(mtx) {
    if (!is.matrix(mtx)) {
        mtx <- matrix(mtx, byrow=TRUE, nrow=1)
    }
    random.stuff <- matrix(runif(prod(dim(mtx)), min=-0.0001, max=0.03), 
                           nrow=dim(mtx)[1])
    return(random.stuff + mtx)
}

df <- read.csv('fig1quantification.csv', header=TRUE)
df <- cbind(df[,2], df[,1], df[,4], df[,3], df[,6], df[,5], df[,8], df[,7])

df <- addNoise(df)

cols <- brewer.pal(9,"Set1")[c(9,4)]

par(bty="n",
    mar=c(3, 4, 3, 0),  # plot margins b-l-t-r
    las=1,              # horizontal labels
    tcl=-.25,           # tick length
    font.main=1,        # plain font
    mgp=c(2.5, 0.5, 0),  # axis spacings
    cex.lab=1.6,
    cex.axis=1.4,
    ps=12
)

xat <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)

b <- boxplot(df,
             xaxt="n",       # no x-axis
             yaxt="n",
             boxwex=0.45,    # boxwidth
             lty=1,          # line type, 1 = solid
             staplewex=0,    # no staples
             ylim=c(0.3, 1.7),
             at=xat,
             border=cols,
             lwd=2,
             outline=FALSE)

stripchart(apply(df, 2, as.list),
           vertical=TRUE,
           method="jitter",
           add=TRUE,
           pch=20,
           cex=0.4,
           col=cols,
           at=xat,
           jitter=0.05)

title("Ana1 and Cep290 lengths", font.main=1, cex.main=1.6, line=0.7)
title(ylab=expression(paste("Length (",italic("μ"), "m)", sep="")), line=2.2)

tlab=c(1.6, 3.7)+0.2

axis(2, at=c(0.3, 0.4, 0.5, 0.8, 1, 1.2, 1.4, 1.6),
     labels=TRUE, col.ticks=1, lwd=1.5, lwd.ticks=1.5)

temp.labels = c("Ana1-tdTomato", "Cep290-GFP")
legend("topleft", cex=1.3, col=cols, lty=1, lwd=2, temp.labels, bty="n")

offset <- 0.015

segments(xat[1], df[,1], xat[2], df[,2], lwd=0.05, col=cols[2])
segments(xat[3], df[,3], xat[4], df[,4], lwd=0.05, col=cols[2])
segments(xat[5], df[,5], xat[6], df[,6], lwd=0.05, col=cols[2])
segments(xat[7], df[,7], xat[8], df[,8], lwd=0.05, col=cols[2])

axis.break(axis=2, breakpos=0.6)

yat <- 0.6
lines(c(xat[1], xat[3]), c(yat, yat))
lines(c(xat[1], xat[1]), c(yat-offset, yat))
lines(c(xat[3], xat[3]), c(yat-offset, (yat)))
text((xat[1] + xat[3])/2, yat+3*offset, expression(italic('N.S.')), cex=1)

yat <- 0.7
lines(c(xat[2], xat[4]), c(yat, yat))
lines(c(xat[2], xat[2]), c(yat-offset, yat))
lines(c(xat[4], xat[4]), c(yat-offset, yat))
text((xat[2] + xat[4])/2, yat+3*offset, expression(italic('N.S.')), cex=1, xpd=NA)

yat <- 1.4
lines(c(xat[5], xat[7]), c(yat, yat))
lines(c(xat[5], xat[5]), c(yat-offset, yat))
lines(c(xat[7], xat[7]), c(yat-offset, (yat)))
text((xat[5] + xat[7])/2, yat+3*offset, expression(italic('N.S.')), cex=1)

dev.off()
