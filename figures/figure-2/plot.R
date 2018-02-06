#png(filename="cepgerm.png", width=6.5, height=6,units="in", res=600)

# import data
df <- read.csv('quantification.csv', header=TRUE)
df <- cbind(df[,2], df[,4], df[,6], df[,1], df[,3], df[,5])

addNoise <- function(mtx) {
    if (!is.matrix(mtx)) mtx <- matrix(mtx, byrow = TRUE, nrow = 1)
    random.stuff <- matrix(runif(prod(dim(mtx)), min = -0.0001, max = 0.01), nrow = dim(mtx)[1])
    random.stuff + mtx
}

df <- addNoise(df)

library(RColorBrewer)

cols <- brewer.pal(9,"Set1")[c(1,2,3)]

par(bty="n",
    mar=c(3, 4, 3, 3),  # plot margins b-l-t-r
    las=1,              # horizontal labels
    tcl=-.25,           # tick length
    font.main=1,        # plain font
    mgp=c(2.5, 0.5, 0),  # axis spacings
    cex.lab=1.1,
    cex.axis=1.1,
    ps=12
)


xat <- c(1,
         2,
         3,
         4.5,
         5.5,
         6.5)
b <- boxplot(df,
             xaxt="n",       # no x-axis
             yaxt="n",
             boxwex=0.45,    # boxwidth
             lty=1,          # line type, 1 = solid
             staplewex=0,    # no staples
             log="",         # no log scaling
             ylim=c(0.5, 1.7),
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

title("Cep290 and Ana1 lengths\n (late G2 phase)", font.main=1, cex.main=1.3, line=0.2)
title(ylab=expression(paste("Length (",italic("μ"), "m)", sep="")), line=2.2)

axis(2, at=seq(0.6, 2, 0.2), labels=TRUE, col.ticks=1, lwd=1)
axis(1, labels=c("Cep290-GFP", "Ana1-tdTomato"), at=c(2, 0.92*(xat[5]+xat[6])/2), lwd.ticks=0, lty=0, line=-0.8)
axis(1, at=c(0, ((xat[3] + xat[4])/2)), labels=FALSE, tck=-0.05, col=0, lty=0)
#box(bty='L')

temp.labels = c("Control", "SigD", "SigD + Sktl")
legend("topright", cex=0.9, col=cols, lty=1, lwd=2, temp.labels, bty="n")

offset <- 0.015
yat <- 0.9
lines(c(xat[4], xat[5]), c(yat, yat))
lines(c(xat[4], xat[4]), c(yat+offset, yat))
lines(c(xat[5], xat[5]), c(yat+offset, (yat)))
text((xat[4] + xat[5])/2, yat-2*offset, expression(italic('N.S.')), cex=0.8)

offset <- 0.015
yat <- 0.8
lines(c(xat[4], xat[6]), c(yat, yat))
lines(c(xat[4], xat[4]), c(yat+offset, yat))
lines(c(xat[6], xat[6]), c(yat+offset, yat))
text((xat[4] + xat[6])/2, yat-2*offset, expression(italic('N.S.')), cex=0.8)

lines(xat[1:2], b$stats[3,1:2], lwd=0.7)
lines(xat[2:3], b$stats[3,2:3], lwd=0.7)
lines(xat[4:5], b$stats[3,4:5], lwd=0.7)
lines(xat[5:6], b$stats[3,5:6], lwd=0.7)

lines(c(xat[1], xat[3]), c(0.5,0.5))
lines(c(xat[4], xat[6]), c(0.5,0.5))

#dev.off()

