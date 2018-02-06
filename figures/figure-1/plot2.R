#png(filename="germlate.png", width=6.5, height=6,units="in", res=600)

# import data
df <- read.csv('fig1quantification.csv', header=TRUE)
df <- cbind(df[,2], df[,1], df[,4], df[,3])

addNoise <- function(mtx) {
    if (!is.matrix(mtx)) mtx <- matrix(mtx, byrow = TRUE, nrow = 1)
    random.stuff <- matrix(runif(prod(dim(mtx)), min = -0.001, max = 0.05), nrow = dim(mtx)[1])
    random.stuff + mtx
}

df <- addNoise(df)

library(RColorBrewer)

cols <- brewer.pal(9,"Set1")[c(1,2)]

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
         4)
b <- boxplot(df,
             xaxt="n",       # no x-axis
             yaxt="n",
             boxwex=0.45,    # boxwidth
             lty=1,          # line type, 1 = solid
             staplewex=0,    # no staples
             log="",         # no log scaling
             ylim=c(0.3, 0.6),
             at=xat,
             border=cols,
             lwd=2,
             outline=FALSE)          # line weight

stripchart(apply(df, 2, as.list),
           vertical=TRUE,
           method="jitter",
           add=TRUE,
           pch=20,
           cex=0.4,
           col=cols,
           at=xat,
           jitter=0.05)

title("Cep290 and Ana1 lengths (early G2 phase)", font.main=1, cex.main=1.3, line=0.7)
title(ylab=expression(paste("Length (",italic("μ"), "m)", sep="")), line=2.2)

tlab=c(1.6, 3.7)+0.3

axis(2, at=seq(0.3, 0.6, 0.1), labels=TRUE, col.ticks=1, lwd=1, lwd.ticks=1)

lab=c('Control', expression(italic('β2t')~'::'~'SigD'))
text(x=tlab, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]-0.3),
     labels=lab, srt=0, adj=1, xpd=TRUE, cex=1)

temp.labels = c("Ana1-tdTomato", "Cep290-GFP")
legend("topleft", cex=0.9, col=cols, lty=1, lwd=2, temp.labels, bty="n")

offset <- 0.015
yat <- 1.5
segments(xat[1], df[,1], xat[2], df[,2], lwd=0.1)
segments(xat[3], df[,3], xat[4], df[,4], lwd=0.1)

lines(c(xat[1]-0.25, xat[2]+0.25), c(0.3, 0.3), xpd=TRUE)
lines(c(xat[3]-0.25, xat[4]+0.25), c(0.3, 0.3), xpd=TRUE)


#dev.off()

