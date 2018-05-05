library(magrittr)
addNoise <- function(mtx) {
  if (!is.matrix(mtx)) {
    mtx <- matrix(mtx, byrow=TRUE, nrow=1)
  }
  random.stuff <- matrix(runif(prod(dim(mtx)), min=-0.0001, max=0.01), 
                         nrow=dim(mtx)[1])
  return(random.stuff + mtx)
}

df <- read.csv('fig1quantification.csv', header=TRUE)
df <- cbind(df[,2], df[,1], df[,4], df[,3], df[,6], df[,5], df[,8], df[,7])

#png(filename="cor.png", width=7, height=2, units="in", res=600)

par(mfrow=c(1,2), mar=c(2,2,0,0)+.3, las=1, tcl=-.25, cex.lab=1.4,
    cex.axis=1)
plot(df[,7] %>% addNoise, 
     df[,8] %>% addNoise, 
     ylab="", 
     xlab="", 
     bty="O",
     pch=16, 
     cex=0.8,
     ylim=c(1.1, 1.7),
     xlim=c(1.1, 1.35))
abline(lm(df[,8] ~ df[,7]), col="red")


plot(df[,5] %>% addNoise, 
     df[,6] %>% addNoise, 
     bty="n",
     ylab="", 
     xlab="", 
     bty="O",
     pch=16, 
     cex=0.8,
     ylim=c(0.5, 1),
     xlim=c(1, 1.3))
abline(lm(df[,6] ~ df[,5]), col="red")

#dev.off()
