#' Perform fully Bayesian linear regression of
#' the effect of Taxol and PIP2 levels on Cep290 length
#' and plot Monte Carlo draws

library(rstan)
library(shinystan)
library(magrittr)
library(RColorBrewer)

set.seed(42)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

df <- read.csv("taxolglm.csv")

# Simple linear regression model
lm.a <- df$length ~ df$pip2 + df$taxol

# Linear regression model with interaction term
lm.b <- df$length ~ df$pip2 + df$taxol + df$taxol*df$pip2

#' Run Monte Carlo simulation
#' @param y response variable, which is Cep290 length in our case
#' @param X model matrix
runStan <- function(y, X) {
  stan("bayeslm.stan",
       data = list(
         y = y,
         X = X,
         J = nrow(X),
         K = ncol(X)),
       chains = 4,
       iter = 2000)
}

fit.a <- runStan(df$length, lm.a %>% model.matrix)
fit.b <- runStan(df$length, lm.b %>% model.matrix)

add.alpha <- function(col, alpha=1) {
  if (missing(col)) { stop("Please provide a vector of colours.") }
  apply(sapply(col, col2rgb)/255, 2, function(x)
    rgb(x[1], x[2], x[3], alpha=alpha))
}

plotMC <- function(stan.out, str, breaks) {
  coefs <- rstan::extract(stan.out, str)[[1]]
  for (i in 1:ncol(coefs)) {
    hist(coefs[,i],
         breaks=breaks[i],
         xlim=c(min(coefs), max(coefs)),
         col=add.alpha(cols[i], 0.9),
         lty="blank",
         add=TRUE)
 }
}

cols <- brewer.pal(9, "Set1")

par(bty="n",
    mar=c(2, 5, 2, 2),
    las=1,
    tcl=-.25,
    font.main=1,
    mgp=c(2.5, 0.5, 0),
    mfrow=c(2, 1))

plot(1,
     type="n",
     ylim=c(0, 1000),
     xlim=c(-1, 2),
     ylab="Frequency",
     main="Length = b0 + PIP2 + Taxol")

plotMC(fit.a, "coeff", rep(30, 3))
legend("topleft",
       c("Intercept", "PIP2", "Taxol"),
       col=cols,
       lty=1, 
       lwd=2,
       cex=0.9,
       bty="n")
plot(1,
     type="n",
     ylim=c(0, 500),
     xlim=c(-1, 2),
     ylab="Frequency",
     main="Length = b0 + PIP2 + Taxol + PIP2*Taxol")

plotMC(fit.b, "coeff", rep(30, 4))

legend("topleft",
       c("Intercept", "PIP2", "Taxol", "PIP2*Taxol"),
       col=cols,
       lty=1, 
       lwd=2,
       cex=0.9,
       bty="n")

