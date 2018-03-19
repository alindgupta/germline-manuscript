library(rstan)
library(dplyr)
library(magrittr)

set.seed(1)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
iters <- 2000

df <- read.csv("quantification.csv")

func <- function(x, col.name, sigd.flag, mature.flag) {
  #' This function prepares the data frame read from 'quantification.csv'
  #' to be converted into long format.
  temp <- select(x, matches(col.name))
  colnames(temp) <- c("cep290", "ana1")
  temp <- mutate(temp, sigd=sigd.flag, mature=mature.flag)
  return(temp[complete.cases(temp),])
}

df.long <- do.call(rbind, 
                   list(func(df, "wtcepearly|wtanaearly", 0, 0),
                        func(df, "r19cepearly|r19anaearly", 1, 0),
                        func(df, "wtceplate|wtanalate", 0, 1),
                        func(df, "r19ceplate|r19analate", 1, 1)))

y <- df.long$cep290
stan.out <- stan('mixture.stan',
                 data=list(y = y,
                           N = length(y),
                           K = 3,
                           J = 2,
                           htheta = c(1, 1, 1),
                           hvar = 0.1),
                 chains = 4,
                 iter = iters)

mu.pred <- rstan::extract(stan.out, "mu_pred")[[1]]
mask <- rmultinom(iters, 1, rstan::extract(stan.out, "theta")[[1]])

y.pred <- rep(0, iters)
for (i in 1:iters) {
  y.pred[i] <- sum(mask[,i] * mu.pred[i,])
}

par(mfrow=c(2,1))
hist(y)
hist(y.pred)
