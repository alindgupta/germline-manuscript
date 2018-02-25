#' Perform fully Bayesian linear regression
#' and plot Monte Carlo draws
#'

library(rstan)
library(shinystan)
library(magrittr)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

df <- read.csv("taxolglm.csv")

# Simple linear regression model
lm.a <- df$length ~ df$pip2 + df$taxol

# Linear regression model with interaction term
lm.b <- df$length ~ df$pip2 + df$taxol + df$taxol*df$pip2

#' Run Monte Carlo simulation
#' @param y response variable,
#'   which is Cep290 length in our case
#' @param X model matrix
#' 
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

# TODO plotting
