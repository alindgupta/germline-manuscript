library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

df <- read.csv('x.csv')
y <- df$cep290

out <- stan('model.stan',
            data=list(y = y,
                      N = length(y),
                      K = 3,
                      J = 2,
                      d_hyp = c(1, 1, 1),
                      var_hyp= 0.1),
            chains = 4,
            iter = 1000)

rstan::extract(out, 'y_pred')[[1]][1:length(y)] %>% hist
