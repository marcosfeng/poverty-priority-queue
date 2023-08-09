library(rhdf5)
library(cmdstanr)

data_list <- list()
# Read each dataset and store it in the list
data_list$K <- h5read(file = "./Models/dyn_prog/data.h5", name = "K")
data_list$N <- h5read(file = "./Models/dyn_prog/data.h5", name = "N")
data_list$P <- h5read(file = "./Models/dyn_prog/data.h5", name = "P")
data_list$T <- h5read(file = "./Models/dyn_prog/data.h5", name = "T")
data_list$choice <- t(h5read(file = "./Models/dyn_prog/data.h5", name = "choice"))
data_list$effort <- t(h5read(file = "./Models/dyn_prog/data.h5", name = "effort"))
data_list$participants <- h5read(file = "./Models/dyn_prog/data.h5", name = "participants")
data_list$reward <- t(h5read(file = "./Models/dyn_prog/data.h5", name = "reward"))
data_list$state <- t(h5read(file = "./Models/dyn_prog/data.h5", name = "state"))
data_list$transition_probs <- h5read(file = "./Models/dyn_prog/data.h5", name = "transition_probs")
data_list$transition_probs <- aperm(data_list$transition_probs, c(3, 1, 2))

model <- cmdstan_model("Models/dyn_prog/exp1.stan")

fit <- model$sample(
  data = data_list,
  seed = 123,
  chains = 1,
  parallel_chains = 1,
  iter_warmup = 100,
  iter_sampling = 100
)
samples <- fit$draws()
fit$summary()

