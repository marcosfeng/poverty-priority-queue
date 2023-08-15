library(rhdf5)
library(cmdstanr)

# © 1998-2023 RANDOM.ORG
# 
# 887468224	
# Timestamp: 2023-08-11 02:07:26 UTC
set.seed(887468224)

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
data_list$grainsize <- round(data_list$N/4)
data_list$slices_n <- seq_len(data_list$N)


model <- cmdstan_model(compile = FALSE,
                       stan_file = "Models/dyn_prog/exp1-hierarchical.stan")
model$compile(cpp_options = list(stan_opencl = TRUE,
                                 OPENCL_DEVICE_ID = 0,
                                 OPENCL_PLATFORM_ID = 1),
              quiet = FALSE, force_recompile = TRUE)

fit <- model$sample(
  data = data_list,
  chains = 1,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500
)

# fit$save_object(file = "./Models/dyn_prog/hierarchical-results.RDS")

samples <- fit$draws()
fit$summary()
