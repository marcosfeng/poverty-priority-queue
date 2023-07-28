library(cmdstanr)
library(dplyr)

# Prepare data
data_list <- df %>%
  arrange(participant, trial) %>%
  group_by(participant) %>%
  summarise(
    a = list(action),
    r = list(reward)
  ) %>%
  ungroup() %>%
  summarise(
    N = n(),
    K = nlevels(df$action),
    P = nlevels(df$participant),
    a = I(list(a)),
    r = I(list(r))
  )

# Compile the model
model <- cmdstan_model("./RL/model_free.stan")
# Fit the model
fit_mf <- model$sample(
  data = data_list,
  iter_warmup = 1000,
  iter_sampling = 1000,
  chains = 4
)

# Compile the model
model <- cmdstan_model("./RL/model_based.stan")
# Fit the model
fit_mf <- model$sample(
  data = data_list,
  iter_warmup = 1000,
  iter_sampling = 1000,
  chains = 4
)
