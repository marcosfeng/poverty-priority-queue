###########################
## Priority queuing model
###########################

library(simmer)
library(simmer.plot)
library(tidyverse)
library(latex2exp)
#library(dplyr)
#library(data.table)
set.seed(1234)

# Parameters
lambda <- 5
mu <- 5
t_simul <- 150 #Calibrated to optimize simulation time (prob_continue ≈ 0)
n_simul <- 100
scale = 100 # The scale on alpha to transfer runif(0,1) to something with more variance

weights <- seq(0, 1, by = 0.01)
arri <- list()
attr <- list()
total <- list()

# Simulations with simmer
for (w in 1:length(weights)) {
  env <- simmer("poverty")
  
  person <- trajectory("Poor's Trajectory") %>%
    set_attribute(keys = "urgency", function()
      runif(1, 0, 1)) %>%
    set_attribute(keys = "importance", function()
      runif(1, 0, 1)) %>%
    set_attribute(keys = "weight", weights[w]) %>%
    set_prioritization(function() {
      prio <-
        10000 * (
          get_attribute(env, "weight") * get_attribute(env, "urgency") +
            (1 - get_attribute(env, "weight")) * get_attribute(env, "importance")
        )
      c(prio, NA, NA)
    }) %>%
    # log_(function() {
    #   paste("Priority is: ", get_prioritization(env)[1])
    # }) %>%
    seize("person", amount = 1) %>%
    timeout(function()
      rexp(1, mu)) %>%
    release("person", amount = 1)
  
  env <-
    simmer() %>%
    add_resource("person", capacity = 1) %>%
    add_generator("Task", person, function()
      rexp(1, lambda), mon = 2)
  
  # env %>% run(until = t_simul)
  
  envs <- lapply(1:n_simul, function(i) {
    env %>%
      run(until = t_simul) %>%
      wrap()
  })
  
  ## Change Variables before
  arri[[w]] <- get_mon_arrivals(envs, ongoing = TRUE)
  attr[[w]] <- get_mon_attributes(envs)
  
  # Merge
  total[[w]] <-
    inner_join(arri[[w]], attr[[w]][attr[[w]]$key == "urgency", c(2, 4, 5)], by = c("name", "replication"))
  total[[w]] <-
    total[[w]] %>%
    rename(urgency = value)
  total[[w]] <-
    total[[w]][order(total[[w]]$replication, total[[w]]$start_time, decreasing = FALSE), ]
  row.names(total[[w]]) <- NULL
  total[[w]] <-
    inner_join(total[[w]], attr[[w]][attr[[w]]$key == "importance", c(2, 4, 5)], by = c("name", "replication"))
  total[[w]] <-
    total[[w]] %>%
    rename(importance = value)
  total[[w]] <-
    total[[w]][order(total[[w]]$replication, total[[w]]$start_time, decreasing = FALSE), ]
  row.names(total[[w]]) <- NULL
  total[[w]]$waiting_time <-
    total[[w]]$end_time - total[[w]]$start_time - total[[w]]$activity_time

    # Take care of precision problems yielding negative wait times.
  if (nrow(total[[w]][total[[w]]$waiting_time < 0 &
                      !is.na(total[[w]]$waiting_time), ]) > 0) {
    total[[w]][total[[w]]$waiting_time < 0 &
                 !is.na(total[[w]]$waiting_time), ]$waiting_time <- 0
  }

  #Weibull distribution with monomial function
  alpha = (scale - scale * as.numeric(total[[w]]$urgency)) #for now
  beta = 1
  total[[w]]$prob_fail <-
    1 - exp(-1 * (total[[w]]$waiting_time / alpha) ^ (beta))

  # Get probability of survival
  total[[w]] <- total[[w]] %>%
    mutate(prob_continue = (1 - prob_fail)) %>%
    group_by(replication) %>%
    mutate(prob_continue = lag(cumprod(prob_continue), k=1, default=1))

  cat('Simulation', w, 'of', length(weights), '\n')
}

sigma_list <- lapply(X = total, function(X)   sd(X[!is.na(X$waiting_time), ]$waiting_time))
mu_list    <- lapply(X = total, function(X) mean(X[!is.na(X$waiting_time), ]$waiting_time))

lag_time <- lapply(X = total, function(X) X %>%
                     filter(!is.na(waiting_time)) %>%
                     group_by(replication) %>%
                     mutate(waiting_time_lag = lag(waiting_time, default = 0)) %>%
                     dplyr::select(waiting_time, waiting_time_lag) %>%
                     cor())
memory   <- lapply(X = lag_time, function(X) X[2,3])


# Burstiness -------------------------------------------------------------
burstiness <- data.frame(weights) %>%
  mutate(sigma = unlist(sigma_list)) %>%
  mutate(mu = unlist(mu_list)) %>%
  mutate(B = (sigma - mu) / (sigma + mu)) %>%
  mutate(M = unlist(memory))
write_csv(burstiness, file = "urgency-burstiness-grid.csv")

# Probability of Survival -------------------------------------------------
prob_survival <- lapply(X = total,
                        function(X) X %>%
                          group_by(name) %>%
                          summarise(mean_survival = mean(prob_continue),
                                    mean_time = mean(end_time)) %>%
                          rename(task = name) %>%
                          mutate(task = readr::parse_number(task))
                          )
names(prob_survival) <- weights
survival <- prob_survival %>%
  bind_rows(.id = "weight")
write_csv(survival, file = "urgency-survival-grid.csv")

img1 <-
  data.frame(density(total[[9]][!is.na(total[[9]]$waiting_time), ]$waiting_time)$x)
img1$y <-
  density(total[[9]][!is.na(total[[9]]$waiting_time), ]$waiting_time)$y

ggplot(img1, aes(
  x = log(
    density.total..9....is.na.total..9...waiting_time.....waiting_time..x
  ) / log(10),
  y = log(y) / log(10)
)) + geom_point() +
  theme_bw() +
  labs(
    x = TeX('$log(\\tau)$'),
    y = TeX('$log(P_{\\tau} )$'),
    title = 'Log-log plot of time lag distribution'
  )


library(simmer.optim)


img1 <- data.frame(density(total9$waiting_time)$x)
img1$y <- density(total9$waiting_time)$y

library(latex2exp)
ggplot(img1, aes(
  x = log(density.total9.waiting_time..x) / log(10),
  y = log(y) / log(10)
)) + geom_point() +
  theme_bw() +
  labs(
    x = TeX('$log(\\tau)$'),
    y = TeX('$log(P_{\\tau} )$'),
    title = 'Log-log plot of time lag distribution'
  )

plot(df9, metric = "waiting_time")
plot(df9, metric = "activity_time")
