##### Bayesian model prediction
## Marcos Gallo
library("rstan") # observe startup messages
library("tidyverse")

setwd(
  "/Users/marcosgallo/Documents/Marcos PhD Files/Poverty/2. Scarcity/3. Data Snapshots/"
)

# Data Wrangling ----------------------------------------------------------
load("3.2 Data/prolific1.Rdata")
data <- pilot1[!is.na(pilot1$participant), ]
data <- data %>%
  group_by(participant) %>%
  mutate(COUNTER = row_number())
Tsubj <- data %>%
  group_by(participant) %>%
  summarize(n = n())

# Creating Choice set ----------------------------------------------------------
choices <- c("HHU", "HH0", "HLU", "HL0", "LHU", "LH0", "LLU", "LL0")
ch <- expand.grid(choices, choices)
# Fix factor levels for next step
levels(ch$Var2) = append(choices, "TR")
for (chr in choices) {
  ch <- rbind(ch, c(chr, NA))
  ch <- rbind(ch, c(chr, "TR"))
}

# Creating State set ----------------------------------------------------------
S <- as.data.frame(t(combn(choices, 2)))
for (chr in choices) {
  # Add states with two of the same task
  S <- rbind(S, c(chr, chr))
  # Add states with only one task transition to one task
  S <- rbind(S, c(chr, NA))
  # Add states with only one task transitioning to two tasks
  S <- rbind(S, c(chr, "TR"))
}

# Create Transition Set ----------------------------------------------------------
# Transition Probabilities
choices <- paste(ch$Var1, ch$Var2, sep = "x")
states <- paste(S$V1, S$V2, sep = "x")
transitions <- expand.grid(choices, states)
# Remaining task (last 3 characters) is in the next state, but no NA
transitions$prob <- NA
## Non-urgent tasks:
## Note this is also transitioning to NA and TR states. Fix it in line 91 and __
transitions[str_detect(str = str_sub(transitions$Var1, -3), pattern = "0"), ]$prob <-
  str_detect(str = transitions[str_detect(str = str_sub(transitions$Var1, -3), pattern = "0"), ]$Var2,
             pattern = str_sub(transitions[str_detect(str = str_sub(transitions$Var1, -3), pattern = "0"), ]$Var1, -3))
## Urgent tasks:
### If HXU -> LXU
transitions[str_detect(str = str_sub(transitions$Var1, -1), pattern = "U") &
              str_detect(str = str_sub(transitions$Var1, start = -3, end = -3),
                         pattern = "H"), ]$prob <-
  str_detect(str = transitions[str_detect(str = str_sub(transitions$Var1, -1), pattern = "U") &
                                 str_detect(str = str_sub(transitions$Var1, start = -3, end = -3),
                                            pattern = "H"), ]$Var2,
             pattern = paste("L",
                             str_sub(
                               transitions[str_detect(str = str_sub(transitions$Var1, -1), pattern = "U") &
                                             str_detect(str = str_sub(transitions$Var1, start = -3, end = -3),
                                                        pattern = "H"), ]$Var1,
                               start = -2,
                               end = -2
                             ), "U", sep = ""))
### If LXU -> NA
## first change all transitions to false
transitions[str_detect(str = str_sub(transitions$Var2, -3), pattern = "NA"), ]$prob <-
  FALSE
# Now calculate other transitions
transitions[str_detect(str = str_sub(transitions$Var1, -1), pattern = "U") &
              str_detect(str = str_sub(transitions$Var1, start = -3, end = -3),
                         pattern = "L"), ]$prob <-
  str_detect(str = transitions[str_detect(str = str_sub(transitions$Var1, -1), pattern = "U") &
                                 str_detect(str = str_sub(transitions$Var1, start = -3, end = -3),
                                            pattern = "L"), ]$Var2,
             pattern = "NA")

## Single task with transition to single task:
transitions[str_detect(str = transitions$Var1, pattern = "NA"), ]$prob <-
  str_detect(str = transitions[str_detect(str = transitions$Var1, pattern = "NA"), ]$Var2,
             pattern = "TR")
## Single task with transition to single task:
transitions[str_detect(str = transitions$Var1, pattern = "TR"), ]$prob <-
  0

#Transforming them into probabilities by dividing by 8
transitions$prob <- as.numeric(transitions$prob)
transitions$prob <- transitions$prob / 8

## Single task with transition to single task:
transitions[str_detect(str = transitions$Var1, pattern = "TR") &
              str_length(transitions$Var2) == 7, ]$prob <- 1 / 36
# We need to change "transitions" for simplicity of search
transitions$choice <- substr(transitions$Var1, 1, 3)
transitions$non_choice <- substr(transitions$Var1, 5, 7)
transitions[transitions$non_choice == "NA", ]$non_choice <- NA
transitions$state2_1 <- substr(transitions$Var2, 1, 3)
transitions$state2_2 <- substr(transitions$Var2, 5, 7)
transitions[transitions$state2_2 == "NA", ]$state2_2 <- NA


# create choice[N,T] ----------------------------------------------------------
data$choice_bin <- 1
data[data$choice == "R", ]$choice_bin <- 2
choice <- data %>%
  select(participant, COUNTER, choice_bin) %>%
  group_by(COUNTER) %>%
  spread(COUNTER, choice_bin)
# Take NAs out
choice[is.na(choice)] <- 0


# Option-State ------------------------------------------------------------
## an easy way to map the choices for choice prob calculation
ch_opt_translation <- ch[, c(1:2)]
ch_opt_translation$index <- c(1:length(ch_opt_translation$Var1))
ch_opt_translation$i1 <- NA
ch_opt_translation$e1 <- NA
ch_opt_translation$i2 <- NA
ch_opt_translation$e2 <- NA

ch_opt_translation$u1 <- grepl("U", ch_opt_translation$Var1)
ch_opt_translation$u2 <- grepl("U", ch_opt_translation$Var2)
ch_opt_translation[is.na(ch_opt_translation$Var2),]$u2 <- NA
ch_opt_translation$u1 <- as.numeric(ch_opt_translation$u1) + 1
ch_opt_translation$u2 <- as.numeric(ch_opt_translation$u2) + 1

ch_opt_translation$i1 <-
  ifelse(substring(ch_opt_translation$Var1, 1, 1) == "H", 2, 1)
ch_opt_translation$i2 <-
  ifelse(substring(ch_opt_translation$Var2, 1, 1) == "H", 2, 1)
ch_opt_translation[is.na(ch_opt_translation$Var2),]$i2 <- NA

ch_opt_translation$e1 <-
  ifelse(substring(ch_opt_translation$Var1, 2, 2) == "H", 2, 1)
ch_opt_translation$e2 <-
  ifelse(substring(ch_opt_translation$Var2, 2, 2) == "H", 2, 1)
ch_opt_translation[is.na(ch_opt_translation$Var2),]$e2 <- NA

ch_opt_translation[grepl("TR", ch_opt_translation$Var2),]$i2 <-
  ch_opt_translation[grepl("TR", ch_opt_translation$Var2),]$i1
ch_opt_translation[grepl("TR", ch_opt_translation$Var2),]$e2 <-
  ch_opt_translation[grepl("TR", ch_opt_translation$Var2),]$e1
ch_opt_translation[grepl("TR", ch_opt_translation$Var2),]$u2 <-
  ch_opt_translation[grepl("TR", ch_opt_translation$Var2),]$u1

ch_opt_translation[grepl("TR", ch_opt_translation$Var2),]$i1 <- NA
ch_opt_translation[grepl("TR", ch_opt_translation$Var2),]$e1 <- NA
ch_opt_translation[grepl("TR", ch_opt_translation$Var2),]$u1 <- NA

data <- merge(
  x = data,
  y = ch_opt_translation[, -c(1:2)],
  by = c("u1", "u2", "e1", "e2", "i1", "i2")
)

opt_st <- data %>%
  select(participant, COUNTER, index) %>%
  group_by(COUNTER) %>%
  spread(COUNTER, index)
# Take NAs out
opt_st[is.na(opt_st)] <- 0

ch_opt_translation$value <- 0
ch_opt_translation[grepl("HH", ch_opt_translation$Var1), ]$value = 1
ch_opt_translation[grepl("LH", ch_opt_translation$Var1), ]$value = 2
ch_opt_translation[grepl("LL", ch_opt_translation$Var1), ]$value = 3
ch_opt_translation[grepl("HL", ch_opt_translation$Var1), ]$value = 4


S$st_index <- c(1:nrow(S))
S <- merge(
  x = S,
  y = ch_opt_translation[, c(1:3)],
  by.x = c("V1", "V2"),
  by.y = c("Var1", "Var2")
)
ch_opt_translation$index2 <- ch_opt_translation$index
S <- merge(
  x = S,
  y = ch_opt_translation[, c(1:2, 11)],
  by.x = c("V2", "V1"),
  by.y = c("Var1", "Var2"),
  all.x = TRUE
)
state_lookup <- S %>%
  select(st_index, index, index2)
# Take NAs out
state_lookup[is.na(state_lookup$index2), ]$index2 <-
  state_lookup[is.na(state_lookup$index2), ]$index
# Order state_lookup
state_lookup <- state_lookup %>%
  arrange(order_by = st_index)

prob_weight <- merge(
  x = transitions,
  y = ch_opt_translation[, c(1:3)],
  by.x = c("choice", "non_choice"),
  by.y = c("Var1", "Var2")
)
prob_weight <- merge(
  x = prob_weight,
  y = ch_opt_translation[, c(1:2, 11)],
  by.x = c("state2_1", "state2_2"),
  by.y = c("Var1", "Var2")
)


# Map Choices to States ---------------------------------------------------
choice_state <- as.data.frame(c(1:80))
choice_state <- merge(
  x = choice_state,
  y = state_lookup[, -c(3)],
  by.x = c("c(1:80)"),
  by.y = c("index"),
  all.x = TRUE
)
choice_state <- merge(
  x = choice_state,
  y = state_lookup[, -c(2)],
  by.x = c("c(1:80)"),
  by.y = c("index2"),
  all.x = TRUE
)
choice_state$st_index <- choice_state$st_index.x
choice_state[is.na(choice_state$st_index.x), ]$st_index <-
  choice_state[is.na(choice_state$st_index.x), ]$st_index.y
choice_state <- choice_state %>%
  select(`c(1:80)`, st_index)
prob_weight <- merge(x = prob_weight,
                     y = choice_state,
                     by.x = "index2",
                     by.y = "c(1:80)")
prob_weight <- prob_weight %>%
  select(index, st_index, prob)

prob_weight <- prob_weight %>%
  group_by(index) %>%
  spread(st_index, prob)


# Counterpart choice|states -----------------------------------------------
counterpart <-
  merge(
    x = choice_state,
    y = state_lookup[, -c(1)],
    by.x = "c(1:80)",
    by.y = "index",
    all.x = TRUE
  )
counterpart <-
  merge(
    x = counterpart,
    y = state_lookup[, -c(1)],
    by.x = "c(1:80)",
    by.y = "index2",
    all.x = TRUE
  )
counterpart$index_ALL <- counterpart$index
counterpart[is.na(counterpart$index), ]$index_ALL <-
  counterpart[is.na(counterpart$index), ]$index2
counterpart <- counterpart %>%
  select(index_ALL)

model_data <-
  list(
    N = length(unique(data$participant)),
    #number of part
    T = max(data$COUNTER),
    # number of max rounds
    nOpt = 2,
    Tsubj = Tsubj$n,
    # number of round
    choice = choice[, c(-1)],
    opt_st = opt_st[, c(-1)],
    value_lookup = ch_opt_translation$value,
    state_lookup = state_lookup[, c(-1)],
    prob_weight = prob_weight[, c(-1)],
    counterpart = counterpart$index_ALL
  ) # transition probabilities

save(model_data, ch_opt_translation, file = "3.2 Data/prolific1StanData.Rdata")

my_model <-
  stan_model(file = "3.1 Code/hier-bayes-simple.stan", verbose = TRUE)
Prolific1_Stan_results <-
  sampling(
    object = my_model,
    data = model_data,
    iter = 1000,
    chains = 1,
    cores = 3
  )

save(Prolific1_Stan_results, file = "3.2 Data/Prolific1StanResults.Rdata")

library("shinystan")
launch_shinystan(MTurk_sample_nolimit2)
