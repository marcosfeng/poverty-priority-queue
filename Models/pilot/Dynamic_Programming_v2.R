############################################################
## Marcos Gallo
## Dynamic Programming Ice Cream Experiment
## With changes from first pilot lessons
#############################################################

# library(dynprog)
# library(stringi)
# library(stringr)
library(stats)
library(dplyr)

tasks <- list()
totaltime = 1000
enqueuerate = 5
completerate = 5
urgencyshape = 5
urgencyscale = 1

tao = 0
while (tao < totaltime) {
  if (tao == 0) {
    task_list <- list(0, rweibull(1, urgencyshape, urgencyscale))
    tasks <- append(tasks, task_list)
    task_list <- list(0, rexp(1, completerate), rweibull(1, urgencyshape, urgencyscale))
    tasks <- append(tasks, task_list)
  }
  
}




#Noise parameter
eps <- 0.5
#Importance (means)
imp <- c(1, 2)
#Cost
cost1 = 0
cost2_grid <- seq(from = 0.1, to = 0.9, by = 0.05)
#Discounting
gamma_grid <- seq(from = 0.8, to = 1, by = 0.025)
#Choices
tasks <- c("HHU",
           "HH0",
           "HLU",
           "HL0",
           "LHU",
           "LH0",
           "LLU",
           "LL0")
tasks <- as.data.frame(tasks)
# Urgency Ratios
ratios <- c()

for (gamma in gamma_grid) {
  print(gamma)
  for (cost2 in cost2_grid) {
    # Add values
    tasks$value  <- c(
      imp[2] - cost2,
      imp[2] - cost2,
      imp[2] - cost1,
      imp[2] - cost1,
      imp[1] - cost2,
      imp[1] - cost2,
      imp[1] - cost1,
      imp[1] - cost1
    )
    #Look up values
    r_choice <- tasks$value
    names(r_choice) <- tasks$tasks
    ## Use unname(r_choice["HH0"]) for example
    
    # States
    S <- as.data.frame(t(combn(tasks$tasks, 2)))
    for (chr in tasks$tasks) {
      # Add states with two of the same task
      S <- rbind(S, c(chr, chr))
      # Add states with only one task transition to one task
      S <- rbind(S, c(chr, NA))
      # Add states with only one task transitioning to two tasks
      S <- rbind(S, c(chr, "TR"))
    }
    
    #Choice|States
    ## 1st column is choice.
    ## 1st+2nd columns are the state
    ch <- expand.grid(tasks$tasks, tasks$tasks)
    # Fix factor levels for next step
    levels(ch$Var2)
    levels(ch$Var2) = c("HHU", "HH0", "HLU", "HL0", "LHU", "LH0", "LLU", "LL0", "TR")
    for (chr in tasks$tasks) {
      ch <- rbind(ch, c(chr, NA))
      ch <- rbind(ch, c(chr, "TR"))
    }
    
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
                 pattern = paste(
                   "L",
                   str_sub(transitions[str_detect(str = str_sub(transitions$Var1, -1), pattern = "U") &
                                         str_detect(str = str_sub(transitions$Var1, start = -3, end = -3),
                                                    pattern = "H"), ]$Var1,
                           start = -2,
                           end = -2),
                   "U",
                   sep = ""
                 ))
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
    
    ##################################################
    ################# Simulation
    ##################################################
    
    # Backwards induction
    ## Determine number of loops
    rounds = 20
    ## Create column with values of each choice for each round
    col_names <- paste("rd", c(1:rounds), sep = "")
    ch[col_names] <- NA
    ## Last round:
    ### fill in column in "ch" with the values
    for (choice in tasks$tasks) {
      ### first column is choice: find in tasks table the value of that choice
      ch[ch$Var1 == choice, rounds + 2] <-
        tasks[tasks$tasks == choice, ]$value
    }
    ### Multiply by discount factor^round
    ch[, rounds + 2] <- ch[, rounds + 2] * gamma ^ (rounds - 1)
    
    
    ## Create state values for each round
    S[col_names] <- NA
    ### for each state in "S" choose the highest value in "ch" and create another column with the values
    best_option <-
      function(round,
               choice_matrix = ch,
               state_matrix = S,
               label_matrix = label) {
        for (row in c(1:nrow(state_matrix))) {
          if (is.na(state_matrix[row, ]$V2)) {
            state_matrix[row, round + 2] <-
              choice_matrix[choice_matrix$Var1 == state_matrix[row, ]$V1 &
                              is.na(choice_matrix$Var2), round +
                              2]
            label_matrix[row, round + 2] <-
              as.character(choice_matrix[choice_matrix$Var1 == state_matrix[row, ]$V1 &
                                           is.na(choice_matrix$Var2), 1])
          } else if (as.character(state_matrix[row, ]$V2) == "TR") {
            state_matrix[row, round + 2] <-
              na.exclude(choice_matrix[choice_matrix$Var1 == state_matrix[row, ]$V1 &
                                         choice_matrix$Var2 == state_matrix[row, ]$V2, round + 2])
            label_matrix[row, round + 2] <-
              as.character(choice_matrix[choice_matrix$Var1 == state_matrix[row, ]$V1 &
                                           is.na(choice_matrix$Var2), 1])
          } else {
            choice1 <-
              choice_matrix[choice_matrix$Var1 == state_matrix[row, ]$V1 &
                              choice_matrix$Var2 == state_matrix[row, ]$V2 &
                              !is.na(choice_matrix$Var2) &
                              as.character(choice_matrix$Var2) != "TR", round +
                              2]
            choice2 <-
              choice_matrix[choice_matrix$Var1 == state_matrix[row, ]$V2 &
                              choice_matrix$Var2 == state_matrix[row, ]$V1 &
                              !is.na(choice_matrix$Var2) &
                              as.character(choice_matrix$Var2) != "TR", round +
                              2]
            # print(choice1)
            # print(choice2)
            # print(row)
            # if (is.na(state_matrix[row,]$V2)) {
            #   state_matrix[row,round + 2] <- choice_matrix[choice_matrix$Var1 == state_matrix[row,]$V1 &
            #                                                  is.na(choice_matrix$Var2), round + 2]
            #   label_matrix[row,round + 2] <- as.character(choice_matrix[choice_matrix$Var1 == state_matrix[row,]$V1 &
            #                                                  is.na(choice_matrix$Var2), 1])
            if (choice1 > choice2) {
              state_matrix[row, round + 2] <- choice1
              # ### save choices: change the suboptimal choices to NA
              # choice_matrix[choice_matrix$Var1 == state_matrix[row,]$V2 &
              #                 choice_matrix$Var2 == state_matrix[row,]$V1 &
              #                 !is.na(choice_matrix$Var2) &
              #                 as.character(choice_matrix$Var2) != "TR",round +2] <- NA
              label_matrix[row, round + 2] <-
                as.character(na.exclude(choice_matrix[choice_matrix$Var1 == state_matrix[row, ]$V1 &
                                                        choice_matrix$Var2 == state_matrix[row, ]$V2, 1]))
            } else if (choice1 < choice2) {
              state_matrix[row, round + 2] <- choice2
              # ### save choices: change the suboptimal choices to NA
              # choice_matrix[choice_matrix$Var1 == state_matrix[row,]$V1 &
              #                 choice_matrix$Var2 == state_matrix[row,]$V2 &
              #                 !is.na(choice_matrix$Var2) &
              #                 as.character(choice_matrix$Var2) != "TR",round +2] <- NA
              label_matrix[row, round + 2] <-
                as.character(na.exclude(choice_matrix[choice_matrix$Var1 == state_matrix[row, ]$V2 &
                                                        choice_matrix$Var2 == state_matrix[row, ]$V1, 1]))
            } else if (choice2 == choice1) {
              state_matrix[row, round + 2] <- choice1
              label_matrix[row, round + 2] <- "either"
            }
          }
        }
        return(list(state_matrix, choice_matrix, label_matrix))
      }
    
    label <- S
    new_states_choices <- best_option(rounds)
    S <- new_states_choices[[1]]
    ch <- new_states_choices[[2]]
    label <- new_states_choices[[3]]
    ## second to last round to first round
    ### loop through the choices|states in "S"
    ### For each choice|state, calculate the the expected value
    
    # We need to change "transitions" for simplicity of search
    transitions$choice <- substr(transitions$Var1, 1, 3)
    transitions$non_choice <- substr(transitions$Var1, 5, 7)
    transitions[transitions$non_choice == "NA", ]$non_choice <- NA
    transitions$state2_1 <- substr(transitions$Var2, 1, 3)
    transitions$state2_2 <- substr(transitions$Var2, 5, 7)
    transitions[transitions$state2_2 == "NA", ]$state2_2 <- NA
    
    for (i in c(1:(rounds - 1))) {
      values <- S[, c(1, 2, (rounds + 3 - i))]
      for (row in c(1:nrow(ch))) {
        #### create probability vector P [44x1] from "transitions"
        if (!is.na(ch[row, 2])) {
          prob_weight <-
            transitions[ch[row, 1] == transitions$choice &
                          ch[row, 2] == transitions$non_choice &
                          !is.na(ch[row, 2]) &
                          !is.na(transitions$non_choice), ]
        } else if (is.na(ch[row, 2])) {
          prob_weight <-
            transitions[ch[row, 1] == transitions$choice &
                          is.na(ch[row, 2]) &
                          is.na(transitions$non_choice), ]
        }
        
        #### Make sure "S" is in the same order [1x44]
        prob_weight <- merge(
          x = prob_weight,
          y = values,
          by.x = c("state2_1", "state2_2"),
          by.y = c("V1", "V2")
        )
        prob_weight$weighted_value <-
          prob_weight$prob * values[, c(length(values))]
        ### Multiply by discount factor^round
        ch[row, rounds + 2 - i] <-
          gamma ^ (rounds - 1 - i) * tasks[tasks$tasks == ch[row, 1], ]$value + sum(prob_weight$weighted_value)
      }
      new_states_choices <- best_option(rounds - i)
      S <- new_states_choices[[1]]
      ch <- new_states_choices[[2]]
      label <- new_states_choices[[3]]
    }
    urgency <-
      apply(label[, -c(1:2)], 2, function(X)
        substr(X, nchar(X), nchar(X)))
    ratios <-
      append(ratios, table(urgency)["U"] / sum(table(urgency)))
  }
}

ratios_grid <- matrix(
  ratios,
  nrow = length(gamma_grid),
  ncol = length(cost2_grid),
  dimnames = list(gamma_grid,
                  cost2_grid)
)

### Export labels
write.csv(ratios_grid, "./urgency-ratio-grid.csv")
