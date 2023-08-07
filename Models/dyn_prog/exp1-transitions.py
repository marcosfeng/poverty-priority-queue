import pandas as pd
import numpy as np
import ast

# Define the possible tasks and their durations
tasks = ["HHU", "HLU", "LHU", "LLU", "HH0", "HL0", "LH0", "LL0"]

# Non-zero duration task with zero duration task
states = [f"{task1}0,{task2}{duration1}" 
          for task1 in tasks 
          for duration1 in range(1, 4) 
          for task2 in tasks[4:]]

# Zero duration tasks with non-identical zero duration task
states += [f"{task1}0,{task2}0" 
           for k, task1 in enumerate(tasks) 
           for task2 in tasks[:k+1]]

# '<NA>' with zero duration tasks
states += [f"{task}0,<NA>{duration}" 
           for task in tasks 
           for duration in [0, 1]]

# Initialize transition tensor with zeros
transition_tensor = np.zeros((len(states), 2, len(states)))

# Create a dictionary for easier next state searching
state_dict = {state: i for i, state in enumerate(states)}

# Function to update transition tensor
def update_tensor(i, j, next_states, increment):
    for next_state in next_states:
        k = state_dict.get(next_state)
        if k is not None:
            transition_tensor[i, j, k] += increment

def update_tensor_reset(i, j):
    next_states = [next_state
                   for next_state in states
                   if (any([task in ["HHU0","HLU0","LH00","LL00"]
                           for task in next_state.split(",")]) and
                       all([task[-1] == "0" and task[:-1] != "<NA>"
                            for task in next_state.split(",")]))]
    for next_state in next_states:
        k = state_dict.get(next_state)
        if k is not None:
            transition_tensor[i, j, k] += 1 / (len(tasks))**2

    next_states = [next_state
                   for next_state in states
                   if (all([task in ["HHU0","HLU0","LH00","LL00"]
                            for task in next_state.split(",")]) and
                       all([task1 != task2 
                            for task1, task2 in zip(*[iter(next_state.split(","))]*2)]))]
    for next_state in next_states:
        k = state_dict.get(next_state)
        if k is not None:
            transition_tensor[i, j, k] += 1 / (len(tasks))**2

    next_states = [next_state
                   for next_state in states
                   if (any([task in ["LHU0","LLU0","HH00","HL00"]
                           for task in next_state.split(",")]) and
                       all([task[-1] == "0" and task[:-1] != "<NA>"
                            for task in next_state.split(",")]))]
    for next_state in next_states:
        k = state_dict.get(next_state)
        if k is not None:
            transition_tensor[i, j, k] += 0.5 / (len(tasks))**2

    next_states = [next_state
                   for next_state in states
                   if (all([task in ["LHU0","LLU0","HH00","HL00"]
                            for task in next_state.split(",")]) and
                       all([task1 != task2 
                            for task1, task2 in zip(*[iter(next_state.split(","))]*2)]))]
    for next_state in next_states:
        k = state_dict.get(next_state)
        if k is not None:
            transition_tensor[i, j, k] += 0.5 / (len(tasks))**2
    
    next_states = [next_state
                   for next_state in states
                   if next_state in ["HH00,LHU0", "HH00,LLU0",
                                      "HL00,LHU0", "HL00,LLU0"]]
    for next_state in next_states:
        k = state_dict.get(next_state)
        if k is not None:
            transition_tensor[i, j, k] += 4 / (len(tasks))**2

# Fill in the transition probabilities
for i, state in enumerate(states):
    for j in range(2):
        # Skip if the choice is not possible
        if state.split(",")[j][:-1] == "<NA>":
            continue
        # Parse remaining task and duration
        remaining_task_duration = state.split(",")[1-j]
        remaining_task, remaining_duration = remaining_task_duration[:-1], int(remaining_task_duration[-1])
        
        if remaining_task == "<NA>":
            if remaining_duration < 1:
                next_states = [f"{task}0,<NA>1" for task in tasks]
                update_tensor(i, j, next_states, 1 / len(next_states))
            else:
                update_tensor_reset(i, j)

        elif remaining_task == "HHU":
            next_states = [next_state
                           for next_state in states
                           if ("LHU0" in next_state.split(",") and
                               all([task[-1] == "0" and task[:-1] != "<NA>"
                                    for task in next_state.split(",")]))]
            update_tensor(i, j, next_states, 0.5 / len(next_states))
            next_states = ["HH00,LHU0", "HL00,LHU0"]
            update_tensor(i, j, next_states, 0.5 / len(next_states))

        elif remaining_task == "HLU":
            next_states = [next_state
                           for next_state in states
                           if ("LLU0" in next_state.split(",") and
                               all([task[-1] == "0" and task[:-1] != "<NA>"
                                    for task in next_state.split(",")]))]
            update_tensor(i, j, next_states, 0.5 / len(next_states))
            next_states = [f"HH00,LLU0", f"HL00,LLU0"]
            update_tensor(i, j, next_states, 0.5 / len(next_states))     

        elif remaining_task in ["LHU", "LLU"]:
            next_states = [f"{task}0,<NA>0" for task in tasks]
            update_tensor(i, j, next_states, 1 / len(next_states))

        elif remaining_task in ["HH0", "HL0"]:
            # The task can be replaced by a new task after four rounds
            if remaining_duration < 3:
                next_states = [next_state
                               for next_state in states
                               if (f"{remaining_task}{remaining_duration+1}" in next_state.split(",") and
                                   any([task[-1] == "0" and task[:-1] != "<NA>"
                                        for task in next_state.split(",")]))]
                update_tensor(i, j, next_states, 0.5 / len(next_states))
                next_states = [next_state
                               for next_state in states
                               if (f"{remaining_task}{remaining_duration+1}" in next_state.split(",") and
                                      any([task in ["LH00","LL00"]
                                       for task in next_state.split(",")]))]
                update_tensor(i, j, next_states, 0.5 / len(next_states))
            else:
                update_tensor_reset(i, j)

        elif remaining_task in ["LH0", "LL0"]:
            # The task can be replaced by a new task after four rounds
            if remaining_duration < 3:
                next_states = [next_state
                               for next_state in states
                               if (f"{remaining_task}{remaining_duration+1}" in next_state.split(",") and
                                   any([task[-1] == "0" and task[:-1] != "<NA>"
                                        for task in next_state.split(",")]))]
                update_tensor(i, j, next_states, 1 / len(next_states))
            else:
                update_tensor_reset(i, j)

# Normalize the transition probabilities to ensure they sum to 1
transition_tensor = transition_tensor / transition_tensor.sum(axis=2, keepdims=True)

# Handle any NaN values (resulting from 0/0) by setting them to 0
transition_tensor = np.nan_to_num(transition_tensor)

# Create a DataFrame to store the transition probabilities
transition_df = pd.DataFrame(index=states, columns=["Choice 0", "Choice 1"])

# Fill in the DataFrame
for i, state in enumerate(states):
    for j in range(2):
        next_states_probs = {next_state: transition_tensor[i, j, k] 
                             for k, next_state in enumerate(states) 
                             if transition_tensor[i, j, k] > 0}
        transition_df.at[state, f"Choice {j}"] = next_states_probs

transition_df.to_csv('./dyn_prog/transition_df.csv')

# Identify all unique states
unique_states = np.unique(np.concatenate([list(ast.literal_eval(row['Choice 0']).keys()) + 
                                          list(ast.literal_eval(row['Choice 1']).keys()) 
                                          for _, row in transition_df.iterrows()]))

# Create a dictionary to map state names to indices
state_to_index = {state: index for index, state in enumerate(unique_states)}

# Initialize the transition matrices
transition_matrix_0 = np.zeros((transition_df.shape[0], len(unique_states)))
transition_matrix_1 = np.zeros((transition_df.shape[0], len(unique_states)))

# Fill the transition matrices
for i, row in transition_df.iterrows():
    for state, prob in ast.literal_eval(row['Choice 0']).items():
        transition_matrix_0[i, state_to_index[state]] = prob
    for state, prob in ast.literal_eval(row['Choice 1']).items():
        transition_matrix_1[i, state_to_index[state]] = prob

# Check the first few rows of the transition matrices
transition_matrix_0[:5, :], transition_matrix_1[:5, :]

