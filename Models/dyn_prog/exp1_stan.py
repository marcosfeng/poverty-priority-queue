import numpy as np
import pandas as pd
from cmdstanpy import CmdStanModel
import h5py

df = pd.read_csv('/home/master/poverty-priority-queue/data/Exp1/df.csv')
transition_df = pd.read_csv('/home/master/poverty-priority-queue/Models/dyn_prog/transition_df.csv')

# Remove block with defective trials:
participant_to_remove = df.iloc[2375]['participant']
block_to_remove = df.iloc[2375]['block']
# Create a condition that is True for the rows you want to keep
condition = ~((df['participant'] == participant_to_remove) & (df['block'] == block_to_remove))
# Apply the condition to the DataFrame
df = df[condition]

# Extract the unique state names
unique_states = transition_df['Unnamed: 0'].values
# Create a mapping from unique state names to indices
state_to_index = {state: index+1 for index, state in enumerate(unique_states)} # +1 because Stan indexing starts from 1

def calculate_counter(df):
    df['counter_left'] = 0
    df['counter_right'] = 0
    # Loop over the rows in the DataFrame
    for i in range(1, len(df)):
        # If the participant is the same as in the previous row
        if df.loc[i, 'participant'] == df.loc[i - 1, 'participant']:
            # If the task is the same as in the previous round and wasn't chosen, increment the counter
            if df.loc[i, 'left'] == df.loc[i - 1, 'left'] and df.loc[i - 1, 'choice'] != 1:
                df.loc[i, 'counter_left'] = df.loc[i - 1, 'counter_left'] + 1
            if df.loc[i, 'right'] == df.loc[i - 1, 'right'] and df.loc[i - 1, 'choice'] != 2:
                df.loc[i, 'counter_right'] = df.loc[i - 1, 'counter_right'] + 1
            # If the counter reached 3 in the previous round, reset it to 0
            if df.loc[i - 1, 'counter_left'] == 2:
                df.loc[i, 'counter_left'] = 0
            if df.loc[i - 1, 'counter_right'] == 2:
                df.loc[i, 'counter_right'] = 0
    return df

df = df.reset_index(drop=True)
df = calculate_counter(df)

# Define the mapping from columns to state name components
mapping = {
    'u': {0: '0', 1: 'U', np.nan: '<NA>'},
    'i': {0: 'L', 1: 'H', np.nan: ''},
    'e': {0: 'L', 1: 'H', np.nan: ''}
}

# Define the function to create state names
def create_state_name(row):
    # Create the state name according to the naming convention used in the exp1-transitions.py script
    # Handle NaN values explicitly
    state_name_1 = mapping['i'].get(row['i1'], '') + mapping['e'].get(row['e1'], '') + mapping['u'].get(row['u1'], '<NA>') + str(row['counter_left'])
    state_name_2 = mapping['i'].get(row['i2'], '') + mapping['e'].get(row['e2'], '') + mapping['u'].get(row['u2'], '<NA>') + str(row['counter_right'])

    # Try both orders for the state names
    state_name = ','.join(sorted([state_name_1, state_name_2]))
    state_name_reverse = ','.join(sorted([state_name_1, state_name_2], reverse=True))

    # If the state order was reversed, recode the choice
    if state_name != state_name_reverse and row['choice'] == 1:
        row['choice'] = 2
    elif state_name != state_name_reverse and row['choice'] == 2:
        row['choice'] = 1

    # Return the state index if the state name exists in the dictionary
    if state_name in state_to_index:
        return state_to_index[state_name]
    elif state_name_reverse in state_to_index:
        return state_to_index[state_name_reverse]
    else:
        raise ValueError(f"The state name '{state_name}' or '{state_name_reverse}' does not exist in the dictionary. Row: {row}")

# Create an empty dictionary to hold the data
data = {}
# Number of rounds per block
block_size = 40

# Create the state matrix
state_matrix = df.apply(create_state_name, axis=1).values

# Create the choice matrix
choice_matrix = df['choice'].values

# Reshape the state and choice matrices into blocks
state_matrix = [state_matrix[i:i+block_size] for i in range(0, len(state_matrix), block_size)]
choice_matrix = [choice_matrix[i:i+block_size] for i in range(0, len(choice_matrix), block_size)]

# Number of blocks
data['N'] = len(state_matrix)
# Number of states
data['K'] = len(np.unique(state_matrix))

# Update the state and choice matrices in the data dictionary
data['state'] = np.array(state_matrix, dtype=int)
data['choice'] = choice_matrix
data['choice'] = np.stack([np.array(lst, dtype=int) for lst in data['choice']])

# Create a dictionary that maps each unique participant ID to a unique integer
participant_mapping = {participant_id: index for index,
                       participant_id in enumerate(np.unique(df['participant']),
                                                   start=1)}
# Export dictionary for R
with h5py.File('Models/dyn_prog/participant_mapping.h5', 'w') as h5file:
    participant_ids = list(participant_mapping.keys())
    indices = list(participant_mapping.values())
    h5file.create_dataset('participant_ids', data=np.array(participant_ids, dtype='S'))
    h5file.create_dataset('indices', data=np.array(indices))
    
# Create a new array of participant IDs, where each string ID is replaced with its corresponding integer
participant_ids = np.array([participant_mapping[id]
                            for id, round in zip(df['participant'], df['round'])
                            if round == 1])
data['participants'] = participant_ids
data['P'] = len(np.unique(participant_ids))

# Each block has a fixed number of rounds
data['T'] = block_size

# Initialize the effort and reward matrices
effort = np.zeros((data['K'], 2))
reward = np.zeros((data['K'], 2))
# Create a mapping for the reward and effort levels
reward_mapping = {'L': 1, 'H': 2}
effort_mapping = {'L': 0, 'H': 1}
# Iterate over the state dictionary
state_to_index = {state: index - 1 for state, index in state_to_index.items()}
for state, index in state_to_index.items():
    # Split the state into two parts
    state_1, state_2 = state.split(',')
    
    # Check if '<NA>' is in state_1
    if '<NA>' in state_1:
        reward[index, 0] = 0
        effort[index, 0] = 0
    else:
        # Extract the reward and effort levels for state_1
        reward_1, effort_1 = state_1[:2]
        # Map the reward and effort levels to their respective numeric values
        reward[index, 0] = reward_mapping[reward_1]
        effort[index, 0] = effort_mapping[effort_1]
    
    # Check if '<NA>' is in state_2
    if '<NA>' in state_2:
        reward[index, 1] = 0
        effort[index, 1] = 0
    else:
        # Extract the reward and effort levels for state_2
        reward_2, effort_2 = state_2[:2]
        # Map the reward and effort levels to their respective numeric values
        reward[index, 1] = reward_mapping[reward_2]
        effort[index, 1] = effort_mapping[effort_2]

# Add the matrices to the data dictionary
data['effort'] = effort.astype(int)
data['reward'] = reward.astype(int)

# Now, transition_probs is a 3D array where transition_probs[:,:,0] is the matrix for choice 1
# and transition_probs[:,:,1] is the matrix for choice 2
transition_probs_1 = pd.read_csv('/home/master/poverty-priority-queue/Models/dyn_prog/transition_matrix_0.csv', header=None)
transition_probs_2 = pd.read_csv('/home/master/poverty-priority-queue/Models/dyn_prog/transition_matrix_1.csv', header=None)
transition_probs = np.stack([transition_probs_1, transition_probs_2], axis=0)
# Add the transition probability matrix to the data dictionary
data['transition_probs'] = transition_probs

## Delete unnecessary variables
all_vars = list(globals().keys())
keep_vars = ['data', 'model', 'r', 'CmdStanModel', 'np', 'pd']
## Delete all variables except for the ones to keep
for var in all_vars:
    if var not in keep_vars:
        del globals()[var]
del all_vars
## Perform garbage collection
import gc
gc.collect()

with h5py.File('/home/master/poverty-priority-queue/Models/dyn_prog/data.h5', 'w') as f:
    for key in data.keys():
        f.create_dataset(key, data=data[key])

