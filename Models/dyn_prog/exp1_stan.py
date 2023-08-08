import numpy as np
import pandas as pd

df = pd.read_csv('./data/Exp1/df.csv')
transition_df = pd.read_csv('./Models/dyn_prog/transition_df.csv')

# Extract the unique state names
unique_states = transition_df['Unnamed: 0'].values

# Create a mapping from unique state names to indices
state_to_index = {state: index+1 for index, state in enumerate(unique_states)} # +1 because Stan indexing starts from 1

def calculate_counter(df):
    df['counter_left'] = (df['left'] != df['left'].shift()).groupby(df['participant']).cumsum()
    df['counter_right'] = (df['right'] != df['right'].shift()).groupby(df['participant']).cumsum()

    df['counter_left'] = df.groupby(['participant', 'counter_left']).cumcount()
    df['counter_right'] = df.groupby(['participant', 'counter_right']).cumcount()

    return df

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

    # Return the state index if the state name exists in the dictionary
    if state_name in state_to_index:
        return state_to_index[state_name]
    elif state_name_reverse in state_to_index:
        return state_to_index[state_name_reverse]
    else:
        raise ValueError(f"The state name '{state_name}' or '{state_name_reverse}' does not exist in the dictionary.")

# Create the state matrix
state_matrix = df.apply(create_state_name, axis=1).values

# Define the number of trials and participants
T = df['round'].max()
N = df['participant'].nunique()

# Reshape the state matrix to have dimensions [T, N]
state_matrix = np.reshape(state_matrix, (T, N))

# Check the first few entries of the state matrix
print(state_matrix[:5, :5])





# Create the state and choice matrices again
state_names = df['state'].unique()
state_to_int = {name: i for i, name in enumerate(state_names)}
int_to_state = {i: name for i, name in enumerate(state_names)}

state_matrix = df.groupby('participant')['state'].apply(lambda x: [state_to_int[state] for state in x]).values
choice_matrix = df.groupby('participant')['choice'].apply(lambda x: x.values).values


# Organize data
data = {
    'N': len(state_matrix),
    'K': len(np.unique(state_matrix)),
    'state': state_matrix,
    'choice': choice_matrix
}

# Fit the model to the data
fit = sm.sampling(data=data)

# Extract the samples
samples = fit.extract(permuted=True)
