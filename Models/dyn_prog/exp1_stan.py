import numpy as np
import pandas as pd

df = pd.read_csv('data/exp1/df.csv')

mapping = {
    'u': {0: '0', 1: 'U', np.nan: 'N'},
    'i': {0: 'L', 1: 'H', np.nan: 'N'},
    'e': {0: 'L', 1: 'H', np.nan: 'N'}
}

# Adjust the create_state_name function to handle NaN values
def create_state_name(row):
    # Convert u, i, e values to characters, handling NaN values
    u1 = mapping['u'][row['u1']] if not np.isnan(row['u1']) else 'N'
    i1 = mapping['i'][row['i1']] if not np.isnan(row['i1']) else 'N'
    e1 = mapping['e'][row['e1']] if not np.isnan(row['e1']) else 'N'
    u2 = mapping['u'][row['u2']] if not np.isnan(row['u2']) else 'N'
    i2 = mapping['i'][row['i2']] if not np.isnan(row['i2']) else 'N'
    e2 = mapping['e'][row['e2']] if not np.isnan(row['e2']) else 'N'

    # Use the 'repeats' column to determine the entry rounds
    t1 = str(row['round'] - row['repeats'][0])
    t2 = str(row['round'] - row['repeats'][1])

    # Combine the characters and times to create the state name
    state_name = f"{i1}{e1}{u1}{t1},{i2}{e2}{u2}{t2}"

    return state_name

# Apply the adjusted mapping to create the state names
df['state'] = df.apply(create_state_name, axis=1)

# Create the state and choice matrices again
state_names = df['state'].unique()
state_to_int = {name: i for i, name in enumerate(state_names)}
int_to_state = {i: name for i, name in enumerate(state_names)}

state_matrix = df.groupby('participant')['state'].apply(lambda x: [state_to_int[state] for state in x]).values
choice_matrix = df.groupby('participant')['choice'].apply(lambda x: x.values).values

state_matrix, choice_matrix
