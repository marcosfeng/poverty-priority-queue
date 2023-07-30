import pandas as pd
import numpy as np
import os
import json

# Function to extract task features
def extract_features(task):
    if pd.isna(task):
        return np.nan, np.nan, np.nan
    u = int('-U' in task)
    i = int('2.png' in task)
    e = int('-sp' in task)
    return u, i, e

# Source and destination folders
source_folder = './data/Exp1/pavlovia'
destination_folder = './data/Exp1/processed'

# Loop through all CSV files in the source folder
for filename in os.listdir(source_folder):
    if filename.endswith('.csv'):
        source_file_path = os.path.join(source_folder, filename)
        
        # Load the data
        data = pd.read_csv(source_file_path)

        # Remove the calibration trials (mouse.x is NA) or any trials where mouse.x is "[]"
        data = data.dropna(subset=['mouse.x'])
        data = data[data['mouse.x'] != "[]"]

        # Add the columns: u1 u2 i1 i2 e1 e2
        data[['u1', 'i1', 'e1']] = pd.DataFrame(data['left'].apply(extract_features).tolist(), index=data.index)
        data[['u2', 'i2', 'e2']] = pd.DataFrame(data['right'].apply(extract_features).tolist(), index=data.index)

        # Add a choice column (whether they selected task 1 or task 2)
        data['choice'] = data['mouse.clicked_name'].apply(lambda x: 1 if x == 'task1_poly' else 2 if x == 'task2_poly' else np.nan)
        
        # Remove the specified columns
        columns_to_drop = ['RoundLeft'] + list(data.loc[:, 'date':'calibration_y'].columns) + list(data.loc[:, 'ic_trials.thisRepN':'path'].columns)
        data = data.drop(columns=columns_to_drop, errors='ignore')
        
        # Save the processed data to the destination folder
        destination_file_path = os.path.join(destination_folder, filename)
        data.to_csv(destination_file_path, index=False)

####################################################################################################
# Function to calculate the fixation points

# Define the boundaries for the regions of interest
regions = {
    "top_left": [-0.4, 0.78, 0.2, 0.2],
    "top_right": [0.4, 0.78, 0.2, 0.2],
    "bottom_left": [-0.5, -0.238, 0.2, 0.2],
    "bottom_right": [0.5, -0.238, 0.2, 0.2]
}

def process_fixations(fixation_data):
    """Process a list of fixations."""
    fixations = json.loads(fixation_data)
    first_fixation = None
    last_fixation = None
    total_time_in_regions = {region: 0 for region in regions}
    fixation_path = []

    for fixation in fixations:
        x = fixation['x']
        y = fixation['y']
        timestamp = fixation['timestamp']

        # First and last fixation
        if first_fixation is None:
            first_fixation = (x, y)
        last_fixation = (x, y)

        # Fixation path
        if len(fixation_path) == 0 or timestamp - fixation_path[-1][2] >= 250:
            fixation_path.append((x, y, timestamp))

        # Time spent in each region
        for region, (center_x, center_y, size_x, size_y) in regions.items():
            if (center_x - size_x / 2 <= x <= center_x + size_x / 2 and
                center_y - size_y / 2 <= y <= center_y + size_y / 2):
                if len(fixation_path) >= 2:
                    total_time_in_regions[region] += fixation_path[-1][2] - fixation_path[-2][2]

    return first_fixation, last_fixation, total_time_in_regions, fixation_path

# Apply the function and check for errors
df_test = pd.read_csv("data/Exp1/processed/5a2adf6a8e00a000019864fb_prioq-eyetrack_2023-07-19_22h44.40.711.csv")
df_test['processed_fixations'] = df_test['rawFixations'].apply(process_fixations)
df_test.head()
