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
# Calculate the fixation points


# Define the boundaries for the regions of interest
regions = {
    "top_left": [-0.4, 0.78, 0.1 * 2.5, 0.1 * 3], 
    "top_right": [0.4, 0.78, 0.1 * 2.5, 0.1 * 3], 
    "bottom_left": [-0.5, -0.238, 0.23 * 2, 0.45 * 2.5],
    "bottom_right": [0.5, -0.238, 0.23 * 2, 0.45 * 2.5]
}

# Lists of common screen resolutions
common_horizontal_resolutions = [1024, 1280, 1366, 1600, 1920, 2048, 2560, 2880, 3840, 4096, 5120, 7680, 8192]
common_vertical_resolutions = [576, 720, 768, 800, 900, 1024, 1080, 1200, 1440, 1536, 1600, 1800, 1920, 2160, 2400, 2880, 3200, 3840, 4320, 4800, 5120, 5760, 6144]

# Function to round to nearest resolution
def round_to_nearest_resolution(n, resolution_list):
    # Find the resolution in the list that is closest to n
    closest_resolution = min(resolution_list, key=lambda x:abs(x-n))
    return closest_resolution

def idt(fixations, w_thresh, d_thresh):
    """Apply the I-DT algorithm to a list of fixations."""
    w = w_thresh
    fixation_sequences = []

    for i in range(len(fixations)):
        if i + w > len(fixations):
            break

        window = fixations[i:i+w]
        d = np.sqrt((max(p[0] for p in window) - min(p[0] for p in window))**2 +
                    (max(p[1] for p in window) - min(p[1] for p in window))**2)

        if d <= d_thresh:
            w += 1
            continue

        if w > w_thresh:
            fixation_sequences.append(window[:-1])

        w = w_thresh

    return fixation_sequences

def process_fixations_scaled(fixation_data):
    """Process a list of fixations with coordinates scaled to [-1, 1] and trial start time set to 0."""
    fixations = json.loads(fixation_data)
    first_fixation = None
    last_fixation = None
    total_time_in_regions = {region: 0 for region in regions}
    
    # Get the timestamp of the first fixation
    start_time = fixations[0]['timestamp'] if fixations else 0

    fixation_data = []
    for fixation in fixations:
        # Scale the x and y coordinates
        x = fixation['x'] / max_x * 2 - 1
        y = fixation['y'] / max_y * 2 - 1

        # Adjust the timestamp to start from 0
        timestamp = fixation['timestamp'] - start_time

        # First and last fixation
        if first_fixation is None:
            first_fixation = (x, y)
        last_fixation = (x, y)

        fixation_data.append((x, y, timestamp))

    # Apply the I-DT algorithm to find fixation sequences
    w_thresh = 50  # Minimum window size (ms)
    d_thresh = 0.1  # Dispersion threshold (normalized units)
    fixation_path = idt(fixation_data, w_thresh, d_thresh)

    # Time spent in each region
    for region, (center_x, center_y, size_x, size_y) in regions.items():
        for fixation_sequence in fixation_path:
            for fixation in fixation_sequence:
                x, y, _ = fixation
                if (center_x - size_x / 2 <= x <= center_x + size_x / 2 and
                    center_y - size_y / 2 <= y <= center_y + size_y / 2):
                    if len(fixation_sequence) >= 2:
                        total_time_in_regions[region] += fixation_sequence[-1][2] - fixation_sequence[0][2]

    return first_fixation, last_fixation, total_time_in_regions, fixation_path

# Apply the function and create new columns
df_test[['first_fixation', 'last_fixation', 'time_in_regions', 'path']] = pd.DataFrame(df_test['rawFixations'].apply(process_fixations_scaled).tolist(), index=df_test.index)

df_test.head()
