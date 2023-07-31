import pandas as pd
import numpy as np
import os
import json
import matplotlib.pyplot as plt
import matplotlib.patches as patches

# Define the ROIs with a buffer for measurement error
buffer = 1.8  # Adjust this value as needed

regions = {
    "top_left": [-0.4, 0.78, 0.4 * buffer, 0.2 * buffer], 
    "top_right": [0.4, 0.78, 0.4 * buffer, 0.2 * buffer], 
    "bottom_left": [-0.5, -0.238, 0.23 * buffer, 0.45 * buffer],
    "bottom_right": [0.5, -0.238, 0.23 * buffer, 0.45 * buffer]
}

# Define common resolution pairs
common_resolutions = [(1024, 576), (1280, 720), (1366, 768),
                      (1600, 900), (1920, 1080), (2048, 1152),
                      (2560, 1440), (2880, 1620), (3840, 2160),
                      (4096, 2304), (5120, 2880), (7680, 4320),
                      (8192, 4608), (1440, 900), (2560, 1600),
                      (2880, 1800), (3072, 1920)]

# Function to extract task features
def extract_features(task):
    if pd.isna(task):
        return np.nan, np.nan, np.nan
    u = int('-U' in task)
    i = int('2.png' in task)
    e = int('-sp' in task)
    return u, i, e

# Function to round to nearest resolution
def round_to_nearest_resolution(x, y, resolution_list):
    # Find the resolution in the list that is closest to x and y
    closest_resolution = min(resolution_list, key=lambda res: np.sqrt((x - res[0])**2 + (y - res[1])**2))
    return closest_resolution

def idt(fixations, w_thresh, d_thresh):
    """Apply the I-DT algorithm to a list of fixations."""
    w = w_thresh
    fixation_sequences = []
    i = 0
    j = 0

    while j < len(fixations):
        j = i + 1
        while j < len(fixations) and fixations[j][2] - fixations[i][2] < w:
            j += 1

        window = fixations[i:j]
        d = np.sqrt((max(p[0] for p in window) - min(p[0] for p in window))**2 +
                    (max(p[1] for p in window) - min(p[1] for p in window))**2)

        if d <= d_thresh:
            w += 2
            continue

        if j - i > 1:
            fixation_sequences.append(window)

        i = j
        w = w_thresh

    return fixation_sequences

def process_fixations_scaled(fixation_data, max_x, max_y):
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
    w_thresh = 200  # Minimum window size (ms)
    d_thresh = 0.05  # Dispersion threshold (normalized units)
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

# Source and destination folders
source_folder = './data/Exp1/pavlovia'
destination_folder = './data/Exp1/processed'

# Loop through all CSV files in the source folder
for filename in os.listdir(source_folder):
    if filename.endswith('.csv'):
        source_file_path = os.path.join(source_folder, filename)
        
        # Load the data
        data = pd.read_csv(source_file_path)
        
        # Add the 'RoundLeft' column to the data
        data['RoundLeft'] = data['RoundLeft'][0]
        # Remove the calibration trials (mouse.x is NA) or any trials where mouse.x is "[]"
        data = data.dropna(subset=['mouse.x'])
        data = data[data['mouse.x'] != "[]"]

        # Add the columns: u1 u2 i1 i2 e1 e2
        data[['u1', 'i1', 'e1']] = pd.DataFrame(data['left'].apply(extract_features).tolist(), index=data.index)
        data[['u2', 'i2', 'e2']] = pd.DataFrame(data['right'].apply(extract_features).tolist(), index=data.index)

        # Add a choice column (whether they selected task 1 or task 2)
        data['choice'] = data['mouse.clicked_name'].apply(lambda x: 1 if 'task1_poly' in x else 2 if 'task2_poly' in x else np.nan)

        # Extract all fixations from all rows
        all_fixations = [json.loads(row) for row in data['rawFixations']]

        # Remove the specified columns
        columns_to_drop = (list(data.loc[:, 'date':'calibration_y'].columns) 
                         + list(data.loc[:, 'ic_trials.thisRepN':'path'].columns)
                         + ["calibrationClick.clicked_name", "mouse.x",
                            "mouse.y", "mouse.leftButton", "mouse.midButton",
                            "mouse.rightButton", "mouse.time", "mouse.clicked_name"])
                            
        # Check if there are at least two rows where all fixations are the same
        rows_with_same_fixations = [len(set((fixation['x'], fixation['y'])
                                            for fixation in fixations)) == 1
                                            for fixations in all_fixations]
        if rows_with_same_fixations.count(True) >= 2:
            # Save the processed data to the destination folder
            data = data.drop(columns=columns_to_drop, errors='ignore')
            destination_file_path = os.path.join(destination_folder, filename)
            data.to_csv(destination_file_path, index=False)
            continue

        # Flatten the list of lists
        all_fixations = [fixation for sublist in all_fixations for fixation in sublist]

        # Find the max x and y values
        max_x = max(fixation['x'] for fixation in all_fixations)
        max_y = max(fixation['y'] for fixation in all_fixations)
        # Round the maxima to plausible screen resolution values
        max_x, max_y = round_to_nearest_resolution(max_x, max_y, common_resolutions)

        # Apply the function and create new columns
        data[['first_fixation', 'last_fixation', 'time_in_regions', 'fixation_path']] = pd.DataFrame(data['rawFixations'].apply(lambda x: process_fixations_scaled(x, max_x, max_y)).tolist(), index=data.index)
        
        print(data['path'])
        # Save the processed data to the destination folder
        data = data.drop(columns=columns_to_drop, errors='ignore')
        destination_file_path = os.path.join(destination_folder, filename)
        data.to_csv(destination_file_path, index=False)
