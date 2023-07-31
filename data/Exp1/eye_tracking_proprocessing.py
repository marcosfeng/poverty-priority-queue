import pandas as pd
import numpy as np
import os
import json
import matplotlib.pyplot as plt
import matplotlib.patches as patches

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

df_test = pd.read_csv('/Users/marcosgallo/Documents/GitHub/poverty-priority-queue/data/Exp1/processed/5b745fb794c93d00010fc4c6_prioq-eyetrack_2023-07-19_18h12.32.493.csv')

# Extract all fixations from all rows
all_fixations = [json.loads(row) for row in df_test['rawFixations']]

# Flatten the list of lists
all_fixations = [fixation for sublist in all_fixations for fixation in sublist]

# Find the max x and y values
max_x = max(fixation['x'] for fixation in all_fixations)
max_y = max(fixation['y'] for fixation in all_fixations)
# Round the maxima to plausible screen resolution values
max_x, max_y = round_to_nearest_resolution(max_x, max_y, common_resolutions)

# Apply the function and create new columns
df_test[['first_fixation', 'last_fixation', 'time_in_regions', 'path']] = pd.DataFrame(df_test['rawFixations'].apply(lambda x: process_fixations_scaled(x, max_x, max_y)).tolist(), index=df_test.index)

df_test.head()

# Extract the fixation sequences from the first row
fixation_sequences = df_test.loc[1, 'path']

# First, let's make the circles just dots and all the same size.
fig, ax = plt.subplots(figsize=(15,10))

# Add the image
img = plt.imread('/Users/marcosgallo/Documents/GitHub/poverty-priority-queue/images/exp_example.png')
ax.imshow(img, extent=[-1, 1, -1, 1])

# Add the ROIs
for region, (center_x, center_y, size_x, size_y) in regions.items():
    rect = patches.Rectangle((center_x - size_x / 2, center_y - size_y / 2),
                             size_x, size_y,
                             linewidth=1, edgecolor='w', facecolor='white', alpha=0.5)
    ax.add_patch(rect)

# Add the fixation sequences
for fixation_sequence in fixation_sequences:
    x_values = [fixation[0] for fixation in fixation_sequence]
    y_values = [fixation[1] for fixation in fixation_sequence]
    
    # Calculate the center of the fixation sequence (average fixation point)
    center_x = sum(x_values) / len(x_values)
    center_y = sum(y_values) / len(y_values)
    
    # Add the dot at the center of the fixation sequence
    ax.plot(center_x, center_y, 'bo', markersize=8)

# Add lines between consecutive fixation sequences
for i in range(len(fixation_sequences) - 1):
    x_values_i = [fixation[0] for fixation in fixation_sequences[i]]
    y_values_i = [fixation[1] for fixation in fixation_sequences[i]]
    x_values_j = [fixation[0] for fixation in fixation_sequences[i+1]]
    y_values_j = [fixation[1] for fixation in fixation_sequences[i+1]]

    center_x_i = sum(x_values_i) / len(x_values_i)
    center_y_i = sum(y_values_i) / len(y_values_i)
    center_x_j = sum(x_values_j) / len(x_values_j)
    center_y_j = sum(y_values_j) / len(y_values_j)

    ax.plot([center_x_i, center_x_j], [center_y_i, center_y_j], 'r--')

# Mark the first and last fixations
first_fixation = fixation_sequences[0]
last_fixation = fixation_sequences[-1]

x_values_first = [fixation[0] for fixation in first_fixation]
y_values_first = [fixation[1] for fixation in first_fixation]
x_values_last = [fixation[0] for fixation in last_fixation]
y_values_last = [fixation[1] for fixation in last_fixation]

center_x_first = sum(x_values_first) / len(x_values_first)
center_y_first = sum(y_values_first) / len(y_values_first)
center_x_last = sum(x_values_last) / len(x_values_last)
center_y_last = sum(y_values_last) / len(y_values_last)

ax.plot(center_x_first, center_y_first, 'go', markersize=12)  # First fixation in green
ax.plot(center_x_last, center_y_last, 'ro', markersize=12)  # Last fixation in red

plt.xlim([-1, 1])
plt.ylim([-1, 1])
#plt.gca().invert_yaxis()  # Match the image orientation
plt.show()
