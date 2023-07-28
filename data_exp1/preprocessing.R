# Load the required package
library(tidyverse)

# Load the data
data1 <- read_csv("data_exp1//Icecream 2023 Prolific_July 20, 2023_16.04.csv")
data2 <- read_csv("data_exp1/Icecream 2023 Prolific_July 20, 2023_16.03.csv")
prolific <- read_csv("data_exp1/prolific_export_64a8538371bd4b80e0e7088d.csv")

# Remove the first two rows from both dataframes
data1 <- data1[-c(1,2), ]
data2 <- data2[-c(1,2), ]

# Concatenate the dataframes
united_data <- bind_rows(data1, data2)

# Perform a full join on united_data and prolific
united_data <- full_join(united_data, prolific, by = c("PROLIFIC_PID" = "Participant id"))

# Define the columns to keep
columns_to_keep <- c("Progress", "Duration (in seconds)", "Finished", "RecordedDate", "LocationLatitude", 
                     "LocationLongitude", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", 
                     "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", 
                     "Q25", "Q26", "Q27", "Q28", "Q29", "Q30", "Q31", "Q32", "Q33_1", "Q33_2", "Q33_3", 
                     "Q33_4", "Q33_5", "Q33_6", "Q34", "Q35", "Q36", "Q37", "Q38", "Q39", "Q40", "Q41", 
                     "Q42", "Q43", "attention_check_1", "Q44", "Q45", "Q46_1", "Q46_2", "Q46_3", "Q46_4", 
                     "Q46_5", "Q46_6", "Q46_7", "Q47_1", "Q47_2", "Q47_3", "Q47_4", "Q47_5", "Q47_6", "Q47_7", 
                     "Q47_8", "Q47_9", "Q48_1", "Q48_2", "Q48_3", "Q48_4", "Q48_5", "Q48_6", "Q48_7", "Q49_1", 
                     "Q49_2", "Q49_3", "Q50", "attention_check_2", "Q51", "Q52", "Q53", "Q54", "Q55", "Q56_1", 
                     "Q56_2", "Q56_3", "Q57", "Q58", "Q59", "Q60", "Q61", "Q63", "Q64", "Q65", "Q66", "Q67", 
                     "Q68", "Q69", "Q70", "Q71", "Q72", "Q73", "Q75_First Click", "Q75_Last Click", 
                     "Q75_Page Submit", "Q75_Click Count", "Q77", "Q78", "Q79", "Q80", "Q81", "Q82", "Q83", 
                     "Q84", "Q85", "Q86", "Q87", "Q88", "Q89", "Q90", "Q91", "Q92", "Q93", "Q94", "Q95", 
                     "Q96", "Q97", "Q99", "Q100", "Q101", "Q102", "Q103", "Q104", "Q105", "Q106", "Q107", 
                     "Q108", "Q109", "Q110", "Q111", "Q112", "Q113", "Q114", "Q115", "Q116", "Q117", "Q118", 
                     "PROLIFIC_PID", "Total approvals", "Age", "Sex", "Ethnicity simplified", "Country of birth", 
                     "Student status")

# Keep only the defined columns
united_data <- united_data %>% select(all_of(columns_to_keep))

# Get a list of all unzipped files
files <- list.files("data_exp1/pavlovia/", pattern = "*.csv", full.names = TRUE)

# Initialize an empty list to store the CSV dataframes
csv_dataframes <- list()

# For each CSV file
for (file in files) {
  # Extract the 'PROLIFIC_PID' from the file name
  prolific_pid <- strsplit(basename(file), "_prioq-eyetrack_")[[1]][1]
  
  # Load the CSV file into a dataframe, skip if the file is empty
  df <- tryCatch(read_csv(file), error = function(e) return(NULL))
  
  # Skip to next file if this file is empty
  if (is.null(df)) next
  
  # Store the dataframe in the list using the 'PROLIFIC_PID' as the key
  csv_dataframes[[prolific_pid]] <- df
}

# Create a new column in the 'united_data' dataframe
# Each entry in this column is a dataframe constructed from the respective CSV file
united_data$CSV_Data <- united_data$PROLIFIC_PID %>% map(~csv_dataframes[[.]])

united_data <- united_data %>%
  filter(!is.na(PROLIFIC_PID))

# Function to extract maximum points from the embedded DataFrame
extract_max_points <- function(df) {
  if(!is.null(df) && "points" %in% names(df)) {
    return(max(df$points, na.rm = TRUE))
  } else {
    return(NA)
  }
}

# # Apply the function to each embedded DataFrame in the 'CSV_Data' column
# united_data <- united_data %>%
#   mutate(Max_Points = map_dbl(CSV_Data, extract_max_points))
# 
# # Extract the required columns
# extracted_data <- select(united_data, PROLIFIC_PID, Progress, Max_Points)

# Load necessary libraries
library(dplyr)
library(purrr)
library(stringr)

# Process each CSV data in each row
united_data <- united_data %>%
  mutate(Processed_CSV_Data = map(CSV_Data, ~ {
    df <- .
    
    # Correcting for choice lag
    df$RT <- lead(str_extract(string = df$mouse.time, pattern = "(?<=\\[).+?(?=,)"))
    df$choice <- ""
    df[str_detect(df$mouse.clicked_name, "task1"),]$choice <- "L"
    df[str_detect(df$mouse.clicked_name, "task2"),]$choice <- "R"
    df$choice <- lead(df$choice)
    
    # Delete trials with no action (adding task to queue)
    df <- df[!is.na(df$RT),]
    
    # Keep only variables of interest
    df <- df %>%
      select(participant, left, right, choice, points, RT)
    
    df$i1 <- NA
    df$e1 <- NA
    
    df$i2 <- NA
    df$e2 <- NA
    
    df$u1 <- str_detect(df$left, "U")
    df$u2 <- str_detect(df$right, "U")
    df[str_detect(df$left, "none"), ]$u1 <- NA
    df[str_detect(df$right, "none"), ]$u2 <- NA
    
    df$i1 <- str_detect(df$left, "2.png")
    df$i2 <- str_detect(df$right, "2.png")
    df[str_detect(df$left, "none"), ]$i1 <- NA
    df[str_detect(df$right, "none"), ]$i2 <- NA
    
    df$e1 <- str_detect(df$left, "sp")
    df$e2 <- str_detect(df$right, "sp")
    df[str_detect(df$left, "none"), ]$e1 <- NA
    df[str_detect(df$right, "none"), ]$e2 <- NA
    
    df$u1 <- as.numeric(df$u1)
    df$u2 <- as.numeric(df$u2)
    df$i1 <- as.numeric(df$i1)
    df$i2 <- as.numeric(df$i2)
    df$e1 <- as.numeric(df$e1)
    df$e2 <- as.numeric(df$e2)
    
    df
  }))

