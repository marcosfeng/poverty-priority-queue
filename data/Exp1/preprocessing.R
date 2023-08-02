# Load the required package
library(tidyverse)

# Load the data
data1 <- read_csv("data/Exp1/Icecream 2023 Prolific_July 20, 2023_16.04.csv")
data2 <- read_csv("data/Exp1/Icecream 2023 Prolific_July 20, 2023_16.03.csv")
prolific <- read_csv("data/Exp1/prolific_export_64a8538371bd4b80e0e7088d.csv")

# Remove the first two rows from both dataframes
data1 <- data1[-c(1,2), ]
data2 <- data2[-c(1,2), ]

# Concatenate the dataframes
united_data <- bind_rows(data1, data2)

# Perform a full join on united_data and prolific
united_data <- full_join(united_data, prolific, by = c("PROLIFIC_PID" = "Participant id"))

# Replace "Prefer not to answer" and "DATA_EXPIRED" with NA
united_data <- united_data %>%
  select(all_of(columns_to_keep)) %>%
  mutate(across(everything(), ~ifelse(. %in% c("Prefer not to answer",
                                               "Prefer not to anwer",
                                               "DATA_EXPIRED"), NA, .)))

united_data$Q25 <- ifelse(as.numeric(gsub("[^0-9]", "", united_data$Q25)) >= 20000,
                          as.numeric(gsub("[^0-9]", "", united_data$Q25)) / 12,
                          as.numeric(gsub("[^0-9]", "", united_data$Q25)))

united_data$Q26 <- ifelse(grepl("week|wk", united_data$Q26, ignore.case = TRUE),
                          as.numeric(gsub("[^0-9]", "", united_data$Q26)) / 5,
                          ifelse(as.numeric(gsub("[^0-9]", "", united_data$Q26)) > 18,
                                 as.numeric(gsub("[^0-9]", "", united_data$Q26)) / 5,
                                 as.numeric(gsub("[^0-9]", "", united_data$Q26))))

united_data$Q27 <- as.numeric(gsub("(?i)\\$|/hour", "", united_data$Q27, perl = TRUE))

united_data$Q28 <- as.numeric(ifelse(united_data$Q28 %in% c("unsure", "n/a", "N/A"), 
                                     NA, 
                                     gsub("(?i)\\$|/hour", "", united_data$Q28, perl = TRUE)))

united_data$Q30 <- as.numeric(gsub("8-|18-", "", united_data$Q30))

united_data$Q31 <- ifelse(grepl("week|wk", united_data$Q31, ignore.case = TRUE),
                          as.numeric(gsub("[^0-9]", "", united_data$Q31)) * 52,
                          ifelse(grepl("month", united_data$Q31, ignore.case = TRUE),
                                 as.numeric(gsub("[^0-9]", "", united_data$Q31)) * 12,
                                 as.numeric(gsub("[^0-9]", "", united_data$Q31))
                          )
)

united_data$Q32 <- ifelse(grepl("week|wk", united_data$Q32, ignore.case = TRUE),
                          as.numeric(gsub("[^0-9]", "", united_data$Q32)),
                          ifelse(grepl("month", united_data$Q32, ignore.case = TRUE),
                                 as.numeric(gsub("[^0-9]", "", united_data$Q32)) / 4,
                                 ifelse(grepl("year|yr", united_data$Q32, ignore.case = TRUE),
                                        as.numeric(gsub("[^0-9]", "", united_data$Q32)) / 48,
                                        as.numeric(united_data$Q32)
                                 )
                          )
)

united_data <- united_data %>%
  mutate(
    Q34 = case_when(
      grepl("\\d+\\s?year[s]?\\s?\\d+\\s?month[s]?", Q34, ignore.case = TRUE) ~ 
        as.numeric(str_extract(Q34, "\\d+(?=\\s?year)")) +
        as.numeric(str_extract(Q34, "(?<=\\s)\\d+(?=\\s?month)")) / 12,
      
      grepl("\\d+/\\d+", Q34) ~ 
        as.numeric(str_extract(Q34, "\\d+(?=/)")) +
        as.numeric(str_extract(Q34, "(?<=/)\\d+")) / 12,
      
      grepl("\\d+\\s?year[s]?", Q34, ignore.case = TRUE) ~ as.numeric(gsub("[^0-9\\.]", "", Q34)),
      
      grepl("\\d+\\s?month[s]?", Q34, ignore.case = TRUE) ~ as.numeric(gsub("[^0-9\\.]", "", Q34)) / 12,
      
      str_detect(Q34, "^\\d{4}$") ~ 2023 - as.numeric(Q34),
      
      TRUE ~ as.numeric(gsub("[^0-9\\.]", "", Q34))
    )
  )


united_data <- united_data %>%
  mutate(
    Q34 = case_when(
      grepl("\\d+\\s?year[s]?", Q34, ignore.case = TRUE) ~ 
        as.numeric(str_extract(Q34, "\\d+(?=\\s?year)")) +
        as.numeric(str_extract(Q34, "(?<=\\s)\\d+(?=\\s?month)")) / 12,
      
      grepl("\\d+\\s?month[s]?", Q34, ignore.case = TRUE) ~ 
        as.numeric(str_extract(Q34, "\\d+(?=\\s?month)")) / 12 +
        as.numeric(str_extract(Q34, "(?<=\\s)\\d+(?=\\s?year)")),
      
      grepl("\\d+/\\d+", Q34) ~ 
        as.numeric(str_extract(Q34, "\\d+(?=/)")) +
        as.numeric(str_extract(Q34, "(?<=/)\\d+")) / 12,
      
      grepl("\\d+\\s?year[s]?", Q34, ignore.case = TRUE) ~ as.numeric(gsub("[^0-9\\.]", "", Q34)),
      
      grepl("\\d+\\s?month[s]?", Q34, ignore.case = TRUE) ~ as.numeric(gsub("[^0-9\\.]", "", Q34)) / 12,
      
      str_detect(Q34, "^\\d{4}$") ~ 2023 - as.numeric(Q34),
      
      TRUE ~ as.numeric(gsub("[^0-9\\.]", "", Q34))
    )
  )

united_data <- united_data %>%
  mutate(across(c("Q3":"Q23", "Q37", "Q49_1":"Q49_3",
                  "Q51", "Q54", "Q56_1":"Q56_3", "Q60"), as.ordered)) %>%
  mutate(across(c("Ethnicity simplified", "Country of birth"), as.factor)) %>%
  mutate(across(c("Q26":"Q32", "Q35", "Q44", "Q45", "Q55",  "Age"), as.numeric)) %>%
  mutate(across(c("Q52", "Q53", "Q55", "Q57":"Q59", "Q61", "Q33_1":"Q33_6",
                  "Q36", "Q46_1":"Q48_7", "Q50", "Student status"),
                function(x) factor(x, levels = c("No", "Yes")))) %>%
  mutate(Sex = factor(Sex, levels = c("Male", "Female"))) %>%
  mutate(attention_check_1 = ifelse(attention_check_1 == "Strongly agree", 1, 0),
         attention_check_2 = ifelse(attention_check_2 == "Sometimes", 1, 0),
         attention = ifelse(attention_check_1 + attention_check_2 == 2, 1,
                            ifelse(attention_check_1 + attention_check_2 == 2, 0, NA))) %>%
  mutate(
    ResidenceValue = case_when(
      !is.na(Q35) & Q35 < 5000 ~ "less than 5,000",
      !is.na(Q35) & Q35 >= 5000 & Q35 < 50000 ~ "more than 5,000 less than 50,000",
      !is.na(Q35) & Q35 >= 50000 & Q35 < 100000 ~ "more than 50,000 less than 100,000",
      !is.na(Q35) & Q35 >= 100000 & Q35 < 150000 ~ "more than 100,000 less than 150,000",
      !is.na(Q35) & Q35 >= 150000 & Q35 < 300000 ~ "more than 150,000 less than 300,000",
      !is.na(Q35) & Q35 >= 300000 ~ "more than 300,000",
      Q40 %in% c("Yes", "Maybe") ~ "more than 300,000",
      Q42 %in% c("Yes", "Maybe") ~ "more than 150,000 less than 300,000",
      Q42 == "No" ~ "more than 100,000 less than 150,000",
      Q41 %in% c("Yes", "Maybe") ~ "more than 50,000 less than 100,000",
      Q43 %in% c("Yes", "Maybe") ~ "more than 5,000 less than 50,000",
      Q43 == "No" ~ "less than 5,000",
      TRUE ~ NA_character_
    ),
    ResidenceValue = ordered(
      ResidenceValue,
      levels = c(
        "less than 5,000",
        "more than 5,000 less than 50,000",
        "more than 50,000 less than 100,000",
        "more than 100,000 less than 150,000",
        "more than 150,000 less than 300,000",
        "more than 300,000")
    )
  ) %>%
  mutate(AnnualSalary = if_else(!is.na(Q25), 
                                Q25 * 12, 
                                if_else(!is.na(Q27), 
                                        Q27 * Q30 * 52, 
                                        Q31)),
         WeeklyHours = if_else(!is.na(Q26), 
                               Q26 * 5, 
                               if_else(!is.na(Q30), 
                                       Q30, 
                                       Q32)))

# Define the columns to keep
columns_to_keep <- c("Progress", "Duration (in seconds)", "Finished", "RecordedDate", "IPAddress", "LocationLatitude", 
                     "LocationLongitude", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", 
                     "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", 
                     "AnnualSalary","WeeklyHours", "Q33_1", "Q33_2", "Q33_3", 
                     "Q33_4", "Q33_5", "Q33_6", "Q34", "Q35", "Q36", "Q37", "Q38", "Q39", "Q40", "Q41", 
                     "Q42", "Q43", "attention_check_1", "Q44", "Q45", "Q46_1", "Q46_2", "Q46_3", "Q46_4", 
                     "Q46_5", "Q46_6", "Q46_7", "Q47_1", "Q47_2", "Q47_3", "Q47_4", "Q47_5", "Q47_6", "Q47_7", 
                     "Q47_8", "Q47_9", "Q48_1", "Q48_2", "Q48_3", "Q48_4", "Q48_5", "Q48_6", "Q48_7", "Q49_1", 
                     "Q49_2", "Q49_3", "Q50", "attention_check_2", "Q51", "Q52", "Q53", "Q54", "Q55", "Q56_1", 
                     "Q56_2", "Q56_3", "Q57", "Q58", "Q59", "Q60", "Q61", "Q77", "Q78", "Q79", "Q80", "Q81",
                     "Q82", "Q83", "Q84", "Q85", "Q86", "Q87", "Q88", "Q89", "Q90", "Q91", "Q92", "Q93", "Q94",
                     "Q95", "Q96", "Q97", "Q99", "Q100", "Q101", "Q102", "Q103", "Q104", "Q105", "Q106", "Q107", 
                     "Q108", "Q109", "Q110", "Q111", "Q112", "Q113", "Q114", "Q115", "Q116", "Q117", "Q118", 
                     "PROLIFIC_PID", "Total approvals", "Age", "Sex", "Ethnicity simplified", "Country of birth", 
                     "Student status")

  
united_data <- united_data %>%
  filter(!is.na(attention)) %>% # Remove observations where both are 0
  select(-Q38)






# Get a list of all unzipped files
files <- list.files("data/Exp1/processed/", pattern = "*.csv", full.names = TRUE)

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
  

# # Function to extract maximum points from the embedded DataFrame
# extract_max_points <- function(df) {
#   if(!is.null(df) && "points" %in% names(df)) {
#     return(max(df$points, na.rm = TRUE))
#   } else {
#     return(NA)
#   }
# }

# # Apply the function to each embedded DataFrame in the 'CSV_Data' column
# united_data <- united_data %>%
#   mutate(Max_Points = map_dbl(CSV_Data, extract_max_points))
# 
# # Extract the required columns
# extracted_data <- select(united_data, PROLIFIC_PID, Progress, Max_Points)

save(united_data, file = "./data/Exp1/df.Rdata")
write_csv(united_data, file = "./data/Exp1/df.csv")








