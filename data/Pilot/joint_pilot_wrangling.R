library(tidyverse)

## Loop through individual csv's
files <- list.files(path = "data/Pilot/Pavlovia", pattern = "*.csv",
                    full.names = TRUE, recursive = FALSE)
# Extract participant ID and time from file names and add them as columns
pilot_list <- lapply(seq_along(files), function(i) {
  t <- read_csv(files[i]) # load file
  t$time <- gsub(".*_(\\d{4}-\\d{2}-\\d{2}_\\d{2}h\\d{2}\\.\\d{2}\\.\\d{3})\\.csv", "\\1", files[i])
  t
})

# Order by participant ID and time
pilot_list <- pilot_list[order(sapply(pilot_list, function(x) x$participant[1]), 
                               sapply(pilot_list, function(x) x$time[1]))]

# Assign block numbers within each participant group
unique_participants <- unique(sapply(pilot_list, function(x) x$participant[1]))
for (participant_ID in unique_participants) {
  block_number <- 1
  for (i in seq_along(pilot_list)) {
    if (pilot_list[[i]]$participant[1] == participant_ID) {
      pilot_list[[i]]$block <- block_number
      block_number <- block_number + 1
    }
  }
}

pilot1 <- do.call(rbind,pilot_list) %>%
  group_by(participant, time) %>%
  filter(n() >= 10) %>% ungroup()

## Correcting for choice lag
pilot1$RT <- lead(str_extract(string = pilot1$mouse.time,
                             pattern = "(?<=\\[).+?(?=,)"))
pilot1$choice <- ""
pilot1[grep("task1", pilot1$mouse.clicked_name),]$choice <- "L"
pilot1[grep("task2", pilot1$mouse.clicked_name),]$choice <- "R"
pilot1$choice <- lead(pilot1$choice)

### Delete trials with no action (adding task to queue)
pilot1 <- pilot1[!is.na(pilot1$RT),]

### Keep only variables of interest
pilot1 <- pilot1 %>%
  rename(round = ic_trials.thisN) %>%
  select(participant, left, right, choice, points, RT, round, block)

pilot1$i1 <- NA
pilot1$e1 <- NA

pilot1$i2 <- NA
pilot1$e2 <- NA

pilot1$u1 <- grepl("U", pilot1$left)
pilot1$u2 <- grepl("U", pilot1$right)
pilot1[grepl("none", pilot1$left), ]$u1 <- NA
pilot1[grepl("none", pilot1$right), ]$u2 <- NA

pilot1$i1 <- grepl("2.png", pilot1$left)
pilot1$i2 <- grepl("2.png", pilot1$right)
pilot1[grepl("none", pilot1$left), ]$i1 <- NA
pilot1[grepl("none", pilot1$right), ]$i2 <- NA

pilot1$e1 <- grepl("sp", pilot1$left)
pilot1$e2 <- grepl("sp", pilot1$right)
pilot1[grepl("none", pilot1$left), ]$e1 <- NA
pilot1[grepl("none", pilot1$right), ]$e2 <- NA

pilot1$u1 <- as.numeric(pilot1$u1) + 1
pilot1$u2 <- as.numeric(pilot1$u2) + 1
pilot1$i1 <- as.numeric(pilot1$i1) + 1
pilot1$i2 <- as.numeric(pilot1$i2) + 1
pilot1$e1 <- as.numeric(pilot1$e1) + 1
pilot1$e2 <- as.numeric(pilot1$e2) + 1

joint_pilot <- pilot1 %>%
  mutate(platform = ifelse(nchar(participant) <= 15, "MTurk", "Prolific"),
         RT = as.numeric(RT))

write_csv(joint_pilot,"data/Pilot/joint_pilot.csv")
