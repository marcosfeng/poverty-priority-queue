#### Data Wrangling for PrioQ experiment
### Marcos Gallo

library(readr)
library(stringr)
library(dplyr)
## Loop through individual csv's
setwd("/Users/marcosgallo/Documents/Marcos PhD Files/Poverty/2. Scarcity/3. Data Snapshots/3.2 Data/raw/")
files <- list.files(path="Prolific-pilot1", pattern="*.csv", full.names=TRUE, recursive=FALSE)
pilot_list <- lapply(files, function(x) {
                t <- read_csv(x) # load file
              })

pilot1 <- do.call(rbind,pilot_list)


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
  select(participant, left, right, choice, points, RT)

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


######### Importing STAI + demographics
qualtrics <- read_csv("/Users/marcosgallo/Documents/Marcos PhD Files/Poverty/PrioQ-exp-results/raw/PrioQ - Consolidated_January 13, 2022_18.42.csv")
qualtrics <- qualtrics[-c(1:2),]
vars <- c("Q36", "Q37" , "Q39" , "Q40" , "Q42" , "Q45" , "Q46" , "Q47" , "Q50" , "Q51",
          "Q34" , "Q35" ,"Q38" , "Q41" , "Q43" , "Q44" , "Q48" , "Q49" , "Q52" , "Q53...54", "Q26", "Q27", "Q32")
qualtrics <- qualtrics %>%
  mutate(across(vars, as.numeric))
qualtrics <- qualtrics %>%
  mutate(
    stai = (Q36 + Q37 + Q39 + Q40 + Q42 + Q45 + Q46 + Q47 + Q50 + Q51 +
      5*10 - Q34 - Q35 -Q38 - Q41 - Q43 - Q44 - Q48 - Q49 - Q52 - Q53...54),
    age = 2021 - Q26
  )
qualtrics <- qualtrics %>%
  rename(
    income = Q32,
    education = Q27
  )
library(factoextra)
qualtrics[,c(55:141)] <- sapply(qualtrics[,c(55:141)],as.numeric)
qualtrics.prepca <- qualtrics[,c(55:141)][,(apply(qualtrics[,c(55:141)], 2, var, na.rm=TRUE) != 0) &
                                            !is.na(apply(qualtrics[,c(55:141)], 2, var, na.rm=TRUE))]
qualtrics.prepca <- qualtrics.prepca %>%
  add_column(PROLIFIC_PID = qualtrics$PROLIFIC_PID)
qualtrics.prepca <- na.omit(qualtrics.prepca[!is.na(qualtrics.prepca$PROLIFIC_PID),])
qualtrics.pca <- prcomp(qualtrics.prepca[,-c(48)], center = TRUE, scale = TRUE)
### Scree plot
fviz_eig(qualtrics.pca)
# Results for Variables
qualtrics.var <- get_pca_var(qualtrics.pca)
View(qualtrics.var$coord)  # Loadings
View(qualtrics.var$contrib)
# Results for individuals
qualtrics.ind <- get_pca_ind(qualtrics.pca)
deprivation <- as.data.frame(qualtrics.ind$coord[,c(1:2)])         # Coordinates
deprivation$PROLIFIC_PID <- qualtrics.prepca$PROLIFIC_PID

qualtrics <- qualtrics %>%
  left_join(deprivation, by = "PROLIFIC_PID")

qualtrics <- qualtrics %>%
  select(PROLIFIC_PID, IPAddress, stai, income, education, age, Dim.1, Dim.2)

#join demographics
pilot1 <- merge(y = qualtrics,
                x = pilot1,
                by.y = "PROLIFIC_PID",
                by.x = "participant")

save(pilot1, qualtrics, file = "/Users/marcosgallo/Documents/Marcos PhD Files/Poverty/PrioQ-exp-results/cooked/prolific1.Rdata")
write.csv(pilot1,"/Users/marcosgallo/Documents/Marcos PhD Files/Poverty/PrioQ-exp-results/cooked/prolific1.csv")




