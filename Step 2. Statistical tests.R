## HPRC EBP Participant Outcomes
## Step 2. Statistical tests
## 12/13/2022


# clear work space of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
  
}
rm(res)

# Load pacman into memory, installing as needed
my_repo <- 'http://cran.r-project.org'
if (!require("pacman")) {install.packages("pacman", repos = my_repo)}

# Load the other packages, installing as needed.
pacman::p_load(readr, dplyr, tidyr, lubridate, table1, stringr, ggplot2)

#set working directory 
# setwd("~/Documents/Git/HPRC EBP")

# read in data
df_outcomes <- read_csv("cleaned_data.csv") ## 908 obs. 219 var.  

#3 Create a copy of data for testing
df_test <- df_outcomes

## filter out pre/post folks
df_outcomes <- df_outcomes %>% group_by(record_id) %>% filter(n() == 2) # N = 596

## copy q10 from pre to post surveys
df_outcomes <- df_outcomes %>% group_by(record_id) %>%
  mutate(across(q10, ~coalesce(.x, first(.x))))

table(df_outcomes$pre_post, df_outcomes$q10)

### Loneliness

### Social isolation

### Anxiety

### Depression 

### Physical activity - # of days

### Physical activity - # of minutes

### Social needs

### Self-efficacy

### Pain interference

### Diabetes

### Self-rated health

### Fatigue

### Pain

### Sleep







