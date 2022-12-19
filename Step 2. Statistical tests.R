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
df_outcomes <- read_csv("cleaned_data.csv") ## 875 obs. 219 var.  
df_depression6 <- read_csv("phq9_6_data.csv") ## 275 obs
df_depression10 <- read_csv("phq9_10_data.csv") ## 132 obs

#3 Create a copy of data for testing
df_test <- df_outcomes

## filter out enrollees so we only have completers for tests
df_outcomes <- df_outcomes %>% 
  filter(completers != "Enrollees") # N = 578

df_depression6 <- df_depression6 %>% 
  filter(completers != "Enrollees") # N = 173

df_depression10 <- df_depression10 %>% 
  filter(completers != "Enrollees") # N = 80



### Loneliness
table(df_outcomes$loneliness)

# na.rm=TRUE code excludes missing values
Pre_data <- df_outcomes %>% filter(pre_post == "Pre")

Pre_mean <- mean(Pre_data$loneliness, na.rm=TRUE) # N = 289 obs.

Post_data <- df_outcomes %>% filter(pre_post == "Post") 

Post_mean <- mean(Post_data$loneliness, na.rm=TRUE) # N = 157 obs. 


## Loneliness
loneliness <- t.test(df_outcomes$loneliness[df_outcomes$pre_post=="Pre"],df_outcomes$loneliness[df_outcomes$pre_post=="Post"], paired = TRUE)
loneliness

### Social isolation
social_isolation <- t.test(df_outcomes$soc_isolation[df_outcomes$pre_post=="Pre"],df_outcomes$soc_isolation[df_outcomes$pre_post=="Post"], paired = TRUE)
social_isolation

### Anxiety
anxiety <- t.test(df_outcomes$anxiety[df_outcomes$pre_post=="Pre"],df_outcomes$anxiety[df_outcomes$pre_post=="Post"], paired = TRUE)
anxiety

### Depression 
depression <- t.test(df_outcomes$depression[df_outcomes$pre_post=="Pre"],df_outcomes$depression[df_outcomes$pre_post=="Post"], paired = TRUE)
depression

### Depression 6
table(df_depression6$depression, df_depression6$pre_post)
depression6 <- t.test(df_depression6$depression[df_depression6$pre_post=="Pre"],df_depression6$depression[df_depression6$pre_post=="Post"], paired = TRUE)
depression6

### Depression 10
table(df_depression6$depression, df_depression6$pre_post)
depression10 <- t.test(df_depression10$depression[df_depression10$pre_post=="Pre"],df_depression10$depression[df_depression10$pre_post=="Post"], paired = TRUE)
depression10

# Physical activity
phys_activity <- t.test(df_outcomes$phys_activity[df_outcomes$pre_post=="Pre"],df_outcomes$phys_activity[df_outcomes$pre_post=="Post"], paired = TRUE)
phys_activity

### Physical activity - # of days
Days <- t.test(df_outcomes$pa1[df_outcomes$pre_post=="Pre"],df_outcomes$pa1[df_outcomes$pre_post=="Post"], paired = TRUE)
Days

### Physical activity - # of minutes
Minutes <- t.test(df_outcomes$pa2[df_outcomes$pre_post=="Pre"],df_outcomes$pa2[df_outcomes$pre_post=="Post"], paired = TRUE)
Minutes

### Social needs
soc_needs <- t.test(df_outcomes$social_needs[df_outcomes$pre_post=="Pre"],df_outcomes$social_needs[df_outcomes$pre_post=="Post"], paired = TRUE)
soc_needs

### Self-efficacy
self_efficacy <- t.test(df_outcomes$self_efficacy[df_outcomes$pre_post=="Pre"],df_outcomes$self_efficacy[df_outcomes$pre_post=="Post"], paired = TRUE)
self_efficacy

### Pain interference
pain_interference <- t.test(df_outcomes$pain_interference[df_outcomes$pre_post=="Pre"],df_outcomes$pain_interference[df_outcomes$pre_post=="Post"], paired = TRUE)
pain_interference

### Diabetes
diabetes <- t.test(df_outcomes$diabetes[df_outcomes$pre_post=="Pre"],df_outcomes$diabetes[df_outcomes$pre_post=="Post"], paired = TRUE)
diabetes

### Self-rated health
health <- t.test(df_outcomes$health[df_outcomes$pre_post=="Pre"],df_outcomes$health[df_outcomes$pre_post=="Post"], paired = TRUE)
health

### Fatigue
fatigue <- t.test(df_outcomes$fatigue[df_outcomes$pre_post=="Pre"],df_outcomes$fatigue[df_outcomes$pre_post=="Post"], paired = TRUE)
fatigue

### Pain
pain <- t.test(df_outcomes$pain[df_outcomes$pre_post=="Pre"],df_outcomes$pain[df_outcomes$pre_post=="Post"], paired = TRUE)
pain

### Sleep
sleep <- t.test(df_outcomes$sleep[df_outcomes$pre_post=="Pre"],df_outcomes$sleep[df_outcomes$pre_post=="Post"], paired = TRUE)
sleep







